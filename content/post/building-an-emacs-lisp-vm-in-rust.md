+++
title = "Building an Emacs lisp VM in Rust"
author = ["Troy Hinckley"]
date = 2021-10-21
tags = ["emacs", "rust"]
draft = false
+++

About a year ago I was bitten by the PL bug. It started with reading [Crafting Interpreters](http://craftinginterpreters.com/) and discovering the wonders hidden under the hood of a compiler. I am also been a big fan of Emacs, and this started to get me interested in how it's interpreter works. At the same time, I was reading the Rust book and trying to understand the concepts there. This all came to a head, and I decided to write an Emacs Lisp interpreter called [rune](https://github.com/CeleritasCelery/rune) in Rust.

My goal for this project is to bootstrap [bytecomp.el](https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/bytecomp.el) and use the Emacs compiler with my bytecode vm using only stable Rust. I have not reached that goal yet, but I have bootstrapped several core Emacs lisp files. At this point I have a enough of an interpreter that I want to share an update and mention some of the trade-offs and other things I have learned.


## Overview {#overview}


### Tree walk or bytecode? {#tree-walk-or-bytecode}

Emacs has 3 seperate execution engines: a tree walk interpreter, a Bytecode VM, and ([recently!](https://git.savannah.gnu.org/gitweb/?p=emacs.git;a=commit;h=289000eee729689b0cf362a21baa40ac7f9506f6)) native compile. They all provide their own sets of trade-offs, but that also mean that any new feature needs to be implemented up to 3 times. I didn't want the duplicate work, so I opted to only have a byte code VM and no interpreter. This turned out to be harder than I initially thought. All the early elisp files assume that you are using an interpreter. Macros are often used before they are defined because the interpreter has lazy-macro expansion. This is harder for a byte-compiler because you want to expand the macro's at compile time instead of run time. I ended up needing to make some tweaks to the ordering and structure of the [lisp files](https://github.com/CeleritasCelery/rune/tree/master/lisp) to support a bytecode-only bootstrap.


### Object representation {#object-representation}

A critical part of any dynamic language is how to represent types at runtime. Since you will frequently be boxing and unboxing values, you want these to be both time and space efficient.

Rust provides a strong candidate in its enums, but you are limited to the representations that they provide. Most of the time this isn't a problem. However, because of the language specification that pointers are a full word, you can’t normally use optimizations like [NaN-boxing](https://piotrduperas.com/posts/nan-boxing) or [pointer tagging](https://en.wikipedia.org/wiki/Tagged_pointer) in Rust. Therefore, when I was initially defining object type as a `union`.

```rust
union Object<'ob> {
    tag: Tag,
    data: i64,
    marker: PhantomData<&'ob ()>,
}
```

Then in the boxing and unboxing code, I could check the type of the tag field and reinterpret the bits as whatever type was needed. This had the advantage of being extremely flexible (since I had complete control over the bit layout and representation) but it also had some drawbacks compared to a proper Rust enum.

1.  Unboxing requires `unsafe` code
2.  Must manually match the tag to the right data type. There are no compiler checks here.
3.  No way to `match` directly on the union. Need to create an accessor functions to get the underlying value as an enum.
4.  Can't use variants as values. With an Enum you can use `Option::None` or `Option::Some(T)` as values, but instead you have to create constants to represent common values.
5.  No debugger support. A union is completely opaque to the Rust debugger.
6.  No destructuring support. Since Rust's pattern syntax does not understand my type, there is no way to do the following

<!--listend-->

```rust
match object {
    Some(Object::Nil | Object::Int(_)) => ...,
    Some(Object::String(s)) => ...,
    _ => ...
}
```

Given all this, I decided to try and see if I could make an enum that still had the space optimization of a tagged pointer (word width). This is done with a `Data` type, which is 56 bits wide.

```rust
#[derive(Copy, Clone)]
pub struct Data<T> {
    data: [u8; 7],
    marker: PhantomData<T>,
}
```

When this is used in an enum the result is a 64 bit object (The same size as a tagged pointer on 64-bit systems).&nbsp;[^fn:1]

```rust
pub enum Object<'ob> {
    Int(Fixnum),
    Float(Data<&'ob f64>),
    Symbol(Data<Symbol>),
    True,
    Nil,
    Cons(Data<&'ob Cons<'ob>>),
    Vec(Data<&'ob Vec<Object<'ob>>>),
    String(Data<&'ob String>),
    LispFn(Data<&'ob LispFn<'ob>>),
    SubrFn(Data<&'ob SubrFn>),
}
```

These `Data` structures have a `inner` method that transforms the 56 bit payload into a full width pointer. This is done by shifting the pointer so that the bottom byte can be used for the discriminant.

```rust
impl<T> Data<T> {
    const fn expand(self) -> i64 {
        let data = self.data;
        let whole = [
            0, data[0], data[1], data[2], data[3], data[4], data[5], data[6],
        ];
        i64::from_le_bytes(whole) >> 8
    }
}

impl<'a, T> Inner<&'a T> for Data<&'a T> {
    fn inner(self) -> &'a T {
        let bits = self.expand();
        let ptr = bits as *const T;
        unsafe { &*ptr }
    }
}
```

Rust optimizes this into a simple 8 bit shift for boxing and unboxing. The Data type can also hold additional information like mutability.

Overall I am very happy with this approach. But is has limitations of it's own.

One thing that would make this approach much simpler is if we could use the `Deref` trait to unbox `Data<T>`. But this only works some of the time. Since deref takes `&self`, it ties the lifetime of the borrow of `self` to the lifetime of the unboxed data.

```rust
impl<'a, T> Deref for Data<&'a T> {
    type Target = T;
    fn deref(&'_ self) -> &'a Self::Target {
        self.inner()
    }
}
```

This makes it much less useful. I wish Rust had a trait like `DerefCopy` that transformed `self` to `T` instead of transforming `&self` to `&T` and still offered [deref coercion](https://doc.rust-lang.org/std/ops/trait.Deref.html#more-on-deref-coercion).

```rust
impl<'a, T> DerefCopy for Data<&'a T> {
    type Target = &'a T;
    fn deref(self) -> &Self::Target {
        self.inner()
    }
}
```

Currently I have repurposed the `Not` operator as my own deref function. It doesn’t offer any of the magic of deref coercion, but it is more concise then calling `inner()` every time you need to unbox something (which is very frequent). And since it takes `self` and not `&self`, it doesn't run into the lifetime issues deref does. So when I need to unbox something I can use `!data` to achieve this.

```rust
impl<T> Not for Data<T> where Data<T>: Inner<T> {
    type Output = T;
    fn not(self) -> Self::Output {
        self.inner()
    }
}
```

Another problem with enums is that they leave the unused portion of the allocation uninitialized&nbsp;[^fn:2]. This can allow optimizations when the kind is known ahead of time and is smaller than the full enum size. However this also prevents other optimizations from being used.

For example, Emacs provides several different version of the `equal` function based on how strict they are. The simplest is `eq`, which checks if two variables point to the same object. This specializes to a single word size comparison (1 instruction), because two objects that point to the same object will have the same bit representation. Unfortunately this optimization is not possible with Rust enums, because part of the enum can be uninitialized, and it is UB to read uninitialized memory. Therefore implementing `eq` is much more expensive with a Rust enum then it is with a union. This one of the disadvantages of not have control over the layout of your type.

To work around this, I have tried implementing the fieldless variants of my enum as having a field of type `Data<()>` which is always zero. This is slightly less ergonomic, but give back the power to compare objects directly by their bit representation.


### Defining functions {#defining-functions}

I can’t take credit for this, as the idea came from [remacs](https://github.com/remacs/remacs/tree/master/rust_src/remacs-macros) (the original Emacs in Rust project), but it really showcases the power of Rust procedural macros. The `defun` macro is applied to any normal Rust function and it then becomes callable from lisp.

```rust
#[defun(name = "-")]
pub(crate) fn sub(number: Option<Number>, numbers: &[Number]) -> NumberValue {
    match number {
        Some(num) => {
            let num = num.val();
            if numbers.is_empty() {
                -num
            } else {
                numbers.iter().fold(num, |acc, x| acc - x.val())
            }
        }
        None => Int(0),
    }
}
```

This macro creates a wrapper around the function that transforms lisp objects into Rust types and back, handling any type errors along the way. The type signature of the Rust function also gets converted to the type signature in lisp; `Option` types become `&optional` and slices become `&rest`. For example the function signature above will become `(- &optional NUMBER &rest NUMBERS)`.This makes it easy to use the function in Rust or lisp, and the syntax is much cleaner then the [DEFUN](https://github.com/emacs-mirror/emacs/blob/3cabf64131b93e1b0510a01bcce8efde38b20bc4/src/lisp.h#L3050) macro used in the the Emacs C code.


## Interesting learnings along the way {#interesting-learnings-along-the-way}


### Generics in Rust {#generics-in-rust}

Generics are a really powerful feature that let you build reusable data structures and help eliminate some boilerplate. Less code means less bugs. Generics are particular useful in conjunction with traits, letting you implement them for a range of types. However, I found that in practice generics were less useful than they could have been due to the lack of [specialization](https://rust-lang.github.io/rfcs/1210-impl-specialization.html). This absence means that anytime you need to specialize for one type you completely lose the use of generics for that function/trait&nbsp;[^fn:3]. Because of this I ended up implementing many of the traits with macros instead of generics. If specialization is ever stabilized, it will remove hundreds of lines of boilerplate from the code base. But it looks like that is still a [ways off](https://aturon.github.io/blog/2017/07/08/lifetime-dispatch/).


### Garbage collection {#garbage-collection}

I have not currently implemented a garbage collector for my interpreter, though it doesn't leak memory. This works because all objects are “owned” by an arena, and all the lifetimes of objects are tied to the borrow of the that arena. So when the arena goes out of scope, so do all the objects it owns. This works fine for bootstrapping, but there is no freeing of unused memory. I have done a lot of reading on garbage collectors and they are considered very tricky to get right. As Bob Nystrom [said](http://craftinginterpreters.com/garbage-collection.html#garbage-collection-bugs), “garbage collector bugs really are some of the grossest invertebrates out there.”

Rust has some unique offerings that promise to not only make garbage collectors easier to implement, but safer to use as well. I am not going to go into detail here because you can find a great overview of different approaches in this [series of blog posts](https://manishearth.github.io/blog/2021/04/05/a-tour-of-safe-tracing-gc-designs-in-rust/).

The most interesting crates to me are ones that use Rust's borrow checker to ensure that it is safe to run the collector. All objects have a lifetime tied to a `Context` object. Anytime the Garbage collector runs it takes a `&mut self`, ensuring all objects it created can't be accessed afterwards. In order to keep objects alive you need to root them. This is either done with stack or linked list. Some examples of this approach are [joesphine](https://github.com/asajeffrey/josephine) and [shifgrethor](https://github.com/withoutboats/shifgrethor).

Another similar approach is the concept of [generativity](https://raw.githubusercontent.com/Gankro/thesis/master/thesis.pdf), which is essentially using a unique lifetime to brand objects so they cannot be unified with other arenas. The [gc-arena](https://crates.io/crates/gc-arena) and [cell-gc ](https://crates.io/crates/cell-gc)are example of this. One thing is for sure, these libraries will become much easier to use if Rust ever gets the ability to track stack roots[^fn:4]. Until that time there is still an wide space to be explored.

The last thing that makes garbage collectors difficult in Rust is the that the [allocator API](https://github.com/rust-lang/wg-allocators) is still unstable, and probably won't be stabilized anytime soon. Some gc algorithm's rely on particular layouts of data to work correctly. Currently you need to either use the changing API in nightly or implement things yourself with pointers.


### Fixing lifetime issues {#fixing-lifetime-issues}

When I first created lisp objects, they were unions with raw boxed pointers. After all, this is what you would have in C. However, after facing several memory errors, I decided to take advantage of Rust lifetime system and add lifetimes to all objects. They now hold a `PhantomData` of a reference. When I first made this change it lead to a lot of pain. I learned it is very valuable to really step back and actually think about borrow checker messages. Oftentimes rather then fighting the borrow checker, you are better off restructuring your code to make it more lifetime friendly. Once I did a major refactor where data flows from main to the rest of the program most of my lifetimes issues just disappeared. Another thing to keep in mind is that just because Rust _allows_ your lifetimes doesn't make them _correct_. All that rustc cares about is that your lifetimes are not memory unsafe; it doesn’t care if they are [correct](https://github.com/pretzelhammer/rust-blog/blob/master/posts/common-rust-lifetime-misconceptions.md/#5-if-it-compiles-then-my-lifetime-annotations-are-correct). It is up to you as the developer to make sure your lifetimes are right. Most often what I needed to do to correct my lifetimes was to split them up. Forcing Rust to unify unrelated lifetimes is guaranteed to cause more pain then needed.


### Globals vs multi-threaded {#globals-vs-multi-threaded}

I was initially inspired to do this project by _Crafting Interpreters_ and reading the Emacs internals. Both of these programs make heavy use of globals to store and manipulate state, which is very common in C. However Rust takes a different stand. In Rust there is no such thing as single threaded code. Even code that does not rely on any concurrency constructs is expected to work without issues in a multi-threaded environment. This means all globals must be wrapped in a concurrency safe type.

However, I was still convinced that I wanted to do things the "C" way. It made following my templates (Lox and Emacs) much easier. Accessing a raw global is cheap; Accessing it though a mutex is not. I "knew" that my interpreter was not multi-threaded and I did not want to pay that overhead. However, finding out how to implement raw globals was no easy task. It took some digging, but I did discover that you can implement C-style zero-cost globals in Rust with some unsafe code. Not too long after I implemented that I began to run into random test failures. I found much to my surprise that even the test runner in Rust is multi-threaded! At this point, I broke down and decided to get on board with the Rust approach to concurrency. I moved all globals to the stack or put them behind a mutex. It wasn’t as bad as I feared.


#### The seeds of parallelism {#the-seeds-of-parallelism}

As part of the move to a concurrency safe runtime, I started thinking about what it would take to have a true multi-threaded Emacs lisp. To experiment with this, I set it up so that all functions are shared between threads with atomics; But values are thread local. This brought up some interesting challenges that Emacs lisp presents concurrency, all related to mutability and aliasing.

For one, [function literals are mutable in lisp](https://coredumped.dev/2021/04/07/when-pure-function-lie/). This means you can change a function by mutating it’s return value. If functions are shared between threads, then they can't be mutable; Otherwise you expose yourself to dataraces. In Common Lisp they just say "yolo!" and make mutating a function literal undefined behavior. However, you can’t easily tell when you are doing this; It can often be very far from the call site.

Another issue is that aliasing is very common in elisp. This generally isn’t an issue in single-threaded code, but becomes a source of very difficult bugs in a multi-threaded world. You need to either make all objects concurrency safe (which is very expensive) or prevent threads from mutably aliasing each other’s data. This is one of the areas where Rust really shines, but would require a lot of hard trade-offs in lisp.

For example, concurrency in Emacs would not be very useful without the ability to share buffers. If you share buffers, you also need a way to share buffer local variables; and buffer local variables can share data (cons cells, strings, and vectors) with other local variables. There is no way to share a buffer with another thread without also sharing your entire environment. At some point I plan to write more about potential multi-threading in Emacs, but that will have to be saved for a future post.


## Rust as a language backend {#rust-as-a-language-backend}

Overall, I have come to love Rust! It makes systems programming feel accessible. And the community is absolutely awesome&nbsp;[^fn:5]. I've never had a question that I was not able to get help with. That being said, implementing an interpreter for a dynamic language in Rust is particularly challenging because the host language does not[^fn:6] follow Rust's rules around mutability and aliasing. To solve this you need to either do runtime accounting using `Rc<RefCell<T>>` (which is expensive and leaks cycles), or deal with upholding all of Rust invariants manually in unsafe code. Neither is a very attractive proposition.

Speaking of unsafe, you often hear that writing unsafe code is "just like writing C". That is not really true. Rust has more invariants that need to be upheld then does C, especially related to mutability, aliasing, traits, layout, initialization, and dropping. All these invariants need to be considered when writing unsafe code and can lead to [very tricky unsound behavior](https://www.youtube.com/playlist?list=PLqbS7AVVErFj1t4kWrS5vfvmHpJ0bIPio). Many of these are either not a concern, or much less of a concern, in correct C code.

Rust also lacks a feature of C that is used to implement fast interpreter loops; [computed goto](https://eli.thegreenplace.net/2012/07/12/computed-goto-for-efficient-dispatch-tables). This feature can be used to implement [direct threading](https://en.wikipedia.org/wiki/Threaded_code#Direct_threading) without the need for assembly code, giving a sizable [performance](http://www.cs.toronto.edu/~matz/dissertation/matzDissertation-latex2html/node6.html) increase on some processors[^fn:7]. Rust may support this in the future, but given the complex interactions this would have with the borrow checker, I doubt it. I could see future where fast Rust interpreters write their inner dispatch loop in C just to take advantage of this feature.

Now, none of this is to say that Rust is poor language for writing a dynamic language backend. On the contrary, it offers some features like sum types, unnullable pointers, and safety from concurrent data races that are really powerful.  However, some of Rust's core strengths in aliasing and mutability apply less well to this domain then they do to others.


## Conclusion {#conclusion}

I really gained an appreciation for the depth of the Emacs internals. That code has been around for a long time and is very mature; but at the same time, it is also under active development. Trying to implement Emacs from scratch would mean not only matching the current well-tested functionality, but also trying to keep up with the constantly changing internals. While Emacs may not be the most elegant C code base, it is certainly robust.

As for how long I plan to continue this project, I don't really know. At very least I am going to bootstrap the Emacs lisp compiler to test it against my runtime and implement a garbage collector. My expectation is either that I will learn enough about text-editors and interpreters to be able to contribute to Emacs proper, or I will find a problem in the Rust ecosystem that does not have a good solution and focus on that instead. Or I may continue to see how far I can push this project. Either way, contributions and testing are welcome. Please take a look at the [code](https://github.com/CeleritasCelery/rune) and give feedback. I am particularly interested in anything that could be unsound or lead to undefined behavior. This has been a great experience and I am learning more than I could have hoped.


### Have a comment? {#have-a-comment}

View the discussion on [Reddit](https://www.reddit.com/r/emacs/comments/qcus3f/building_an_emacs_lisp_virtual_machine_in_rust/?utm_source=share&utm_medium=web2x&context=3), [Hacker News](https://news.ycombinator.com/item?id=29038140), or send me an [email](mailto:troy.hinckley@dabrev.com)

[^fn:1]: As an added bonus converting between objects can be a no-op with the [arbitrary_enum_discriminant](https://github.com/rust-lang/rust/issues/60553) feature that was set to make it into 1.56. Unfortunately this was [recently reverted](https://github.com/rust-lang/rust/pull/89884).
[^fn:2]: For an example of how subtle UB can happen with enums see [this crossbeam issue](https://github.com/crossbeam-rs/crossbeam/issues/748).
[^fn:3]: This [issue](https://github.com/rust-lang/rust/issues/50133) shows how a seemingly innocent blanket implementation in the core can break a bunch of generics for all users due to no specialization.
[^fn:4]: LLVM has support for this, but is has not been moved into Rust yet.
[^fn:5]: That is, so long as you don't use a trigger phrase like "unsafe code" or "this works fine in C".
[^fn:6]: Some functional languages do have invariants around immutability, but they often use mutability under the hood.
[^fn:7]: I saw some claims that using threaded dispatch in CPython brought a 10-20% improvement, but I didn't see benchmarks.
