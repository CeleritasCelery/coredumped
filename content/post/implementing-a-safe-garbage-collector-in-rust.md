+++
title = "Implementing a safe garbage collector in Rust"
author = ["Troy Hinckley"]
date = 2022-04-11
tags = ["rust"]
draft = false
+++

In my [last post](https://coredumped.dev/2021/10/21/building-an-emacs-lisp-vm-in-rust/) I introduced an Emacs Lisp VM I was [writing in Rust](https://github.com/CeleritasCelery/rune). My stated goal at the time was to complete a garbage collector. I think Rust has some really interesting properties that will make building garbage collectors easier and safer. Many of the techniques used in my GC are not original and have been developed by other Rustaceans in previous projects.


## Why use garbage collection? {#why-use-garbage-collection}

virtually all non-trivial programs need some way to reuse memory. Rust does this by tracking every allocation statically to determine when it's no longer in use. However, this system is not flexible enough for some applications. In these cases Rust gives you [Rc](https://doc.rust-lang.org/std/rc/struct.Rc.html), the reference counting cell. This cell tracks the number of owners of a piece of memory at runtime. Reference counting has the advantage that is relatively easy to implement and integrates seamlessly with non-rc code. However, it also has two big downsides: It's slow[^fn:1] and it can't detect cyclic data (which lisp is full of). For these reasons (and others) dynamic languages often use garbage collection (GC) to manage data.


## Why is GC hard? {#why-is-gc-hard}

In his book _crafting interpreters_ Robert Nystrom has a whole [section](http://craftinginterpreters.com/garbage-collection.html#garbage-collection-bugs) dedicated to some of the "nasty bugs" you can have in a garbage collector. The problem lies in identifying all objects that are accessible in the heap. Once you have an object you know is live, it's fairly easy to trace through anything it points to and find other live data. But how do you find the pointers that don't have anything pointing to them? These pointers are problematically scattered across the stack or stored in machine registers. If you miss even one you open yourself up to memory unsafety.


### How does Emacs solve this problem? {#how-does-emacs-solve-this-problem}

Emacs (and many C based GC implementations) solves this by recognizing that the stack is just a block of memory[^fn:2]. If an object can't be reach from the stack, it can't be reached at all. So when garbage collection is triggered, they will dump all registers to the stack and scan the it for anything that "looks like" a pointer. I say looks like because we can't _actually_ know if something is pointer or a number in range of a pointer. There is no type information in the hardware. So anything that might be a pointer is treated as a pointer and traced. However because we aren't sure, we can't move any of the gc data. In my implementation we are building a "precise" collector that knows exactly what's a pointer and what's not. That rules out blind stack scanning.


### Let's start from the beginning {#let-s-start-from-the-beginning}

When we allocate a new object, we know that it is unaliased (nothing has a pointer to it). But as soon as we give that pointer to user code, it becomes exposed. Problem is, we need to know when it safe to call `drop` and release the memory.  In C, it is up to the user to call `free` when they are done with it. But Rust tracks the liveness of the data with the type system. The Rust rule is this: There can be many immutable references to an object or one mutable reference (but not both). If you have immutable references, they get invalidated as soon as a mutable reference is used.


### Affine types {#affine-types}

This key property of Rust (called affine types) is what is used in the gc library [Jospehine](https://docs.rs/josephine/latest/josephine/). They use Rust's borrow checker to ensure no references are live after collection. We do the same. All pointers into the GC heap borrowed from our allocator (called `Arena`) via a immutable reference. When we call `garbage_collect`, we take a `&mut Arena`, ensuring that all heap references are no longer accessible.

```rust
let arena: &'ob Arena = ...;
let object: Object<'ob> = arena.add("foo");
use_object(object);
arena.garbage_collect(); // Object is no longer accessible
```

However, If we  invalidate all references to the GC heap when we call `garbage_collect`, we can't access our data at all afterwards. We obviously need something more here.


## Rooting {#rooting}

What we really want is to have some pointers _preserved_ across calls to `garbage_collect`. But we need to make sure the gc knows about these special pointers, or it will free the data they point to. We call these special pointers roots.

We have a similar problem with normal data structures. We need to get a reference to a value after we mutate something. How do we solve this problem in that case? Take the example below:

```rust
let mut map = HashMap::new();
let key = "my key";
map.insert(key, 13); // insert at key
let value1 = map.get(key).unwrap(); // get a reference to our item
let _ = &mut map; // take a mutable reference, invalidating our value1
let value2 = map.get(key).unwrap(); // Use key to get our data back again
```

Here we are storing our data inside the map and using some unique token (the key) to keep track of our value inside the data structure when we loose access to our reference. We can do the same thing with our gc `Arena`. We store the roots inside before we garbage collect.

```rust
struct Arena {
    roots: HashMap<Token, Object>,
    ...
}
```

However we have at least two problems with this:

First, what do we use for a token? Everytime we need to root something we need a token that is unique. Even if we generated something random there is still a chance we could have two roots with the same `Token`, which would lead to memory unsafety.

Which leads to the second problem: once something is no longer rooted, how do we remove it from the `Arena`? We could require the user to manually call `remove` when they no longer need a root, but any failure to do so would result in leaking memory. That is not a good API.


### Standing on the shoulders of boats {#standing-on-the-shoulders-of-boats}

Thankfully, I am not the first person to think about this problem. Saoirse has a [blog post](https://without.boats/blog/shifgrethor-iii/) about some novel observations on rooting in Rust. This was implemented in his gc library [shifgrethor](https://github.com/withoutboats/shifgrethor). I will summarize this approach below.

The first observation is that if you don't drop or move a type on the stack, then its lifetime is perfectly stack-like. Shocking I know, but the really cool part about this is that we can use it to define the way we store the roots in `Arena`. What if instead of storing them as a map, we store them as a stack instead? When something is rooted, it is pushed on the stack. When it drops, it is popped from the stack. This also solves our problem of creating a unique `Token` to find our object, because when we drop we know that our item will always be the top of the root stack. So no `Token` needed.

In order for this to work we have to make sure the object can't move. This sounds just like the pinning! We define a new macro `root!` that works similarly to [pin_mut!](https://docs.rs/pin-utils/0.1.0/pin_utils/macro.pin_mut.html). This ensure our objects behaves in a stack-like manner, greatly simplifying the implementation.

As far as keeping our references around post garbage collection, we know that so long as our object is rooted it will be valid. We can keep a reference into the Gc heap until we unroot (i.e. the root goes out of scope). Our `root!` macro will change our reference from borrowing from `Arena` to a borrowing from the root. So long as the root is live, our reference is valid; Even if we garbage collect.

```rust
let object = arena.add("new");
// add the object to the root stack, enabling it to live past collection
root!(object, arena);
arena.garbage_collect(); // gc will mark the object as live
println!("{object}"); // Object is still accessible
```


### Returning from functions {#returning-from-functions}

There is one more ergonomic problem we want to solve here. Suppose we have the function below:

```rust
fn foo<'a>(arena: &'a mut Arena) -> Object<'a> {
    ...
    arena.add(5);
}
```

The function takes a `&mut Arena`, and at the end it returns an `Object` with the same lifetime. [Seems fine](https://github.com/pretzelhammer/rust-blog/blob/master/posts/common-rust-lifetime-misconceptions.md#9-downgrading-mut-refs-to-shared-refs-is-safe) right? Not so. The Rust lifetime rules [require](https://doc.rust-lang.org/nomicon/lifetime-mismatch.html) that the _mutable borrow_ of `Arena` now lasts for the lifetime `'a`! This means we can't reuse `Arena` while the `Object` is borrowed from it. We could just `root!` the object, but that adds overhead to every call. In my interpreter, nearly every method takes `&mut Arena`, so that would get expensive fast.

To work around this we create a new macro `rebind!`

```rust
rebind!(object, arena);
```

This macro releases the _mutable_ borrow and reborrows the object with an _immutable_ borrow. This frees `Arena` to be used by other code while still being [sound](https://github.com/CeleritasCelery/rune/issues/2).


### Preventing escapes {#preventing-escapes}

This approach works fine for rooting a single object, but what if we have a whole collection of objects? You might be tempted to think that would be an non-issue, but consider the problem below:

```rust
let rooted: Vec<Object<'root>> = ...; // we have a vec of root objects
let object: Object<'arena> = arena.add("new"); // object is bound to arena
rooted.push(object); // object is now bound to rooted
arena.garbage_collect(); // Object is marked as live and not freed

// Object is no longer rooted, but still bound to the root lifetime
let escape: Object<'root> = rooted.pop().unwrap();
arena.garbage_collect(); // Object is freed
println!("{escape}"); // Oh No! Use after Free!
```

We cannot move references out without some way of making sure they stay rooted. Thankfully shifgrethor comes to the rescue here again with it's `Gc` type.

Once again we can model after the `pin` API, since we are trying to solve a similar problem. If you have a `Pin<P>` you know that the data point to by `P` will not move. Similarly, we create a `Root<T>` type that guarantees `T` will not move **and** it's rooted. We use the `struct_root!` macro to take a data structure `T` and returns a `&mut Root<T>` to it.

```rust
let arean: Arena = ...;
struct_root!(my_struct, arena);
let _: &mut Root<Vec<Object>> = my_struct;
// get a reference to vec from root
let len = my_struct.len();
// use a special function to mutate
my_struct.root_push(object);
// use projection
let slice: &[Root<Object>] = my_struct.as_slice();
// Object with lifetime bound to root
let object: Object<'_> = slice[0].as_obj();
```

With this API, we can safely get a `&T` out when we need to. But mutating the `T` requires unsafe methods (like [map_unchecked_mut](https://doc.rust-lang.org/std/pin/struct.Pin.html#method.map_unchecked_mut)) to ensure we don't expose roots as in the example above. Using a similar approach to [pin projection](https://doc.rust-lang.org/std/pin/index.html#projections-and-structural-pinning) you can get a `Root` to the fields of rooted struct. For example if you have a `&Root<(T, U)>` it is safe get a `&Root<T>` or `&Root<U>`. For some the std lib types (vec, hashmap, option, etc) I have already implemented some safe mutation methods like `push`. If you have a structure that is just built out of these stdlib data structures, you could use a proc macro to derive the "root projection" methods for it.


## The problem with aliasing {#the-problem-with-aliasing}

There is still one subtle problem here. You see, we now have a `&mut Root<T>`, and when we garbage collect, we will trace through `T` with `&T`. However `&mut T` guarantees that that it is unique. To break this invariant means undefined behavior. Shifgrethor does not handle this, instead requiring that all roots be immutable (even forbidding interior mutability). Ugh.

How about we use `UnsafeCell`? It is full of dark magic that lets us do thing we couldn't normally do.

_\* reads documentation \*_

> Note that only the immutability guarantee for shared references is affected by UnsafeCell. The uniqueness guarantee for mutable references is unaffected. There is no legal way to obtain aliasing &amp;mut, not even with UnsafeCell&lt;T&gt;.

Oh, biscuits. What other options do we have? I am sure Rust has an `AliasCell` that let's us work around this, right?

_\* googles frantically \*_

Nope. Though apparently we not the only ones who need this. The std lib created a [hack](https://github.com/rust-lang/rust/pull/82834) to avoid miscompilations with aliasing mutable references that is used in [Tokio](https://github.com/tokio-rs/tokio/pull/3654) as well. We could take that route (and I was really tempted to) but let's see if there is another way we could fix this.


### Can we approach the problem from the other side? {#can-we-approach-the-problem-from-the-other-side}

Actually there is a way `UnsafeCell` can help us here. There is one legal way in which you can have aliasing `&mut T`. By design, a `&mut T` can alias with a `&UnsafeCell<T>`, (but not the other way around). So long as we don't have any `&T` at the same time; this is sound. But this of course means interior mutability. We could try just putting `RefCell`'s everywhere, but that means we are going have to debug runtime panics instead of compile time errors. We _really_ don't want that.


### Qcell to the rescue {#qcell-to-the-rescue}

[Qcell](https://docs.rs/qcell/latest/qcell/index.html) is a crate trying to design a compile time `RefCell`. It makes a bunch of different cell types, each with their own set of trade off, that give you exactly that. We are going to use [LCell](https://docs.rs/qcell/latest/qcell/struct.LCell.html)[^fn:3], which is zero cost and perfect for our use case. With this type, multiple cells have a shared owner that control when a cell can be borrowed mutable or immutable. To make this safe we define the following conditions:

1.  We can borrow a `Root` as immutable if we have a `&RootOwner`.
2.  We can borrow a `Root` as mutable if we have a `&mut RootOwner` **and** we have a `&Arena`. This ensures that we can never call garbage collect while our mutable reference is live, because garbage collect requires a mutable borrow of `Arena`!

<!--listend-->

```rust
impl<'id, T> Root<'id, T> {
    pub(crate) fn borrow<'a>(
        &'a self,
        owner: &'a RootOwner<'id>
    ) -> &'a RootRef<T> {...}

    pub(crate) fn borrow_mut<'a>(
        &'a self,
        owner: &'a mut RootOwner<'id>,
        _: &'a Arena
    ) -> &'a mut RootRef<T> {...}
}
```

Using compile-time interior mutability makes our code more verbose then it needs to be, but that is the price we pay sometimes for correctness. I would love for Rust to get an `AliasCell` that solves this problem for everyone. Either way, this is what the final API looks like:

```rust
let root_owner: RootOwner<'id> = ...;
let arean: Arena = ...;
struct_root!(my_struct, arena);
let _: &Root<'id, Vec<Object>> = my_struct;
// immutable borrow
let len = my_struct.borrow(&root_owner).len();
// mutable borrow
my_struct.borrow_mut(&mut root_owner, &arena).push(object);
```


## A Safe GC {#a-safe-gc}

So there you have it! A safe, precise, garbage collector in stable Rust! Now, this comes with a few caveats. It is often said that solving a general problem is three times harder then solving a specific problem. I am solving the specific problem here; creating a GC for my VM. This not ready to ship as a general purpose library without more work. But I am confident it could be made into a library if needed. Right now the garbage collector is about as naive as possible. It traces things recursively (which could stackoverflow on large heaps) and can't handle cycles. But these are all pretty simple under-the-hood improvements that don't change the API.

What I think is really cool is that the API is **safe**! You can't create this in `C`  or `C++`; The type system is not powerful enough. Rust enables us to have "fearless garbage collection", and no longer be scared of the "nasty bugs" that we might create. As an anecdote, I was pleasantly surprised to find that when I turned on reclaiming memory in my gc, everything just worked first time; No memory leaks, no use-after-free. The API just took care of it at compile time. Miri was satisfied as well.

Overall, I am pretty happy with how it turned out. That being said, there is **a lot** of unsafe code behind the scenes. I am the only person that has reviewed it, and I am not smart enough to get everything right. I created a [unsound?](https://github.com/CeleritasCelery/rune/labels/unsound%3F) Label on Github that tracks some of the code I have the least confidence in. If you are initiated in the dark arts of the [nomicon](https://doc.rust-lang.org/nomicon/), I would love for you to [prove me wrong](https://github.com/CeleritasCelery/rune/issues?q=is%3Aissue+label%3Aunsound%3F+).

I am going to continue work on bootstrapping more elisp files to eventually bootstrap the elisp byte compiler and use that to test my VM. I was forced to take break from that effort and implement the garbage collector because I kept using too much memory! Implementing the garbage collector was a much bigger rabbit hole than I expected. Hopefully this will help move the community forward on the quest for a Rust GC.


### Have a comment? {#have-a-comment}

send me an [email](mailto:troy.hinckley@dabrev.com) or open an [issue](https://github.com/CeleritasCelery/rune/issues/new).

[^fn:1]: Why is reference counting slow? There is a lot that goes into it, but it boils down to three main issues:

    1.  Reference counting turns all reads (which are cheap) into writes (which are more expensive). This is because we need to update the count on every read.
    2.  RC can fall victim to "destructor avalanche" when the root of a chain of objects goes out of scope.  This results in unbounded pause times. Modern GC's by contrast are usually incremental, and will do work in small chunks to preventing long pauses.
    3.  The counting overhead is added to every _access_, compared to GC where most accesses are free (unless there is a read/write barrier). You can also move the overhead of GC tracing to a time when it will have less impact (or even to another thread), but RC overhead needs to happen real time.

    With all of these issues, there are techniques to try and mitigate them and get some performance back. But even a naive GC can often beat a well optimized RC implementation. And optimized GC (like JVM or V8) will always outclass reference counting. See [this SO post](https://softwareengineering.stackexchange.com/questions/30254/why-garbage-collection-if-smart-pointers-are-there) and [follow up post](https://web.archive.org/web/20200325094430/http://flyingfrogblog.blogspot.com/2010/12/why-gc-when-you-have-reference-counted.html) for more.
[^fn:2]: I don't think this is true in Rust though. My best guess is that scanning the stack would violate some of rust's aliasing rules and be UB.
[^fn:3]: Actually I am using a fork of [LCell](https://github.com/CeleritasCelery/rune/blob/master/src/arena/cell.rs) that uses the [generativity](https://crates.io/crates/generativity) crate to avoid the requirement of putting everything in closures. But the principle is the same.
