+++
title = "Design of Emacs in Rust"
author = ["Troy Hinckley"]
date = 2023-01-17
draft = false
+++

This is the third post in my series about writing an Emacs core in Rust. The [first post](https://coredumped.dev/2021/10/21/building-an-emacs-lisp-vm-in-rust/) laid out my initial observations and ideas about the language runtime. The [second post](https://coredumped.dev/2022/04/11/implementing-a-safe-garbage-collector-in-rust/) focused on building a safe garbage collector in rust using the type system. I initially stated that I wanted to reach the point where I could bootstrap bytecomp.el (the elisp byte compiler). That goal is reached[^fn:1], so I am providing an update on my latest learnings.


## Interpreter {#interpreter}

Emacs has multiple runtime environments including the interpreter, bytecompiler, and native code. My initial goal was to not implement more then one runtime, so I determined to only have a bytecode VM. I created a bootstrap compiler to get [bytecomp.el](https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/bytecomp.el) enabled and then planned to use the elisp compiler for the rest of the code. Despite some good first steps I continually ran into problems with the bootstrap process[^fn:2]. I would hack the elisp or the load order to try and work around the problem, but eventually it became a game of whack-a-mole. I spent most of my time trying to workaround issues instead of actually writing code.

I finally broke down and implemented an elisp interpreter. I agree with Stefan Monnier that the the elisp interpreter [is a crutch to bootstrap the system](https://lists.gnu.org/archive/html/emacs-devel/2021-03/msg00068.html), but the startup code is written in a way that you can't bootstrap without the crutch[^fn:3]. Now I have both an interpreter and bytecode. I learned that I was a little too ambitious in my desire to get rid of the interpreter since it is much easier to debug than bytecode is.


## Symbol Layout {#symbol-layout}

I learned some interesting details about how Emacs represents symbols in objects. The symbol itself is a [struct](https://github.com/emacs-mirror/emacs/blob/master/src/lisp.h#L833-L887) that hold a bunch of fields like function, value, or properties. You would assume that a `LispObject` that contains a symbol would be a pointer to that struct, and indeed it was for a long time.

The issue with using a pointer comes from codegen. On most architectures immediate values larger than a certain size (usually 16 bits) need to be moved into a register. Pointers are generally larger than that, so they can't be embedded directly in the instruction encoding. Instead they have to be loaded with a separate instruction. Total code size will also be increase because there need to be pointer sized constants all over the code.

To address this, the Emacs maintainers implemented a clever scheme where symbols are not encoded as pointers; they are offsets from a static symbol array called `lispsym`. This means the first object in the array has offset 0, the next one has offset `sizeof(LispSymbol)`, and so forth. When you want to get the pointer, you add the [start address of lispsym to the offset](https://github.com/emacs-mirror/emacs/blob/master/src/lisp.h#L1142-L1144). The symbol at index 0 of `lispsym` is the symbol `nil`, which is used more than any other[^fn:4]. As reported in the Emacs [changelog](https://github.com/emacs-mirror/emacs/blob/master/src/ChangeLog.13#L1653-L1662), this approach reduced the code size by 2.5% and led to a minor speedup.


### Implementation in Rust {#implementation-in-rust}

One of the really nice things about this scheme is that it maps well to Rust. The language has a limitation that constants cannot refer to statics. The [reasons for this](https://github.com/rust-lang/const-eval/issues/11) are complex, and I hope this will be changed someday. It's very useful for symbols to be constants because then they can be used in match patterns and const functions. My first attempt working around this limitation was a terrible hack involving self-referential functions and other terrible ideas. Once I learned the Emacs approach, things became super clean and simple:

```rust
pub(crate) struct Symbol<'a> {
    // Offset from the start of the symbol table
    data: *const SymbolCell,
    marker: PhantomData<&'a SymbolCell>,
}

// Can be used in match statements ...
match obj {
    Object::Symbol(NIL) => {...},
    ...
}

// Or even simpler with associated constants
match obj {
    Object::NIL => {...},
    ...
}
```


## Representing Strings {#representing-strings}

[UTF-8](https://en.wikipedia.org/wiki/UTF-8#Encoding) has become the de facto standard for representing text. Emacs closely follows the unicode standard, but uses an extended version of UTF-8 which enables support for raw bytes.  Let me explain.

One of the reasons that UTF-8 is so useful is because [ASCII characters](https://www.asciitable.com/) are automatically valid. These are the values between 0 and 127 and includes the English alphabet. If you assigned a code point to every value of the byte you could only have 256 possible characters. Instead, bigger code points are encoded using multiple bytes. The values above 127 are reserved for leading bytes in UTF-8. Thus a random value above the ASCII range may not be valid. However Emacs [extends unicode](https://www.gnu.org/software/emacs/manual/html_node/elisp/Text-Representations.html) to reserve the code points [0x3FFF80 to 0x3FFFFF](https://github.com/emacs-mirror/emacs/blob/master/src/character.h#L31-L47) as "raw bytes between 128 and 255".

The advantage of this is that Emacs can distinguish a "normal" byte that just happens to be valid UTF-8 from a "raw byte" that is not intended to be valid. However the display representation can be a confused with unprintable characters. For example, if you see this printed representation in the buffer:

`\201`

it can either be the unicode codepoint `0x81` (Emacs displays things in octal) or the raw byte `0x81` represented by codepoint `0x3FFF81`. The only way to tell the difference is to inspect the character.

There are other use-cases for a "mostly UTF-8 but not quite" type of formats. For example, [WTF-8](https://simonsapin.github.io/wtf-8/#generalized-utf-8) is used to handle invalid UTF-16 conversions to UTF-8. The downside of these formats is that you loose compliance with the spec, which means you can't use third-party string libraries that operate on code points. The Remacs team [had to rewrite the primitive string type](https://github.com/remacs/remacs/blob/master/rust_src/src/multibyte.rs#L13-L33) in their project to support raw bytes.

I am taking the [bstr](https://docs.rs/bstr/latest/bstr/) approach for my project. That assumes that strings are conventionally UTF-8, but will handle invalid bytes gracefully. Raw bytes are no longer distinguishable from other bytes, but I see that as an acceptable trade-off to use existing libraries.


## Finding GC Roots {#finding-gc-roots}

One of the trickiest parts of implementing a garbage collector is handling roots. You need to ensure that any value that is reachable from the program stack or machine register is not garbage collected. In the days of yore, Emacs had a method called GCPRO to handle this. As the [SXEmacs docs say](http://www.sxemacs.org/docs/internals/GCPROing.html#GCPROing), "GCPROing is one of the ugliest and trickiest parts of Emacs internals".

In order to use GCPRO,  there were a bunch of rules provided to avoid memory issues including:

1.  For every GCPROn, there have to be declarations of struct gcpro gcpro1, gcpro2, etc.
2.  You must UNGCPRO anything that’s GCPROed
3.  You must not UNGCPRO if you haven’t GCPROed
4.  Make sure not to use a relocated string. They are not GCPROed
5.  If you have to nest GCPRO’s, use NGCPROn
6.  Don't GCPRO uninitialized memory
7.  If you create any Lisp objects, you are responsible for GCPROing them
8.  Make sure that traps can't occur between allocating memory and GCPRO

The docs state that bugs resulting from not following these rules are "intermittent and extremely difficult to track down, often showing up in crashes inside of garbage-collect or in weirdly corrupted objects or even in incorrect values in a totally different section of code".

It's no wonder that the maintainers decided to abandon this approach and instead use conservative stack scanning, (where you treat everything that looks like a pointer on the stack as pointer). This is what the Spidermonkey team [had to say](https://blog.mozilla.org/javascript/2013/07/18/clawing-our-way-back-to-precision/) about switching in Firefox:

> Language implementations with automatic memory management often start out using exact rooting. At some point, all the careful rooting code gets to be so painful to maintain that a conservative scanner is introduced. The embedding API gets dramatically simpler, everyone cheers and breathes a sigh of relief, and work moves on (at a quicker pace!)

However this comes with a tradeoff, you also loose the ability to precisely know what is really a pointer. This may not seem like a big deal, but it limits the kind of collectors [you can implement](https://lists.gnu.org/archive/html/emacs-devel/2015-09/msg00695.html) (such as a copying GC). The same post by the Spidermonkey team mentions their effort to "claw their way back to precision". They needed the performance improvements that can come with precise memory management techniques. Despite that, given all the complexity added to Emacs by GCPRO I think removing it was the right call.

Rust gives us a different option. It's powerful type system and affine types let us have both precision and a bug-free implementation. I wrote a whole post describing how you to [implement a safe GC in rust](https://coredumped.dev/2022/04/11/implementing-a-safe-garbage-collector-in-rust/),  so I won't expand on that here. Suffice it to say that the borrow checker can ensure that all stack roots are accounted for.


## Making Emacs Multi-Threaded {#making-emacs-multi-threaded}

I mentioned in my first post in this series how Rust does not have a concept of single threaded applications. Every program is considered multi-threaded, even if only used with a single thread. This changes how you design programs and data structures, with the biggest difference being no unguarded global mutable state. I implemented my VM to support multi-threading, opening the possibility for the elisp itself to take advantage of that. I [wrote a post](https://coredumped.dev/2022/05/19/a-vision-of-a-multi-threaded-emacs/) about my ideas for implementing a multi-core Emacs. That approach is what I am using in this Rust runtime. In fact, the basic support is already there! Right now you can write code like this:

```emacs-lisp
(go (lambda () (do-something-in-thread)))
```

And it will execute the code in another thread! The other thread has access to all the functions defined in the runtime. It won't be too much work to add support for channels and variables to send data back and forth.


## Future Design Work {#future-design-work}

I hit my initial goal of bootstrapping the elisp bytecompiler. My new goal is to finish bootstrapping all elisp included with GNU Emacs and bytecompile it. While working on my bytecompiler objective I tried very hard to not get sucked into tangents. However, there are several things that are in desperate need of attention. For one, the [garbage collector](https://github.com/CeleritasCelery/rune/blob/master/src/core/gc/context.rs) is the most basic mark-and-sweep imaginable. I am going to bring it up to snuff and implement a [generational copying collector](https://github.com/CeleritasCelery/rune/issues/20).

There is also a big need for testing. My code has unit tests, but the true spec is not my tests - it's Emacs itself. I plan to write a test harness that will let me fuzz my implementation against GNU Emacs. This will hopefully help me flush out a bunch of issues instead of hitting them during development. The bigger the core gets, the more important this will be.

This project is still far from complete or useful. It's truly a love letter to Emacs and has been an amazing learning experience. There have been a lot of design challenges trying to do things in a memory-safe and multi-threaded way. I created a [design doc](https://github.com/CeleritasCelery/rune/blob/master/details.org) that contains a bunch of loosely structured thoughts on ways different things could be implemented. I also created a bunch of [issues](https://github.com/CeleritasCelery/rune/issues?q=is%3Aopen+is%3Aissue+label%3A%22design+needed%22) in the issue tracker with the label: **Design Needed**. This is where I have put some ideas about how to handle things from [multi-threading](https://github.com/CeleritasCelery/rune/issues/21) and [string representation](https://github.com/CeleritasCelery/rune/issues/14) to [regex](https://github.com/CeleritasCelery/rune/issues/19) and [buffer data structures](https://github.com/CeleritasCelery/rune/issues/17). If you have an eye for design or just want to add your thoughts, go ahead and submit a comment or new issue.


### Have a comment? {#have-a-comment}

Join the [discussion](https://discu.eu/?q=https%3A%2F%2Fcoredumped.dev%2F2023%2F01%2F17%2Fdesign-of-emacs-in-rust%2F%23fnref%3A3&submit_title=Design%20of%20Emacs%20in%20Rust%20%E2%80%A2%20Core%20Dumped) or send me an [email](mailto:troy.hinckley@dabrev.com)

[^fn:1]: This would not have been possible without Rocky Bernstein's amazing resource; [Bytecode Reference Manual](http://rocky.github.io/elisp-bytecode.pdf).
[^fn:2]: Most of the boostrap issues were functions being called before their macros were defined. Or it would be variables that were not defined before functions were evaluated. This is fine for an interpreter because the macros and variables would be code paths that were not used, so the macro was never evaluated. But a bytecompiler will expand all macros in a function, so it would generate a function call instead of a macro expansion.
[^fn:3]: Too be fair, bootstrapping is a really hard problem. You would need something to bootstrap a bytecompiler written in the language you are bytecompiling. My approach was to create a minimal compiler in rust, but you could also take the approach of using bytecode written in a previous build of Emacs. [This post by the Zig team](https://ziglang.org/news/goodbye-cpp/#the-solution-space) covers some of the many ways you can bootstrap a language.
[^fn:4]: Another advantage of `nil` being the symbol with offset 0 is that you can now test for nil by comparing with zero, which usually has it's own dedicated instruction.
