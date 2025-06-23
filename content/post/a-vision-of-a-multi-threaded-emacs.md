+++
title = "A vision of a multi-threaded Emacs"
author = ["Troy Hinckley"]
date = 2022-05-19
tags = ["emacs"]
draft = false
+++

## The Threading library {#the-threading-library}

Starting in Emacs 26 some very ambitious changes were added. Basic thread support was enabled, laying the groundwork for a future concurrent emacs. The [docs](https://www.gnu.org/software/emacs/manual/html_node/elisp/Threads.html) layout this possibility:

> Emacs Lisp provides a limited form of concurrency, called threads. All the threads in a given instance of Emacs share the same memory. Concurrency in Emacs Lisp is “mostly cooperative”, meaning that Emacs will only switch execution between threads at well-defined times. However, the Emacs thread support has been designed in a way to later allow more fine-grained concurrency

What would a future with fine-grained concurrency look like? Could we have an Emacs that uses more then 1 thread? This post sketches out some rough ideas of what that could look like from an elisp perspective. I am going to take the easy way out and completely ignore  _how_ to actually implement this, just speculating on the big what-ifs.


## Concurrency vs parallelism {#concurrency-vs-parallelism}

The `thread` feature is specifically trying to enable _concurrency_, which is the ability to interweave lines of execution to make progress on more then one program at a time. This is the model used by async/await and coroutines. Concurrency is useful when your application is IO bound. The library enables you to switch between concurrent programs at designated points, which is why it is call _cooperative_ concurrency. _Parallelism_ on the other hand is the ability to actually run multiple programs at the same time. This is useful when your application is CPU bound. See [this post](https://oxylabs.io/blog/concurrency-vs-parallelism) for a more detailed explanation.

There are two main scenarios where concurrency/parallelism are useful.

1.  When I want some elisp task done in the background without blocking my main user thread, I can use concurrency. This includes things like handling filter output, indexing buffers, watching for changes, etc. The background task will "steal" idle time from my main thread to make incremental progress on its job.
2.  When I want to get the results of some task faster, I need parallelism. This includes things like updating or searching buffers, applying font lock, or loading code. In order to do these task faster I need multiple threads running at the same time.

Note that parallelism can service both use case 1 and 2, but concurrency can only deal with use case 1. In some sense, parallelism is a superset of concurrency. All parallel code is concurrent, but concurrent code is not necessarily parallel. For this reason, I am much more interested in a parallel Emacs then just a concurrent one.


## What level of parallelism do you want? {#what-level-of-parallelism-do-you-want}

My oversimplification of parallel languages breaks them into three categories:

Level 1 - memory unsafe and data races allowed
: Languages where incorrect code can lead to corruption of the program state and segfaults. This includes C++ and Swift.

Level 2 - memory safe and data races allowed
: Languages where parallelism is memory safe, but can still lead to data races. This includes Java and Go.

Level 3 - memory safe and no data races
: Languages that enable [fearless concurrency](https://doc.rust-lang.org/book/ch16-00-concurrency.html) by eliminating unguarded access to shared-memory. This includes Clojure, Rust, and TCL.

Generally the closer you are Level 1 the more footguns there are, but the more performance you can squeeze out. The higher you go the easier concurrent code is to write, but you have less performance and control. The exception to this is Rust, which is a safe Level 3 language with the performance of a Level 1.

So where do we want Emacs to land on this spectrum? The creator of [nogil](https://github.com/colesbury/nogil) python, a multi-threaded python implementation, [said this](https://docs.google.com/document/d/18CXhDb1ygxg-YXNBJNzfzZsDFosB5e6BfnXLlejd9l0/edit):

> The project aims for a concurrency model that matches the threads + shared memory model implemented in common operating systems... The shared-memory model is notoriously difficult to program correctly, but provides a good base to build better abstractions because it closely matches the primitives provided by the operating system (e.g. threads, mutexes).

The argument is that Level 2 is the right balance, because you avoid crazy bugs you get with unsafe languages but still have more flexibility then level 3. You should just give programmers the tools they need to build safe abstractions.

I, however, disagree with that take. As the author said, shared memory is "notoriously difficult" to do correctly. As Emacs pulls in hundreds of packages, the potential for data races grows exponentially. Even Emacs' current threads library [suffers from data races](https://nullprogram.com/blog/2018/05/31/) which is one of the reasons I believe it has not seen much adoption. We need to make concurrency as pain free as possible if it is going to be usable. Therefore I am in the "Emacs parallelism should be level 3" boat.


### What are some requirements for parallel Emacs {#what-are-some-requirements-for-parallel-emacs}

So what are the standards we want for a multi-threaded Emacs implementation? Here is my short list:

1.  No [function coloring](https://journal.stuffwithstuff.com/2015/02/01/what-color-is-your-function/) or special requirements on functions. One of Emacs' big advantages is the huge bulk of existing lisp code. We want to reuse as much as we can. This is generally only a problem with concurrent schemes like async/wait.
2.  No data races. This will make programs significantly easier to write correctly, but is also going to make our code more limited (\*\* _foreshadowing \*\*_).
3.  We want the behavior of multi-threaded code to be as close to single-threaded code as possible. More on this later.


## A jumping off point {#a-jumping-off-point}

Most people have never heard of TCL (or if they have they've never used it) but I find it has a [very simple approach](https://www.activestate.com/blog/threads-done-right-tcl/) to multi-threading. Essentially the interpreter can work in its own thread, and carries with it all of its state. This is the multi-interpreter approach; Every thread starts in a clean environment with its own interpreter. "Messages" can be passed to any thread and they can return a result. In elisp it could look something like this.

```emacs-lisp
(let ((thread1 (make-thread))
      (thread2 (make-thread))
      (filename "file.txt")
      result1 result2)
  ;; Send commands to the threads
  (thread-send thread1 #'setup-function)
  (setq result1 (thread-send thread1 #'my-function))
  ;; closure captures a copy of the variable
  (setq result2 (thread-send thread2 (lambda () (delete-file filename))))
  ;; run some other code while threads are working
  (my-other-function)
  ;; wait for the results to populate
  (thread-wait result1 result2)
  ;; return the results in list
  (list result1 result2))
```

This is really simple and really effective, but it has some limitations. First is that since each thread starts a new interpreter, you need to load a bunch of lisp code to do almost anything useful. This means that thread overhead is significant and is not a good fit for small tasks. Second, since you are copying objects between threads when you pass them with `thread-send` you can't modifying existing buffers[^fn:1]. And buffers are probably the most important use case here. Let's see if we can fix that.


## Can you pass the buffer please? {#can-you-pass-the-buffer-please}

What if instead of needing to copy the buffers between threads, they could be shared? I know, I know, shared-memory is a footgun, but we are going to use mutexes! So it's more like sharing in pre-school where everyone gets a turn. Each buffer is guarded by a mutex, and only one thread can have access to a buffer at a given time. The way you acquire the mutex is by switching to that buffer (using `set-buffer` , `switch-to-buffer`, or `with-current-buffer`). Just as you can only have a single "current buffer", you can only have the mutex for a single buffer at a time. A thread can switch to a buffer, do some operations, then release it. This is all well and good, but we have a major issue; shared-state.

You see, for a buffer to really be useful you need have the buffer local variables. Without those you can't even know the `major-mode`! But buffer local variable can share data with global variables, and each thread has its own set of globals. Consider the code below:

```emacs-lisp
(defvar local nil)
(defvar global nil)
(make-variable-buffer-local 'local)
(setq local '(1 2 3))
(setq global local)
```

Here both `local` and `global` share the same cons cells. If I mutate one it will mutate the other. This obviously won't work if I am sharing buffer local variables between threads. What we need to sever the ties between these. Buffer local variables can't share any data with non-buffer local variables. You could setup a write barrier that would make copies when a thread releases the mutex. But I am not going to get into the _how_ (I get to ignore implementation details remember?).

This would technically make multi-threaded emacs have different behavior then the old single threaded one (I told you we would talk about that later). But I would argue that if you are relying on sharing data between globals and buffer-locals for the correct operation of your code, it is in serious need of a refactor. I imagine that in real life this situation is very rare.


## Cutting down initialization {#cutting-down-initialization}

As it currently stands you basically need to load your entire init file in each new thread. Why can't you just load the bare minimum elisp? Consider what would happen when a buffer local hook calls some function from another package? We need to make sure the code is loaded, and the only way we can do that is by loading the init file which contains all package initialization. There is also the problem that the state can get out sync. Each thread would start in a clean state, but this is not going to match the state of your main thread. This impedance mismatch is a clear source of bugs.

What if we did something crazy? What if we said that all functions are shared between threads? If you think about it, this is almost a perfect match. Function rarely change, and when they do, you can just replace the whole function atomically. However to do this you would need to address the _functional literal mutation problem_ I talk about [here](https://coredumped.dev/2021/04/07/when-pure-function-lie/). Otherwise you could have multiple threads mutating the same function constants and potentially corrupting the VM state. But again, _implementation details_.

Okay so that takes care of functions, but what about variables? Sharing variables would lead to data races, which is exactly what we are trying avoid. What if instead of sharing, we copied variables on demand? Hear me out! The first time a "sub-thread" does a lookup of a global variable, its value is copied from the main thread. This sets the initial value in that thread. From then on that copy of the value is "owned" by the thread and it can be mutated or read whenever. This would also help with the out-of-sync with the main thread problem we mentioned earlier. The state in your sub-thread would be much closer to the main global state. There could be a message queue internal to the VM that sends these requests back and forth. At certain points, the main thread would check the queue and send the values.

But this also means that sub-threads could spend a decent amount of time waiting for the main thread to be ready to send the values it needs, at least at the start. There could be a couple ways to alleviate this. The one that comes to mind is that you could cache the list of variables used at the call site of the thread creation. Then next time the thread is called you eagerly copy all the variables it had used before. This would make repeated initialization of the same thread much faster, but could also mean you get different variables the first time vs following times (if the calling function changed something immediately afterwards for example). As with everything; trade-offs.

There is one big reason for all this song and dance around buffers locals, functions, and variables: reusing existing elisp code. Languages that were created with concurrency in mind have designed their languages around these considerations. But Emacs is a giant ball of mutable state. Taming that to do something useful in a multi-threaded world while still reusing existing code is tricky.


## Going Green {#going-green}

So our threads are pretty cheap to create (as threads go), but not to keep around. Each elisp thread maps to an OS thread, and even when thread is idle it is still taking OS resources. Go and Clojure have solved this by creating so called green threads that are managed by the runtime. The green threads will be executed on OS threads, but they can be created, destroyed and managed by the VM. In Go you can create thousands of green threads and it will not impact the system. Don't try at home with OS threads.

Now that we have green threads, the observant among you will notice that we have basically reinvented goroutines. All we need to do is add channels and we can have something close to [core.async](https://clojuredocs.org/clojure.core.async). I imagine usage looking something like this:

```emacs-lisp
;; Delete 3 files concurrently
(let ((files '("file1.txt" "file2.txt" "file3.txt")))
  (dolist (file files)
    (go (delete-file file))))

;; loop through all buffers and insert "a" for some reason...
(let* ((c (chan)))
  (go (loop-chan chan buffer
                 (with-current-buffer buffer
                   (insert "a"))))
  (dolist (buffer (buffer-list))
    (chan-put c buffer)))

;; run a buffer search on another thread and yield the output
(let ((queries (chan))
      (results (chan)))
  (go (let* ((message (channel-take queries))
             (buffer (car message))
             (string (cdr message)))
        (chan-put results
                  (with-current-buffer buffer
                    (save-excursion
                      (re-search-forward string)
                      (buffer-substring
                       (point)
                       (+ 5 (point))))))))
  (chan-put queries (cons other-buffer "foo"))
  (do-something-else)
  ;; wait till the routine returns
  (setq my-string (chan-take results)))
```


## So where does that leave us? {#so-where-does-that-leave-us}

We have created some simple light weight green threads for Emacs. Well, we didn't actually create anything, but we sure did talk about it a lot! The thing I like about this approach is that threads are easy to create and use to accomplish work in parallel. There are no data races and footguns have been minimized. But I can still see a few open problems that are not solved:

1.  The most important buffer in Emacs is the one you are currently editing. But with the mutex scheme, you can't use any other threads to work on that buffer! If you want to index or search or syntax-highlight the buffer, that still needs to use your main thread, meaning that the user is blocked. I don't like it, but am not sure of a clean way to fix it.
2.  What about when you need to iterate over all buffers (like with `ibuffer`)? Here you would need to acquire the mutex for each one in turn. If another thread is using a buffer the main thread will have to wait. Hopefully sub-threads would choose to do their work incrementally, giving time for the thread to yield the mutex.
3.  Since the sub-threads take their global variables from the main thread, you can't load code in parallel. Only the main thread can load code that can be used by everyone.
4.  I haven't even mentioned a bunch of other multi-threading concerns like cancellation, atomics, garbage collection, message queue buffering, semaphores, signals, error reporting, debugging, concurrent data structures, C integration, deadlock, and livelock to name a few. Perhaps those are a topic for a future post.

It has been fun to speculate about multi-threaded Emacs. But the real question is, would it be worth it? Emacs has gotten along just fine with a single thread; In fact many (most?) dynamic languages have. I would guess that threads would only be useful in about 10% of the programming tasks you would do in elisp. But when threads can be used, they would be big boon.

As with everything in engineering, concurrency comes with trade-offs. Implementing a scheme like I described would be a monumental task. It would probably involve a complete rewrite of the core runtime[^fn:2]. Also anytime you make an interpreter multi-threaded, you make single threaded code slower[^fn:3]. There is no avoiding that. If 90% of code is still single-threaded, is that worth the cost?

Anyways, I would love to get some feedback on the ideas presented. Are there obvious holes that I missed? Would this scheme be useful? Do you know way that these could be implemented (or would not be possible to implement)? How does this compare to other dynamic languages? Do you prefer the more "thread-like" or "green-thread-like" approach? Is there a way to address some of the problems presented above?


### Have a comment? {#have-a-comment}

Join the discussion on [HN](https://hn.algolia.com/?dateRange=all&page=0&prefix=false&query=https%3A%2F%2Fcoredumped.dev%2F2022%2F05%2F19%2Fa-vision-of-a-multi-threaded-emacs%2F&sort=byPopularity&type=story), [Lobsters](https://lobste.rs/search?q=https%3A%2F%2Fcoredumped.dev%2F2022%2F05%2F19%2Fa-vision-of-a-multi-threaded-emacs%2F&what=stories&order=newest), [Reddit](https://www.reddit.com/search/?q=url%3Acoredumped.dev%2F2022%2F05%2F19%2Fa-vision-of-a-multi-threaded-emacs%2F&sort=top), or send me an [email](mailto:troy.hinckley@dabrev.com)

[^fn:1]: And you can't share anything that doesn't have a clear [readable](https://www.gnu.org/software/emacs/manual/html_node/elisp/Streams-Intro.html) representation such as windows, frames, or makers.
[^fn:2]: Similar to what is being attempted for python [here](https://github.com/colesbury/nogil) and [here](https://github.com/larryhastings/gilectoy).
[^fn:3]: Why does adding multi-threading make single-threaded code slower? It essentially comes down the fact that synchronizing memory between cores is [slow](https://spcl.inf.ethz.ch/Publications/.pdf/atomic-bench.pdf). So unless your interpreters are completely independent (which would not be very useful), you are going to add some overhead. Now this doesn't have to be a lot, it can be just a couple percentage points. But it will always be something. The [nogil design doc](https://docs.google.com/document/d/18CXhDb1ygxg-YXNBJNzfzZsDFosB5e6BfnXLlejd9l0/edit) goes into great detail about some of performance implications (for python) and how to mitigate them. See also this [design doc](https://github.com/ocamllabs/compiler-hacking/wiki/OCaml-Multicore-Runtime) for multi-core OCaml or [this presentation](https://www.atdot.net/~ko1/activities/2016_rubykaigi.pdf) on the Actor model in Ruby.
