+++
title = "When pure function lie"
author = ["Troy Hinckley"]
date = 2021-04-07
tags = ["design", "emacs"]
draft = false
+++

Here is a simple question. Given the lisp function below (and that the function is not advised) what will the output be of `(foo)`?

```emacs-lisp
(defun foo ()
  "foo")
```

Seems pretty simple right? How could the answer be anything other then `"foo"`? It is just returning a constant string. Or is it? The real answer to this question is...

We have no idea. It can could be any string of length 3.

How can this be? Well it turns out that constant literals in lisp (common lisp, emacs lisp, etc) are not constant. They can be modified at runtime like any other variable. Here is an elisp example that does just that.

```emacs-lisp
(defun foo ()
  "foo")

(foo) => "foo"

(defun bar ()
  (let ((x (foo)))
    (aset x 0 ?福)
    x))

(bar) => "福oo"
(foo) => "福oo"
```

Look at that! We have modified the behavior of another function using only its return value. We might have considered `foo` a [pure function](https://en.wikipedia.org/wiki/Pure_function). However this cannot be the case because we can return different values for the same input.

This is true whether it's dynamic or lexically bound, interpreted or byte compiled. And there is no way to get the original literal value back without redefining the function. Which means that once you try and instrument the function to see what is going on, the problem magically goes away! Here is another example where we are able to change a different constant literal in the same function. Since `foo` is byte-compiled, the two constants are mapped to the same mutable object. So changing one changes the other!

```emacs-lisp
(defvar global)

(byte-compile
 (defun foo (x)
   (setq global "foo")
   (concat "foo" x)))

(foo "bar") => "foobar"

(aset global 2 ?x)

(foo "bar") => "foxbar"
```


## Why does this happen? {#why-does-this-happen}

Strings in lisp are mutable, and when you pass a string to something it actually passes a reference to it. But once you have a reference to a string, you can modify it however you want. Which means that you are modifying the original string that was part of the constant list of the function!

As far as I could find, this behavior is unique to lisp (and it works with literal lists and vectors as well). Other dynamic languages don't have this property. For example, the python code below does not change the original list[^fn:1].

```python
def foo():
    return [1, 2, 3]

def bar():
    x = foo()
    x[0] = 0
    return x

print(foo()) => [1, 2, 3]
print(bar()) => [0, 2, 3]
print(foo()) => [1, 2, 3]
```

But the elisp version will

```emacs-lisp
(defun foo ()
  '[1 2 3])

(defun bar ()
  (let ((x (foo)))
    (aset x 0 0)
    x))

(foo) => [1 2 3]
(bar) => [0 2 3]
(foo) => [0 2 3]
```

I would venture to say that anytime you overwrite a constant literal it almost certainly a bug. And a very hard to debug one at that! So why does lisp allow this? I imagine a large part of it is simplifying the implementation. You don't have to check if the value you are modifying is constant or not, you just mutate it. Another part of it is that Common Lisp was conceived in a time when string where just vectors of ascii characters (Similar to C). That made modifying them like modifying normal arrays. But now with the advent of unicode, changing a "character" of string is not so easy. It can involve shifting the entire string or even reallocating depending on the size of the code point. This is why most modern languages have immutable strings by default.

This is another reason that it is hard to make a true multi-threaded elisp. You can't share function between threads when "normal" code can change the behavior of functions being used in different threads. Functions that look pure might be changing under the hood. In this situation you are cross-pollinating your code with mutable data. I should add that this really has nothing to do with [homoiconicity](https://en.wikipedia.org/wiki/Homoiconicity). You could still have a fully homoiconic language without the ability to overwrite constant literals.

When I first saw this behavior, I thought for sure that I found a bug in the language implementation. This couldn't possibly be intentional. But after investigating more, I found that this was expected... at least if you are lisp programmer.


### Have a comment? {#have-a-comment}

Join the [discussion](https://discu.eu/?q=https%3A%2F%2Fcoredumped.dev%2F2021%2F04%2F07%2Fwhen-pure-function-lie%2F&submit_title=When%20pure%20function%20lie%20%E2%80%A2%20Core%20Dumped) or send me an [email](mailto:troy.hinckley@dabrev.com)

[^fn:1]: As several people [pointed out](https://www.reddit.com/r/emacs/comments/mm70re/when_pure_functions_lie/gtq1oir?utm_source=share&utm_medium=web2x&context=3), the reason this works in python is because it is making a copy of the list every time it returns. You could introduce the same issue in python using default arguments, which are not copied. To protect against this in lisp, you can use `(vector 1 2 3)` instead of `'(1 2 3)` and it will make a copy of the vector.