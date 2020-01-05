+++
title = "Native shell completion in Emacs"
author = ["Troy Hinckley"]
date = 2020-01-04
categories = ["emacs"]
draft = false
+++

I am obsessed with autocompletion in shell mode. Running a shell in `shell-mode`  
instead of a terminal emulator has so many advantages. You can treat the whole  
buffer just like a normal Emacs buffer. You can copy and paste and edit the line  
normally. You can hook it into native Emacs functionality. You can even [display  
images!](https://github.com/riscy/shx-for-emacs)  

However there is one big disadvantage. You loose access to the state the shell.  
This means that you have to do tricks like `shell-dirtrack-mode` just to make sure  
you are in the right directory. It also means that all the native shell  
completions are not available. I have tried to approach this problem from  
multiple angles with packages like [this](https://github.com/CeleritasCelery/company-fish), [this](https://github.com/CeleritasCelery/company-async-files), and [this](https://github.com/CeleritasCelery/company-arguments). (Yes, I have written a  
half dozen modes to try solve this.) The most popular package to try and solve  
this is [bash-shell-completion](https://github.com/szermatt/emacs-bash-completion). However all these solutions have the same problem  
that they don't actually reflect the internal state of the shell, they are just  
close approximations.  

But the shell knows its own state. In a normal terminal emulator, tab completion  
will give you access to it. When looking at my shell buffer, it seems that the  
information I want is just below the surface. If only there was some way to  
access it. I am not the only one who has had this thought. There is a  
[stackoverflow](https://stackoverflow.com/questions/163591/bash-autocompletion-in-emacs-shell-mode) with over 26,000 views asking this same question. But no one has  
managed to access the native tab completion before. So I determined to solve  
this once and for all.  


## The curious case of bash {#the-curious-case-of-bash}

If you use csh and send some text followed by the tab character, it will print  
out all possible completions. But not bash. Try this code and you get nothing.  

```lisp
(comint-simple-send (get-buffer-process (current-buffer)) "git\t\x15")
```

It works in the terminal but not in the Emacs shell. What conspiracy is this?  
Turns out that Emacs and bash have put a lot of effort into making sure  
completion does not work. The first thing to notice is that `explicit-bash-args`  
contains the argument `--no-editing`, which will disable readline completion. Let  
get rid of that shall we?  

```lisp
(setq explicit-bash-args
          (delete "--noediting" explicit-bash-args))
```

However removing that still does not enable tab completion. There must be  
something else going on here. This time it is on the bash side. Looking in the  
source code we see the follow block.  

```c
term = get_string_value ("TERM");
emacs = get_string_value ("EMACS");
inside_emacs = get_string_value ("INSIDE_EMACS");

if (inside_emacs)
  {
    emacs_term = strstr (inside_emacs, ",term:") != 0;
    in_emacs = 1;
  }
 else if (emacs)
   {
     /* Infer whether we are in an older Emacs. */
     emacs_term = strstr (emacs, " (term:") != 0;
     in_emacs = emacs_term || STREQ (emacs, "t");
   }
 else
   in_emacs = emacs_term = 0;

/* Not sure any emacs terminal emulator sets TERM=emacs any more */
no_line_editing |= STREQ (term, "emacs");
no_line_editing |= in_emacs && STREQ (term, "dumb");
```

For some reason that I can't explain, bash has special code for running inside  
Emacs. Lo and behold, if `$TERM` is `dumb` and `$INSIDE_EMACS` is set, then line  
editing is disabled by the shell itself. Any reason for this? I would love to  
know. Changing `$TERM` to something other then `dumb` fixes the issue, but then  
programs might not interpret our terminals capabilities correct. The best thing  
to do is remove the environment variable `$INSIDE_EMACS`. Doesn't seem to do  
anything useful after all.  

```lisp
(advice-add 'comint-term-environment
            :filter-return (lambda (env) (cons "INSIDE_EMACS" env)))
```

And with that we have working tab completion in bash!  


## Making completion "Emacsy" {#making-completion-emacsy}

We could stop here and just create a function to send tab to the underlying  
process. It would behave exactly like the terminal does. But this is Emacs. The  
whole reason we are using `shell-mode` in the first place is because things in  
Emacs are nicer then things in the terminal. We have things like  
`completion-at-point` and `company-mode` that would make a terminal emulators head  
spin. Makes sense to take advantage of that. So I created a new package called  
[native-complete](https://github.com/CeleritasCelery/emacs-native-shell-complete) that talks to the underlying process and gets the actual  
completions that the shell would natively provide. No more trying to use other  
packages to _guess_ the shells state, or starting another process that may be out  
of sync. It even supports invoking subshells! This effort is still work in  
progress, and many shells have yet to be tested. As with many things, it is not  
as simple in the implementation as it should be.  

I hope this is the shell-mode completion Endgame. I don't think I can take much  
more.
