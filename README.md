Dot Emacs
=========

My Emacs configuration ^_^.  Borrowed a lot from
[Prelude Emacs](https://github.com/bbatsov/prelude).  Feel free to
grab code snippets.

Structure
---------

```
.emacs.d/

  init.el      # from Emacs 22 and higher, this serves as init file if ~/.emacs is missing
  lisp/
      core/    # core functionality
      modules/ # per module configuration, xxx-conf.el
      vender/  # packages not yet maintained by Emacs package system.
```
