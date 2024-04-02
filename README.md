# editor
Common lisp server and c++ client for 3d shared text editing.

* Simultaneous text editing by multiple users via socket.
* GPU rendering of closed quadratic bezier curves based on "Eric Lengyel, GPU-Centered Font Rendering Directly from Glyph Outlines, Journal of Computer Graphics Techniques (JCGT), vol. 6, no. 2, 31-47, 2017".
  
![image](https://github.com/chae1/editor/assets/29856486/18e689d2-46ee-4e01-9219-2472f8a9e888)

## How to run the editor

### For Mac OS

1. [install Common Lisp](https://lisp-lang.org/learn/getting-started/)
   
   in terminal, run homebrew to install Common Lisp implementation ([SBCL](https://www.sbcl.org/))
   ```console
   brew install sbcl
   ```
   download and setup a package manager ([Quicklisp](https://www.quicklisp.org/beta/))
   ```console
   curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
   sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
        --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
        --eval '(ql:add-to-init-file)' \
        --quit
   ```
   run homebrew to install [GNU Emacs](https://www.gnu.org/savannah-checkouts/gnu/emacs/emacs.html)
   ```console
   brew install --cask emacs
   ```
   run quicklisp to install Lisp IDE ([SLIME](https://slime.common-lisp.dev/))
   ```console
   sbcl --eval '(ql:qickload :qicklisp-slime-helper)' --quit
   ```
   set emacs to interoperate with slime
   ```console
   echo '\n(load (expand-file-name "~/.quicklisp/slime-helper.el"))' `
       `'\n(setq inferior-lisp-program "sbcl")\n' >> ~/.emacs.d/init.el
   ```
   run emacs
   ```console
   emacs
   ```
   in emacs, press `Alt+x`, type `slime`, and press `Enter` 
