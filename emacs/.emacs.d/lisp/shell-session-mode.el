;;; shell-session-mode.el --- Colorize Shell Commands within their Output.

;;; Commentary:
;;
;; Colorizes shell commands and their prompts similar to the "console"
;; specifier in GitHub Flavored Markdown code blocks:
;;
;;    ```console
;;    $ command                              <-- this is colorized
;;    output
;;    more output
;;    user@machine$ command                  <-- this is colorized
;;    output
;;    ...
;;    ```
;;
;; This mode therefore allows using equivalent org-mode source blocks:
;;
;;    #+begin_src shell-session
;;      $ command
;;      output
;;      more output
;;      user@machine$ command
;;      output
;;      ...
;;    #+end_src
;;
;; Or you can simply save files ending in .sh-session
;;
;; The original implementation of GitHub Flavored Markdown of "console" code
;; blocks falls under the heading of "ShellSession" in GitHub's linguist
;; project (https://github.com/github/linguist).  These code blocks in turn use
;; the regexp rules defined in GitHub's Atom editor CoffeeScript file
;; (https://github.com/atom/language-shellscript/blob/master/grammars/shell-session.cson).
;;
;; However the full regex of the CoffeeScript file is not implemented here.
;; Only $ prompts are supported because # root prompts may cause confusion with
;; # shell comments.
;;
;; To install this mode, copy this file to your ~/.emacs.d/lisp directory or
;; similar.  If you prefer to install Emacs packages with use-package, you can
;; add this single expression to your Emacs init file:
;;
;;    (use-package shell-session-mode
;;      :load-path "lisp"
;;      :ensure nil)
;;
;; If not, you can more simply include these lines in your Emacs init file:
;;
;;    (push "~/.emacs.d/lisp" load-path)
;;    (require 'shell-session-mode)

;;; Code:

(define-derived-mode shell-session-mode fundamental-mode "shell-session-mode"
  "A major mode to display shell sessions."
  :group 'shell-session
  (font-lock-add-keywords
   nil
   '(("^\\<.*\\$ .*\\>" . font-lock-keyword-face))))

(add-to-list 'auto-mode-alist '("\\.sh-session\\'" . shell-session-mode))

(provide 'shell-session-mode)

;;; shell-session-mode.el ends here.
