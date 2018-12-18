;;; init.el --- Initialization file for Emacs
;;;
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;;
;;; Code:

;; Package independent configuration.
(tool-bar-mode -1)
(setq backup-directory-alist `(("." . "~/.saves")))
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)
(setq save-abbrevs nil)
(savehist-mode 1)
(defun un-mitm-url (url)
  "Deobfuscate URL from UConn's outlook protection.

See https://stackoverflow.com/a/797552"
  (interactive "Murl: ")
  (apply 'last (apply 'last (last (url-parse-query-string (url-unhex-string url))))))

;; Package specific configuration.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
; Force refresh contents if new package installations fail.
;(package-refresh-contents)
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)	 ; See https://github.com/jwiegley/use-package
(setq use-package-always-ensure t)
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))
(use-package flycheck
  :config
  (global-flycheck-mode)
  :init
  (setq-default flycheck-disabled-checkers '(c/c++-clang)))
;; Python.
(use-package elpy
  :config
  (elpy-enable)
  :init
  (add-hook
   'elpy-mode-hook
   (lambda ()
     ;; Don't show indentation guides.
     (highlight-indentation-mode -1)
     (add-to-list
      'python-shell-completion-native-disabled-interpreters "jupyter")))
  (setq
   ;; Don't use python2, even if it's available.
   elpy-rpc-python-command "python3"
   ;; Use jupyter interpreter.
   python-shell-interpreter (expand-file-name "~/.local/bin/jupyter")
   python-shell-interpreter-args "console --simple-prompt"
   python-shell-prompt-detect-failure-warning nil))
;; R.
(use-package ess
  :init
  (require 'ess-site)
  :config
  ;; Yes, ess-toggle-underscore is deprecated in ESS 18, but it's not
  ;; clear to me how to use the replacement ess-insert-assign
  ;; function.
  (ess-toggle-underscore nil))
  ;; (ess-insert-assign ""))
  ;; (local-unset-key "_"))
(use-package poly-markdown
  :config
  (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode)))
(use-package poly-noweb)
(use-package poly-R
  :requires (poly-noweb poly-markdown)
  :config
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode)))
;; Continuous Integration.
(use-package yaml-mode)
;; Git interface.
(use-package magit
  :bind ("C-x g" . magit-status))
;; LaTeX PDF.
(use-package tex
  :ensure auctex
  :config
  (setq TeX-PDF-mode t))

;; Restart emacs if any dotfiles were updated.  FIXME: One should only
;; need to restart if .emacs related files were updated.
(use-package restart-emacs)
(defun first-directory-in-path (path)
  "Return first directory in PATH with trailing slash.

Emacs doesn't provide a directory separator character, so this
function recursively runs 'file-name-directory' until nil, and
returns the directory before it became nil."
  (let ((path-new (file-name-directory path)))
    (if path-new
	(first-directory-in-path (directory-file-name path-new))
      (file-name-as-directory path))))
(let ((path-dotfiles (first-directory-in-path
		      ;; Get directory of symlink target.
		      (file-symlink-p "~/.emacs"))))
  (let ((pull-output
	 ;; Update .dotfiles git repo.
	 (shell-command-to-string (concat "git -C " path-dotfiles " pull"))))
    ;; Restart emacs if needed.
    (if (not (equal pull-output "Already up to date.
"))
	(restart-emacs))))
;;; .emacs ends here
