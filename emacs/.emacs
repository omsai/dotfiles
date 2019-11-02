;;; init.el --- Initialization file for Emacs
;;;
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;;
;;; Code:

;; Package independent configuration.
(menu-bar-mode -1)
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(setq backup-directory-alist `(("." . "~/.saves")))
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)
(setq save-abbrevs nil)
(savehist-mode 1)
(setq dired-listing-switches "-alh")
(defun my-c-mode-common-hook ()
  "Disable extra lambda indentation in 'c-mode' and related modes."
  (c-set-offset 'inlambda 0))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(defun un-mitm-url (url)
  "Deobfuscate URL from UConn's outlook protection.

See https://stackoverflow.com/a/797552"
  (interactive "Murl: ")
  (apply 'last (apply 'last (last (url-parse-query-string (url-unhex-string url))))))
(defun has-no-internet ()
  "Return true if no internet."
  (not (equal 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1" "eff.org"))))

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
(unless (has-no-internet)
  (setq use-package-compute-statistics t)
  (setq use-package-always-ensure t)
  (use-package auto-package-update
    :config
    (setq auto-package-update-delete-old-versions t)
    (setq auto-package-update-hide-results t)
    (auto-package-update-maybe)))
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
  :defer t)
(use-package poly-R
  :requires poly-markdown)
;; Continuous Integration.
(use-package yaml-mode)
;; Git interface.
(use-package magit
  :bind ("C-x g" . magit-status))
;; LaTeX PDF.
(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-PDF-mode t))
;; Bash unit tests.
(use-package bats-mode)
;; Org mode hooks.
(use-package org
  :ensure nil
  :init
  (add-hook 'org-mode-hook #'auto-fill-mode)
  (setq fill-column 80)
  (setq org-src-fontify-natively t)
  (setq org-file-apps
	'(("\\.epub" . "ebook-viewer %s"))))
;; Org-ref.
;; (use-package org-ref
;;   :config
;;   (setq reftex-default-bibliography '("~/Sync/bibliography/references.bib")
;; 	org-ref-bibliography-notes "~/Sync/bibliography/notes.org"
;; 	org-ref-pdf-directory "~/Sync/bibliography/bibtex-pdfs/")
;;   (setq bibtex-completion-bibliography "~/Sync/bibliography/references.bib"
;; 	bibtex-completion-library-path "~/Sync/bibliography/bibtex-pdfs"
;; 	bibtex-completion-notes-path "~/Sync/bibliography/helm-bibtex-notes"))
;; Remove old packages.
(package-autoremove)

;; GitHub packages.
(defun use-package-github (package)
  "Install Emacs PACKAGE string 'user/package' from GitHub."
  (unless (has-no-internet)
    (let ((url (concat "https://github.com/" package))
	  (dir (file-name-nondirectory package)))
      (let ((install-dir (concat "~/.emacs.d/" dir)))
	(if (file-directory-p install-dir)
	    (shell-command (concat "git -C " install-dir " pull"))
	(shell-command (concat "git clone " url " " install-dir)))
	(load (concat (file-name-as-directory install-dir) dir ".el"))))))
(use-package-github "wentasah/meson-mode")

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
(let ((git				; "git -C ~/.dotfiles "
       (concat
	"git -C "
	;; Get absolute path.
	"~/"
	(first-directory-in-path
	 ;; Get directory of symlink target.
	 (file-symlink-p "~/.emacs"))
	" ")))
  (unless (has-no-internet)
    ;; git fetch to check if the origin is ahead of our local repo.
    (unless (not (equal 0 (shell-command (concat git "fetch"))))
      ;; Now git status is aware of remote commits.
      (let ((git-status-full
	     (shell-command-to-string
	      (concat git "status --short --branch"))))
	(let ((git-status-firstline
	       (substring git-status-full 0
			  (string-match "\n" git-status-full))))
	  (let ((pos-first-bracket
		 (string-match "\\[" git-status-firstline)))
	    ;; Can look like "## master...origin/master [behind 1]"
	    (unless (not pos-first-bracket)
	      (let ((state (substring
			    git-status-firstline
			    (+ 1 pos-first-bracket)
			    (string-match " " git-status-firstline
					  pos-first-bracket))))
		(unless (not (string= "behind" state))
		  (unless (not (equal 0 (shell-command (concat git "pull"))))
		    (restart-emacs)))))))))))
;;; .emacs ends here
