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
(add-hook 'write-contents-hooks 'c-mode-untabify nil t)
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.j2\\'" . jinja2-mode))
(defun un-mitm-url (url)
  "Deobfuscate URL from UConn's outlook protection.

Run the function in the scratch Lisp Interaction buffer using
\\[eval-print-last-sexp] or its more convenient keymap shortcut.

See URL `https://stackoverflow.com/a/797552;."
  (interactive "Murl: ")
  (apply 'last (apply 'last (last (url-parse-query-string (url-unhex-string url))))))
(defun has-no-internet ()
  "Return non-nil if no internet."
  (not (equal 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1" "eff.org"))))

;; Package specific configuration.
;;
;; Workaround known error with emacs <= 26.2 "Failed to download
;; 'melpa' archive"
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
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
    :ensure t
    :config
    (setq auto-package-update-delete-old-versions t)
    (setq auto-package-update-hide-results t)
    (auto-package-update-maybe)))
(use-package flycheck
  :config
  (global-flycheck-mode)
  :init
  (setq-default flycheck-disabled-checkers '(c/c++-clang))
  (lambda ()
    (add-to-list 'flycheck-gcc-include-path "/usr/share/R/include")))
;; Refactoring.
(use-package iedit)
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
   elpy-rpc-virtualenv-path 'current
   ;; Use jupyter interpreter.
   python-shell-interpreter (expand-file-name "~/.local/bin/jupyter")
   python-shell-interpreter-args "console --simple-prompt"
   python-shell-prompt-detect-failure-warning nil))
;; R.
(use-package ess
  :defer t)
(use-package poly-R
  :requires poly-markdown)
(use-package stan-mode
  :config
  (setq indent-tabs-mode nil))
(use-package minizinc-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.mzn\\'" . minizinc-mode)))
;; Continuous Integration.
(use-package yaml-mode)
;; Git interface.
(use-package magit
  :bind ("C-x g" . magit-status))
;; Build systems.
(use-package meson-mode)
;; LaTeX PDF.
(use-package tex
  :defer t
  :ensure nil
  :init
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  :config
  (defvar TeX-PDF-mode)
  (setq TeX-PDF-mode t)
  (setq LaTeX-verbatim-environments-local '("Verbatim" "lstlisting")))
;; Bash unit tests.
(use-package bats-mode)
;; Org mode hooks.
(use-package org
  :ensure nil
  :bind ("C-c a" . 'org-agenda)
  :init
  (add-hook 'org-mode-hook #'auto-fill-mode)
  (add-hook 'org-mode-hook #'auto-revert-mode)
  (setq fill-column 80)
  (setq org-enforce-todo-dependencies t)
  (setq org-list-allow-alphabetical t)
  (setq org-log-done 'time)
  (setq org-src-fontify-natively t)
  (setq org-file-apps
	'(("\\.epub" . "ebook-viewer %s")))
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((R . t)
				 (latex . t)
				 (shell . t)))
  (setq org-confirm-babel-evaluate nil))
(use-package appt
  :ensure nil
  :config
  (setq appt-display-duration 725)	; seconds.
  (setq appt-display-interval 1)	; minute.
  (setq appt-display-format 'window)
  (require 'notifications)		; Load notifications-notify.
  (defun notify-send (title body)
    (interactive)
    (if (eq window-system 'x)
	(notifications-notify
	 :title title
	 :body body
	 :timeout (* 60000 appt-display-interval))))
  (defun notify-send-wrap (mins new-time body)
    (notify-send (format "Appointment in %s min(s)" mins) body))
  (setq appt-disp-window-function (function notify-send-wrap))
  (appt-activate t))
(use-package org-agenda
  :ensure nil
  :init
  (add-to-list 'org-modules 'org-habit t)
  (add-hook 'org-agenda-finalize-hook 'org-agenda-to-appt 'append)
  (setq org-agenda-files
	(list "~/corelab1"
	      "~/uits"
	      "~/Sync/schedule"))
  (setq org-agenda-window-setup "current-window")
  (setq org-agenda-span 14)
  (setq org-agenda-use-time-grid nil))
(use-package org-kanban)
;; Org-ref.
;; (use-package org-ref
;;   :config
;;   (setq reftex-default-bibliography '("~/Sync/bibliography/references.bib")
;; 	org-ref-bibliography-notes "~/Sync/bibliography/notes.org"
;; 	org-ref-pdf-directory "~/Sync/bibliography/bibtex-pdfs/")
;;   (setq bibtex-completion-bibliography "~/Sync/bibliography/references.bib"
;; 	bibtex-completion-library-path "~/Sync/bibliography/bibtex-pdfs"
;; 	bibtex-completion-notes-path "~/Sync/bibliography/helm-bibtex-notes"))
(use-package ledger-mode
  :init
  (setq ledger-binary-path "hledger"))
;; Remove old packages.
(package-autoremove)

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
