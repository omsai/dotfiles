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
(if (eq system-type 'darwin)
    (setq dired-listing-switches "-alh")
    (setq dired-listing-switches "-alhv"))
(semantic-mode 1)
(setq sort-fold-case t)
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
(defun open-terminal-here ()
  "Open macOS Terminal.app GUI in the current directory.

See URL `https://emacs.stackexchange.com/a/31009;."
  (interactive)
  (shell-command
   (concat "open -a Terminal "
           (shell-quote-argument (expand-file-name
                                  default-directory))) nil nil))
(defun has-no-internet ()
  "Return non-nil if no internet."
  (not (equal 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1" "eff.org"))))
(defun up-directory (path)
  "Move up a directory in PATH without affecting the kill buffer."
  (interactive "p")
  (if (string-match-p "/." (minibuffer-contents))
      (let ((end (point)))
	(re-search-backward "/.")
	(forward-char)
	(delete-region (point) end))))
(define-key minibuffer-local-filename-completion-map
  [C-backspace] #'up-directory)

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
;; General purpose.
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))
(use-package flycheck
  :config
  (global-flycheck-mode)
  :init
  (setq-default flycheck-disabled-checkers '(org-lint))
  (unless (eq system-type 'darwin)
    (append flycheck-disabled-checkers '(c/c++-clang)))
  (setq flycheck-gcc-openmp t)
  (lambda ()
    (add-to-list 'flycheck-gcc-include-path "/usr/share/R/include")))
(use-package flyspell)
(use-package ace-window
  :init
  (winner-mode)
  :bind ("M-o" . ace-window))
(use-package unfill)
(use-package flymake-ansible-lint
  :ensure t
  :commands flymake-ansible-lint-setup
  :hook (((yaml-ts-mode yaml-mode) . flymake-ansible-lint-setup)
         ((yaml-ts-mode yaml-mode) . flymake-mode)))
(use-package detached
  :if
  (locate-file "dtach" exec-path)
  :init
  (detached-init)
  :bind
  ;; Replace `async-shell-command' with `detached-shell-command'
  ([remap async-shell-command] . detached-shell-command)
  ;; Replace `compile' with `detached-compile'
  ([remap compile] . detached-compile)
  ([remap recompile] . detached-compile-recompile)
  ;; Replace built in completion of sessions with `consult'
  ([remap detached-open-session] . detached-consult-session)
  :custom
  (detached-show-output-on-attach t)
  (detached-terminal-data-command system-type))
(use-package erc
  :defer t
  :ensure nil
  :config
  ;; Enable SASL to login from VPNs, colorize nicknames, show panels,
  ;; and log buffers.
  (setopt erc-modules
	  (seq-union '(sasl services nicks bufbar nickbar log keep-place)
		     erc-modules))
  :custom
  ;; Protect me from accidentally sending excess lines.
  (erc-inhibit-multiline-input t)
  (erc-send-whitespace-lines t)
  (erc-ask-about-multiline-input t)
  ;; Scroll all windows to prompt when submitting input.
  (erc-scrolltobottom-all t)
  ;; Reconnect automatically using a fancy strategy.
  (erc-server-reconnect-function #'erc-server-delayed-check-reconnect)
  (erc-server-reconnect-timeout 30)
  ;; Log all channels.
  (erc-enable-logging t)
  (erc-save-buffer-on-part t)
  ;; Show new buffers in the current window instead of a split.
  (erc-interactive-display 'buffer)
  ;; List of IRC message types to hide.
  (erc-hide-list '("JOIN" "PART" "QUIT" "MODE" "NICK"))
  ;; Insert a newline when I hit <RET> at the prompt, and prefer
  ;; something more deliberate for actually sending messages.
  :bind (:map erc-mode-map
              ("RET" . nil)
              ("C-c C-c" . #'erc-send-current-line))
  ;; Emphasize buttonized text in notices.
  :custom-face (erc-notice-face ((t (:slant italic :weight unspecified)))))
(use-package erc-sasl
  :defer t
  :ensure nil
  ;; Since my account name is the same as my nick, free me from having
  ;; to hit C-u before M-x erc to trigger a username prompt.
  :custom
  (erc-sasl-user :nick)
  (erc-use-auth-source-for-nickserv-password t)
  (erc-sasl-password "irc.libera.chat")
  (erc-sasl-auth-source-function
   #'erc-sasl-auth-source-password-as-host))
(use-package erc-track
  :defer t
  :ensure nil
  ;; Prevent JOINs and PARTs from lighting up the mode-line.
  :config (setopt erc-track-faces-priority-list
                  (remq 'erc-notice-face erc-track-faces-priority-list))
  :custom (erc-track-priority-faces-only 'all))
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
  ;; The jupyter interpreter is typically only available inside virtual
  ;; environmnets due to python 3.12 advising use of pipx instead of global
  ;; --user installations.  Therefore, only configure jupyter when it is found
  ;; in the path.
  (if (locate-file "jupyter" exec-path)
      (setq
       elpy-rpc-virtualenv-path 'current
       ;; Use jupyter interpreter.
       python-shell-interpreter "jupyter"
       python-shell-interpreter-args "console --simple-prompt"
       python-shell-prompt-detect-failure-warning nil
       ;; Workaround for
       ;; https://github.com/jorgenschaefer/elpy/issues/1976
       elpy-shell-echo-output nil)
    (setq
     ;; Don't use python2, even if it's available.
     elpy-rpc-python-command "python3")))
;; R.
(use-package ess
  :defer t
  :init
  (require 'ess-r-mode)
  :config
  (setq ess-auto-width 'window)
  (setq ess-style 'RStudio)
  (setq ess-own-style-list
	'((ess-indent-offset . 4)
	  (ess-offset-arguments . open-delim)
	  (ess-offset-arguments-newline . prev-call)
	  (ess-offset-block . prev-line)
	  (ess-offset-continued . straight)
	  (ess-align-nested-calls "ifelse")
	  (ess-align-arguments-in-calls "function[ 	]*(")
	  (ess-align-continuations-in-calls . t)
	  (ess-align-blocks control-flow)
	  (ess-indent-from-lhs arguments fun-decl-opening)
	  (ess-indent-from-chain-start . t)
	  (ess-indent-with-fancy-comments . t))))
(use-package poly-markdown)
(use-package quarto-mode)
(use-package stan-mode
  :config
  (setq indent-tabs-mode nil))
;; Continuous Integration.
(use-package yaml-mode)
;; Git interface.
(use-package magit
  :bind ("C-x g" . magit-status))
;; Build systems.
(use-package meson-mode)
(use-package ninja-mode)
;; LaTeX PDF.
(use-package tex
  :defer t
  :ensure nil
  :init
  (add-hook 'LaTeX-mode-hook #'flyspell-mode)
  :config
  (defvar TeX-PDF-mode)
  (setq TeX-PDF-mode t)
  ;; Fix fontification, etc. https://emacs.stackexchange.com/a/30430
  (setq TeX-parse-self t)
  (setq TeX-tree-roots
	(if (locate-file "tlmgr" exec-path)
	    (progn
	      (let ((lines (process-lines "tlmgr" "conf")))
		(list (replace-regexp-in-string
		       "^[^=]+=" ""
		       (elt
			(seq-filter
			 (lambda (line) (string-prefix-p "TEXMFDIST" line))
			 lines)
			0)))))
	  nil))
  ;; Use the Ubuntu 25 default PDF viewer, Papers.
  (if (locate-file "papers" exec-path)
      (progn
	(setq TeX-view-program-list
	      '(("Papers" "papers --page-index=%(outpage) %o")))
	(add-to-list 'TeX-view-program-selection
		     '(output-pdf "Papers")))))
;; Bash unit tests.
(use-package bats-mode)
;; Org mode hooks.
(use-package org
  :ensure nil
  :bind ("C-c a" . 'org-agenda)
  :init
  (add-hook 'org-mode-hook #'auto-fill-mode)
  (add-hook 'org-mode-hook #'auto-revert-mode)
  (add-hook 'org-mode-hook #'flyspell-mode)
  (setq fill-column 80)
  (setq org-agenda-sticky t)
  (if (eq system-type 'darwin)
      (setq org-agenda-prefix-format
	    '((agenda . "%-7:c%?-12t% s")
	      (todo . " %i %-12:c")
	      (tags . " %i %-12:c")
	      (search . " %i %-12:c"))))
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
  (setq org-confirm-babel-evaluate nil)
  (plist-put org-format-latex-options :scale 1.5))
(use-package alert
  :config
  (setq alert-default-style
	(cl-case (window-system)
	  (x 'notifications)
	  (ns 'osx-notifier))))
(use-package appt
  :ensure nil
  :config
  (setq appt-display-duration 725)	; seconds.
  (setq appt-display-interval 1)	; minute.
  (advice-add 'appt-check
	      :before
	      (lambda (&rest args)
		(org-agenda-to-appt t)))
  (setq appt-display-format 'echo)
  (appt-activate t))
(use-package org-agenda
  :ensure nil
  :init
  (add-to-list 'org-modules 'org-habit t)
  (add-hook 'org-agenda-finalize-hook 'org-agenda-to-appt 'append)
  (setq org-agenda-files
	(seq-filter
	 (lambda (elt) (file-exists-p elt))
	 '("~/corelab1"
	   "~/uits"
	   "~/kirschner-lab/schedule"
	   "~/shoemaker-lab/schedule"
	   "~/Sync/schedule")))
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
(use-package tj3-mode)
(use-package font-lock-studio)
;; Local packages.
(use-package shell-session-mode
  :if (file-exists-p "~/.emacs.d/lisp/shell-session-mode.el")
  :load-path "lisp"
  :ensure nil)
;; Remove old packages.
(package-autoremove)

;; Restart emacs if any dotfiles were updated.  FIXME: One should only
;; need to restart if .emacs related files were updated.
(use-package restart-emacs)
(defun first-directory-in-path (path)
  "Return first directory in PATH with trailing slash.

Emacs doesn't provide a directory separator character, so this
function recursively runs \='file-name-directory\=' until nil, and
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
