;;; init.el --- Initialization file for Emacs
;;;
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;;
;;; Code:

;; Package independent configuration.
(setq backup-directory-alist `(("." . "~/.saves")))
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(setq save-abbrevs nil)
;; TODO: Enable auto-revert-mode with doc-view-mode

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
  (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1))))
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
;;; .emacs ends here
