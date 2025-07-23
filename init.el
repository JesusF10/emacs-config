;;; init.el --- Emacs configuration entry point -*- lexical-binding: t; -*-

;;; Commentary:
;; This is the main configuration entry point for Emacs.
;; It loads modular configuration files from the lisp/ directory.

;;; Code:

;; Improve startup performance
(setq gc-cons-threshold (* 50 1000 1000)) ;; Increase GC threshold during startup
(setq read-process-output-max (* 1024 1024)) ;; Increase process read size

;; Enable underlining at the descent line
(setq x-underline-at-descent-line t)

;; Set default tab width
(setq-default tab-width 4)

;; Disable dialog boxes and file dialogs
(setq use-dialog-box nil)
(setq use-file-dialog nil)

;; Enable recursive minibuffer
(setq enable-recursive-minibuffers t)

;; Shut down the bell
(setq ring-bell-function #'ignore)

;; Load newer files if available
(setq load-prefer-newer noninteractive)

;; Case-sensitive completion
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

;; Enable short answers in the minibuffer
(setq use-short-answers t)

;; Trash files instead of deleting them
(setq delete-by-moving-to-trash t)

;; Enable line numbers
(global-display-line-numbers-mode t)

;; Define a helper function to load modules
(defun load-config-module (module)
  "Load a configuration module from the lisp directory."
  (load (expand-file-name (concat "lisp/" module) user-emacs-directory)))

;; Load configuration modules in order
(load-config-module "packages")    ;; Package management (Elpaca setup)
(load-config-module "ui")          ;; UI tweaks and appearance
(load-config-module "keybindings") ;; Custom keybindings
(load-config-module "editor")      ;; Editing enhancements
(load-config-module "lsp")         ;; LSP & coding support
(load-config-module "org-mode")    ;; Org-mode customizations
(load-config-module "utils")       ;; Utilities & miscellaneous

;; Restore GC threshold after initialization
(setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here