;;; init.el --- Emacs configuration entry point -*- lexical-binding: t; -*-

;;; Commentary:
;; This is the main configuration entry point for Emacs.
;; It loads modular configuration files from the lisp/ directory.

;;; Code:

;; Improve startup performance
(setq gc-cons-threshold (* 50 1000 1000)) ;; Increase GC threshold during startup
(setq read-process-output-max (* 1024 1024)) ;; Increase process read size

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