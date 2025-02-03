;;; -*- lexical-binding: t; -*-

;; Improve performance
(setq gc-cons-threshold (* 50 1000 1000)) ;; Increase GC threshold
(setq read-process-output-max (* 1024 1024)) ;; Increase process read size

;; Load config.org file
(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))

;; Define a helper function to load modules
(defun load-config-module (module)
  "Load a configuration module from the lisp directory."
  (load (expand-file-name (concat "lisp/" module) user-emacs-directory)))

;; Load configuration files
(load-config-module "packages")    ;; Package management
(load-config-module "ui")          ;; UI tweaks
(load-config-module "keybindings") ;; Keybindings
(load-config-module "editor")      ;; Editing enhancements
(load-config-module "lsp")         ;; LSP & coding support
(load-config-module "org-mode")    ;; Org-mode customizations
(load-config-module "utils")       ;; Utilities & misc

