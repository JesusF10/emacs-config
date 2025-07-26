;; init.el --- Emacs configuration entry point -*- lexical-binding: t; -*-

;;; Commentary:
;; This is the main configuration entry point for Emacs.
;; It loads modular configuration files from the lisp/ directory.

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modules configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define a helper function to load modules
(defun load-config-module (module)
  "Load a configuration module from the lisp directory."
  (load (expand-file-name (concat "lisp/" module) user-emacs-directory)))

;; Load configuration modules in order
(load-config-module "packages")    ;; Package management (Elpaca setup)
(load-config-module "keybindings") ;; Custom keybindings
(load-config-module "ui")          ;; UI tweaks and appearance
(load-config-module "editor")      ;; Editing enhancements
(load-config-module "lsp")         ;; LSP & coding support
(load-config-module "org-mode")    ;; Org-mode customizations
(load-config-module "utils")       ;; Utilities & miscellaneous

;; Restore GC threshold after initialization
(setq gc-cons-threshold (* 2 1000 1000))


;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3061706fa92759264751c64950df09b285e3a2d3a9db771e99bcbb2f9b470037" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "4d5d11bfef87416d85673947e3ca3d3d5d985ad57b02a7bb2e32beaf785a100e" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
