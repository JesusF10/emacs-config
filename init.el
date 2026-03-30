;; init.el --- Emacs configuration entry point -*- lexical-binding: t; -*-

;;; Commentary:
;; This is the main configuration entry point for Emacs.
;; It loads modular configuration files from the lisp/ directory.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Performance Optimization (Startup) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Native Compilation Settings (Emacs 28+)
(when (native-comp-available-p)
  ;; Silence native-comp warnings (they can be noisy)
  (setq native-comp-async-report-warnings-errors nil)
  
  ;; Increase native compilation cache size
  (setq native-comp-async-jobs-number 4)  ; Use 4 CPU cores for compilation
  
  ;; Compile packages in background
  (setq native-comp-deferred-compilation t)
  
  ;; Set compilation path
  (startup-redirect-eln-cache
   (expand-file-name "eln-cache/" user-emacs-directory))
  
  (message "Native compilation enabled with %d jobs" native-comp-async-jobs-number))

;; Maximize GC threshold during startup (defer GC for speed)
;; This will be reset after initialization completes
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Increase process read size for LSP servers (especially Eglot/Ty)
;; Default is 4KB, we increase to 1MB for better LSP performance
(setq read-process-output-max (* 1024 1024))

;; Prevent garbage collection during minibuffer usage
;; GC during completion causes noticeable lag
(defun my/minibuffer-gc-setup ()
  "Defer garbage collection while in minibuffer."
  (setq gc-cons-threshold most-positive-fixnum))

(defun my/minibuffer-gc-restore ()
  "Restore garbage collection after minibuffer."
  (run-at-time 1 nil (lambda () (setq gc-cons-threshold my/gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'my/minibuffer-gc-setup)
(add-hook 'minibuffer-exit-hook #'my/minibuffer-gc-restore)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Post-Initialization GC Configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; After startup completes, set GC to optimal runtime values
;; 16MB threshold balances memory usage and GC pauses
;; GC triggers less frequently but still prevents excessive memory growth
(defvar my/gc-cons-threshold (* 16 1024 1024)
  "Optimal GC threshold for runtime (16MB).")

(setq gc-cons-threshold my/gc-cons-threshold
      gc-cons-percentage 0.1)

;; GC when Emacs loses focus (prevents GC during active editing)
;; This strategy significantly reduces perceived lag
(add-hook 'focus-out-hook #'garbage-collect)

;; Show GC statistics in messages (useful for profiling)
;; Disable if it gets noisy: (setq garbage-collection-messages nil)
(setq garbage-collection-messages t)


;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   ))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
