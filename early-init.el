(setq package-enable-at-startup nil) ;; Prevent Emacs from loading packages early
(setq native-comp-async-report-warnings-errors 'silent) ;; Silence native-comp warnings
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      inhibit-default-init t) ;; Suppress startup messages

;; Reduce garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 50 1000 1000))))

;; Prevent unnecessary UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Enable line numbers
(global-display-line-numbers-mode t)

(setq package-enable-at-startup nil)

(setq gc-cons-threshold (* 100 1024 1024)) ;; speed up init
(setq read-process-output-max (* 1024 1024)) ;; for LSP

