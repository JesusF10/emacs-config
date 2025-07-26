;;; early-init.el --- Emacs configuration early entry point -*- lexical-binding: t; -*-

;;; Commentary:
;; This is the early configuration entry point for Emacs.
;; It loads some configurations to optimize startup and ui design.

(setq package-enable-at-startup nil) ;; Prevent Emacs from loading packages early
(setq native-comp-async-report-warnings-errors 'silent) ;; Silence native-comp warnings
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      inhibit-default-init t) ;; Suppress startup messages

;; Use package load time statistics
(setq use-package-compute-statistics t)

;; Reduce garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Restore garbage collection after startup
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 50 1024 1024))))

;; Prevent unnecessary UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Enable pixel-wise frame resizing
(setq frame-resize-pixelwise t)

;; Prevent implicit frame resizing
(setq frame-inhibit-implied-resize t)

(setq read-process-output-max (* 1024 1024)) ;; for LSP

;; early-init.el ends here
