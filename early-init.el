(setq package-enable-at-startup nil) ;; Prevent Emacs from loading packages early
(setq native-comp-async-report-warnings-errors 'silent) ;; Silence native-comp warnings

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
