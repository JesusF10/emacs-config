;;; ui.el --- UI and appearance configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains UI customizations, themes, fonts, and visual settings.

;;; Code:

;; Basic UI improvements
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; Show line numbers
(global-display-line-numbers-mode 1)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Better defaults
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(setq use-dialog-box nil)

;; Show matching parentheses
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Theme configuration
(use-package catppuccin-theme
  :ensure t
  :config
  (setq catppuccin-flavor 'mocha)
  (catppuccin-reload)) ;; Flavors: latte, frappe, macchiato, mocha

;; Font configuration
(use-package nerd-icons
  :ensure t
  :if (display-graphic-p))

;; Doom modeline configuration
(use-package doom-modeline
  :ensure t
  :if (display-graphic-p)
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-support-imenu t)
  (doom-modeline-height 30)
  (doom-modeline-bar-width 5)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-env-version t)
  (doom-modeline-indent-info t)
  (doom-modeline-checker-simple t)
  (doom-modeline-vcs-max-length 15))


;;; ui.el ends here