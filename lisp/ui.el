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

;;; ui.el ends here