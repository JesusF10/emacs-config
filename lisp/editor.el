;;; editor.el --- Editor enhancements and settings -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains editor enhancements, text editing improvements,
;; and general editing behavior configurations.

;;; Code:

;; Better defaults
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq sentence-end-double-space nil)
(setq require-final-newline t)
(setq tab-always-indent 'complete)

;; Backup and auto-save settings
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory))))
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-saves/" user-emacs-directory) t)))

;; Better scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Delete selection mode
(delete-selection-mode 1)

;; Auto-revert buffers when files change on disk
(global-auto-revert-mode 1)

;; Electric pair mode for automatic bracket pairing
(electric-pair-mode 1)

;; Which-key mode for better keybinding discovery
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3)
  (setq which-key-popup-type 'side-window))

;; Save place in files
(save-place-mode 1)

;; Recent files
(recentf-mode 1)
(setq recentf-max-saved-items 50)

;;; editor.el ends here