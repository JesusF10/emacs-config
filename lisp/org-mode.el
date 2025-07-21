;;; org-mode.el --- Org-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains Org-mode customizations and configurations.

;;; Code:

;; Basic org-mode settings
(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "notes.org"))

;; Org-mode keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Better org-mode defaults
(setq org-log-done t)
(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation t)

;; Org-mode visual improvements
(setq org-hide-emphasis-markers t)
(setq org-pretty-entities t)

;;; org-mode.el ends here