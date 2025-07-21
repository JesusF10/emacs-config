;;; keybindings.el --- Custom keybindings configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains custom keybindings and key remappings.

;;; Code:

;; Better defaults
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c r") 'revert-buffer)

;; Window navigation
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c l") 'windmove-right)

;; Buffer management
(global-set-key (kbd "C-c b") 'switch-to-buffer)
(global-set-key (kbd "C-c k") 'kill-buffer)

;; File operations
(global-set-key (kbd "C-c f") 'find-file)
(global-set-key (kbd "C-c F") 'find-file-other-window)

;;; keybindings.el ends here