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
(global-set-key (kbd "C-c b") 'consult-buffer)  ; Use consult-buffer instead of switch-to-buffer
(global-set-key (kbd "C-c k") 'kill-buffer)

;; File operations
(global-set-key (kbd "C-c f") 'consult-find)    ; Fuzzy find files (with preview control)
(global-set-key (kbd "C-c F") 'find-file-other-window)
(global-set-key (kbd "C-c C-f") 'find-file)     ; Traditional find-file for when you want directory navigation
(global-set-key (kbd "C-c s") 'consult-line)    ; Search within current buffer
(global-set-key (kbd "C-c S") 'consult-ripgrep) ; Search across project files

;;; keybindings.el ends here