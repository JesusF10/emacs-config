;;; keybindings.el --- Custom keybindings configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains custom keybindings and key remappings.


;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliar Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun reload-config ()
  "Reload the Emacs configuration."
  (interactive)
  (load-file user-init-file))

(defun open-config-file ()
  "Open the main configuration file."
  (interactive)
  (find-file user-init-file))
 
(defun open-config-dir ()
  "Open the configuration directory in dired."
  (interactive)
  (dired user-emacs-directory))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manual Mapping Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-prefix-command 'editor-map nil "Editor")
(define-prefix-command 'visual-map nil "Visual")
(define-prefix-command 'omni-map  nil "Omni")
(define-prefix-command 'lsp-map nil "LSP")


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Binding Configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-key "C-c e" 'editor-map)
(bind-key "C-c v" 'visual-map)
(bind-key "C-c o" 'omni-map)
(bind-key "C-c l" 'lsp-map)
 
(bind-key (kbd "r") 'reload-config 'editor-map)
(bind-key (kbd "i") 'open-config-file 'editor-map)
(bind-key (kbd "R") 'restart-emacs 'editor-map)
(bind-key (kbd "I") 'open-config-dir 'editor-map)


;; Key Unbinding for avoiding frame suspension
(use-package frame
  :ensure nil  ;; Mark as built-in
  :config
  (unbind-key (kbd "C-z") global-map)     
  (unbind-key (kbd "C-x C-z") global-map))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aditional settings: ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Better defaults
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c r") 'revert-buffer)

;; File operations
(global-set-key (kbd "C-c F") 'find-file-other-window)


;;; keybindings.el ends here
