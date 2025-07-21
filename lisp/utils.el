;;; utils.el --- Utility functions and miscellaneous settings -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains utility functions, helper functions, and
;; miscellaneous configurations that don't fit in other modules.

;;; Code:

;; Initial window size settings
(when (display-graphic-p)
  ;; Set initial frame size (width x height in characters)
  ;; (add-to-list 'default-frame-alist '(width . 120))   ; 120 characters wide
  ;; (add-to-list 'default-frame-alist '(height . 40))   ; 40 lines tall
  
  ;; Alternative: Set initial frame size in pixels
  ;; (add-to-list 'default-frame-alist '(width . (text-pixels . 1200)))
  ;; (add-to-list 'default-frame-alist '(height . (text-pixels . 800)))
  
  ;; Optional: Set initial position (pixels from top-left corner)
  ;; (add-to-list 'default-frame-alist '(left . 100))
  ;; (add-to-list 'default-frame-alist '(top . 100))
  
  ;; Optional: Start maximized
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  )

;; Utility functions
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

;; Keybindings for utility functions
(global-set-key (kbd "C-c C-r") 'reload-config)
(global-set-key (kbd "C-c C-e") 'open-config-file)
(global-set-key (kbd "C-c C-d") 'open-config-dir)

;;; utils.el ends here