;;; utils.el --- Utility functions and miscellaneous settings -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains utility functions, helper functions, and
;; miscellaneous configurations that don't fit in other modules.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initial Window Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Initial window size settings
(when (display-graphic-p)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aditional settings: ;;
;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Native Compilation Helpers ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (native-comp-available-p)
  (defun my/native-compile-packages ()
    "Native compile all installed packages for better performance.
This can take several minutes but significantly improves runtime speed."
    (interactive)
    (if (yes-or-no-p "Native compile all packages? This may take 5-10 minutes. ")
        (progn
          (message "Starting native compilation of packages...")
          (native-compile-async
           (expand-file-name "elpaca/builds/" user-emacs-directory) 'recursively)
          (message "Native compilation started in background. Check *Async-native-compile-log* for progress."))
      (message "Native compilation cancelled.")))
  
  (defun my/native-compile-config ()
    "Native compile your personal configuration files."
    (interactive)
    (message "Compiling configuration files...")
    (native-compile-async
     (expand-file-name "lisp/" user-emacs-directory) 'recursively)
    (message "Configuration compilation started. Check *Async-native-compile-log*.")))


;;; utils.el ends here
