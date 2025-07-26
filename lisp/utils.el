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


;;; utils.el ends here
