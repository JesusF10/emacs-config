;;; org-mode.el --- Org-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains Org-mode customizations and configurations.
;; Org-mode is deferred (:defer t) to avoid loading at startup (~123ms savings).
;; It will load automatically when opening a .org file.

(use-package org
  :defer t
  :custom
  (org-directory "~/org/")
  (org-log-done t)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t))

;;; org-mode.el ends here
