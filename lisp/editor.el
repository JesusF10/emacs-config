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
  (setq which-key-idle-delay 0.1)
  (setq which-key-popup-type 'side-window))

;; Save place in files
(save-place-mode 1)

;; Recent files
(recentf-mode 1)
(setq recentf-max-saved-items 50)

;; Modern completion and fuzzy finding
;; Vertico: A performant and minimalistic vertical completion UI
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t))

;; Orderless: Provides orderless completion style for fuzzy matching
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia: Rich annotations in the minibuffer
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; File explorer configuration
(use-package dired
  :config
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  ;; this command is useful when you want to close the window of `dirvish-side'
  ;; automatically when opening a file
  (put 'dired-find-alternate-file 'disabled nil))

;; Dirvish: Modern file manager to replace dired
(use-package dirvish
  :ensure t
  :defer t
  :init
  ;; Let dirvish take over dired globally
  (dirvish-override-dired-mode)
  :config
  ;; Basic dirvish settings
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  
  ;; Configure attributes (visual enhancements)
  (setq dirvish-attributes
        '(vc-state subtree-state all-the-icons collapse git-msg file-time file-size))
  
  :bind
  (("C-c C-f" . dirvish)
   ("C-c C-s" . dirvish-side)   ; Side panel version
   :map dirvish-mode-map
   ;; Vim-style navigation
   ("j" . dired-next-line)
   ("k" . dired-previous-line)
   ("h" . dired-up-directory)
   ("l" . dired-find-file)
   ;; Additional useful bindings
   ("?" . dirvish-dispatch)     ; Help menu
   ("q" . quit-window)))        ; Quick quit

;;; editor.el ends here