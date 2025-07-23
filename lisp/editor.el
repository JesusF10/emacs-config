;;; editor.el --- Editor enhancements and settings -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains editor enhancements, text editing improvements,
;; and general editing behavior configurations.

;;; Code:

;; Basic editor settings
(setq-default indent-tabs-mode nil)
(setq sentence-end-double-space nil)
(setq require-final-newline t)
(setq tab-always-indent 'complete)

;; Helper function for paths from home directory
(defun path-from-home (path)
  "Return the absolute path from the user's home directory."
  (expand-file-name path user-emacs-directory))

;; Backup and auto-save settings
(use-package files
  :ensure nil  ;; Mark as built-in
  :custom
  (make-backup-files t)  ;; Enable backup files
  (backup-by-copying t)  ;; Backup by copying
  (version-control t)  ;; Enable version control for backups
  (delete-old-versions t)  ;; Delete old versions
  (kept-old-versions 6)  ;; Number of old versions to keep
  (kept-new-versions 6)  ;; Number of new versions to keep
  (backup-directory-alist `(("." . ,(path-from-home "backups"))))  ;; Set backup directory
  :config
  ;; Ensure backup directory exists
  (unless (file-exists-p (path-from-home "backups"))
    (make-directory (path-from-home "backups") t)))  ;; Create backup directory if it doesn't exist


;; Better scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;;; Delete Selection Mode
(use-package delsel
  :config
  (delete-selection-mode 1))

;; Key Unbinding for avoiding frame suspension
(use-package frame
  :ensure nil  ;; Mark as built-in
  :config
  (unbind-key (kbd "C-z") global-map)      
  (unbind-key (kbd "C-x C-z") global-map))

;; Help Window Behavior
(use-package help
  :ensure nil  ;; Mark as built-in
  :custom
  (help-window-select t))  ;; Automatically select the help window after opening it

;; Narrowing to Page
(use-package page
  :ensure nil  ;; Mark as built-in
  :config
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil))  ;; Enable the 'narrow-to-page' function

;;; Easy Window Navigation
(use-package windmove
  :config
  (windmove-default-keybindings))

;; Unique Buffer Names
(use-package uniquify
  :ensure nil  ;; Mark as built-in
  :custom
  (uniquify-buffer-name-style 'forward)  ;; Use forward style for unique buffer names
  (uniquify-separator " › "))  ;; Set the separator for unique buffer names

;;; Recently Opened Files
(use-package recentf
  :custom
  (recentf-max-menu-items 50)
  (recentf-max-saved-items 50)
  (recentf-save-file (path-from-home "recentf"))
  :bind ("C-x C-r" . recentf-open-files)
  :init
  (recentf-mode 1))

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
        '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size))
  
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
