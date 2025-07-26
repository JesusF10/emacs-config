;;; editor.el --- Editor enhancements and settings -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains editor enhancements, text editing improvements,
;; and general editing behavior configurations.


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic editor settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default indent-tabs-mode nil)
(setq sentence-end-double-space nil)
(setq require-final-newline t)
(setq tab-always-indent 'complete)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Helper function for paths from home directory
(defun path-from-home (path)
  "Return the absolute path from the user's home directory."
  (expand-file-name path user-emacs-directory))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backup & Autosave Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;
;; Window Settings ;;
;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;
;; Buffer Settings ;;
;;;;;;;;;;;;;;;;;;;;;

;; Unique Buffer Names
(use-package uniquify
  :ensure nil  ;; Mark as built-in
  :custom
  (uniquify-buffer-name-style 'forward)  ;; Use forward style for unique buffer names
  (uniquify-separator " › "))  ;; Set the separator for unique buffer names

;; Auto-revert buffers when files change on disk
(global-auto-revert-mode 1)

;; Use package for eldoc-box
;(use-package eldoc-box
;  :ensure t
;  :hook (eldoc-mode . eldoc-box-hover-mode) ;; Enable eldoc-box in eldoc mode
;  :config
;  (setq eldoc-box-border-color "black"))  ;; Set the border color (optional)


;;;;;;;;;;;;;;;;;;
;; Recent Files ;;
;;;;;;;;;;;;;;;;;;

;;; Recently Opened Files
(use-package recentf
  :custom
  (recentf-max-menu-items 50)
  (recentf-max-saved-items 50)
  (recentf-save-file (path-from-home "recentf"))
  :bind ("C-x C-r" . recentf-open-files)
  :init
  (recentf-mode 1))

;; Save place in files
(save-place-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybinding Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Which-key mode for better keybinding discovery
(use-package which-key
  :ensure t
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.3)
  (setq which-key-popup-type 'side-window))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding & Matching Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Vertico: A performant and minimalistic vertical completion UI
(use-package vertico
  :ensure t
  :config
  (vertico-mode 1)
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
  :config
  (marginalia-mode 1))

;; Consult: Consulting completing-read
(use-package consult
  :ensure t
  :bind (:map editor-map
              ("l" . consult-line)
              ("G" . consult-git-grep)
              ("g" . consult-grep)))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File Explorer Config ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  :config
  (dirvish-override-dired-mode 1)
  ;; Basic dirvish settings
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  
  ;; Configure attributes (visual enhancements)
  (setq dirvish-attributes
        '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size))

  :bind
  (:map visual-map
   ("f" . dirvish)
   ("s" . dirvish-side))
  (:map dirvish-mode-map
   ;; Vim-style navigation
   ("j" . dired-next-line)
   ("k" . dired-previous-line)
   ("h" . dired-up-directory)
   ("l" . dired-find-file)
   ;; Additional useful bindings
   ("?" . dirvish-dispatch)     ; Help menu
   ("q" . quit-window)))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Pair Mode Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Electric pair mode for automatic bracket pairing
(electric-pair-mode 1)

;;; Automatic Pair Insertion:
(defun custom-electric-pair-pairs ()
  "Define custom pairs for specific modes."
  (setq-local electric-pair-pairs '((?\{ . ?\})
                                    (?\[ . ?\])
                                    (?\( . ?\))
                                    (?\" . ?\")
                                    (?\` . ?\`))))

(add-hook 'prog-mode-hook #'custom-electric-pair-pairs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selection Mode Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Delete Selection Mode
(use-package delsel
  :config
  (delete-selection-mode 1))


;;;;;;;;;;;;;;;;;;;;;
;; Editor Settings ;;
;;;;;;;;;;;;;;;;;;;;;

(use-package crux
  :ensure t
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ("C-o" . crux-smart-open-line)        ;; Open line below
         ))

;(use-package company
;  :ensure t
;  :delight
;  :custom
;  (company-idle-delay 0.0)
;  (company-tooltip-idle-delay 0.0)
;  (setq company-minimum-prefix-length 2)
;  (setq company-selection-wrap-around t)
;  (setq company-backends '((company-files company-yasnippet company-capf)))
;  :hook (prog-mode . company-mode)
;  :config
;  (bind-key (kbd "o") 'company-complete 'omni-map))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aditional settings: ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helpful
  :ensure t
  :bind (:map global-map
              ("C-h f" . helpful-callable)
              ("C-h v" . helpful-variable)
              ("C-h k" . helpful-key)
              ("C-h ." . helpful-at-point)
              ("C-h F" . helpful-function)
              ("C-h C" . helpful-command)))


;;; editor.el ends here
