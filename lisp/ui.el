;;; ui.el --- UI and appearance configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains UI customizations, themes, fonts, and visual settings.


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Customization ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Basic UI improvements
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; Show line numbers
(global-display-line-numbers-mode 1)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Better defaults
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(setq use-dialog-box nil)

;; Show matching parentheses
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Themes Customization ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package doom-themes
  :ensure t
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; for treemacs users
  ;; (doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  :config
  (load-theme 'doom-material-dark t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Icons Customization ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Font configuration
(use-package nerd-icons
  :ensure t
  :if (display-graphic-p)
  :config
  ;; Install fonts if not already installed
  ;; Run M-x nerd-icons-install-fonts manually if icons don't show
  (unless (member "Symbols Nerd Font Mono" (font-family-list))
    (message "Nerd icons fonts not installed. Run M-x nerd-icons-install-fonts")))

;; Configure Flymake to use better icons in the fringe
(use-package flymake
  :ensure nil
  :config
  (when (display-graphic-p)
    ;; Use simple, visible fringe bitmaps for errors/warnings
    (define-fringe-bitmap 'my/flymake-error-fringe
      [#b00000000
       #b00111100
       #b01111110
       #b11111111
       #b11111111
       #b01111110
       #b00111100
       #b00000000])
    
    (define-fringe-bitmap 'my/flymake-warning-fringe
      [#b00011000
       #b00111100
       #b00111100
       #b01111110
       #b01111110
       #b11111111
       #b11111111
       #b00000000])
    
    (define-fringe-bitmap 'my/flymake-note-fringe
      [#b00000000
       #b00111100
       #b01111110
       #b01111110
       #b00111100
       #b00011000
       #b00011000
       #b00000000])
    
    ;; Apply to flymake
    (setq flymake-error-bitmap '(my/flymake-error-fringe compilation-error))
    (setq flymake-warning-bitmap '(my/flymake-warning-fringe compilation-warning))
    (setq flymake-note-bitmap '(my/flymake-note-fringe compilation-info))))

;; Better default font (if available)
(when (display-graphic-p)
  (cond
   ;; Try different fonts in order of preference
   ;; ((find-font (font-spec :name "JetBrains Mono"))
   ;;  (set-face-attribute 'default nil :font "JetBrains Mono-12"))
   ((find-font (font-spec :name "Fira Code"))
    (set-face-attribute 'default nil :font "Fira Code-12"))
   ((find-font (font-spec :name "Source Code Pro"))
    (set-face-attribute 'default nil :font "Source Code Pro-12"))
   ((find-font (font-spec :name "Consolas"))
    (set-face-attribute 'default nil :font "Consolas-12"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delimeters Customization ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Rainbow delimiters for better bracket visibility
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))


;;;;;;;;;;;;;;;;;
;; Color Codes ;;
;;;;;;;;;;;;;;;;;

;; Show colors for color codes
(use-package rainbow-mode
  :ensure t
  :hook (css-mode . rainbow-mode))

;; Beacon - highlight cursor position
(use-package beacon
  :ensure t
  :defer 1  ; Load 1 second after startup (purely visual effect)
  :config
  (beacon-mode 1)
  :custom
  (beacon-size 20)
  (beacon-blink-when-point-moves-vertically 10))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modeline Customization ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Doom modeline configuration
(use-package doom-modeline
  :ensure t
  :if (display-graphic-p)
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-support-imenu t)
  (doom-modeline-height 30)
  (doom-modeline-bar-width 5)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-env-version t)
  (doom-modeline-indent-info t)
  (doom-modeline-checker-simple t)
  (doom-modeline-vcs-max-length 15))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scrolling Customization ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Smooth scrolling
(use-package smooth-scrolling
  :ensure t
  :defer 1  ; Load 1 second after startup (visual enhancement)
  :config
  (smooth-scrolling-mode 1)
  :custom
  (smooth-scroll-margin 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git Integration Visual   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Git gutter - show git diff in fringe
(use-package git-gutter
  :ensure t
  :if (display-graphic-p)
  :defer 2  ; Load 2 seconds after startup (purely visual, not critical)
  :config
  (global-git-gutter-mode +1)
  :custom
  (git-gutter:update-interval 2)
  (git-gutter:modified-sign "│")
  (git-gutter:added-sign "│")
  (git-gutter:deleted-sign "│")
  :custom-face
  (git-gutter:modified ((t (:background "#e5c07b" :foreground "#e5c07b"))))
  (git-gutter:added ((t (:background "#98c379" :foreground "#98c379"))))
  (git-gutter:deleted ((t (:background "#e06c75" :foreground "#e06c75")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation Visual Guides ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Indent guides - show indentation levels
(use-package indent-guide
  :ensure t
  :defer t  ; Load only when prog-mode is activated
  :hook (prog-mode . indent-guide-mode)
  :custom
  (indent-guide-char "│")
  (indent-guide-delay 0.3)
  :custom-face
  (indent-guide-face ((t (:foreground "#3e4451" :slant normal)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Transparency       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Window transparency (95% active, 90% inactive)
(when (display-graphic-p)
  (set-frame-parameter (selected-frame) 'alpha '(98 . 90))
  (add-to-list 'default-frame-alist '(alpha . (98 . 90))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion Icons          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Nerd icons in completion (Vertico/Consult)
(use-package nerd-icons-completion
  :ensure t
  :defer 1  ; Load 1 second after startup (visual enhancement)
  :after (marginalia nerd-icons)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dashboard (Start Screen)  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Dashboard - modern startup screen
(use-package dashboard
  :ensure t
  :init
  ;; Check if Emacs was started with file arguments
  (defun my/emacs-started-with-file-p ()
    "Return t if Emacs was started with a file argument."
    (seq-some #'file-exists-p (cdr command-line-args)))
  
  :config
  ;; Only setup dashboard if no file arguments were passed
  ;; This prevents the split window issue when opening files from terminal
  (unless (my/emacs-started-with-file-p)
    (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
    (dashboard-setup-startup-hook))
  :custom
  (dashboard-banner-logo-title "Welcome to Emacs")
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-items '((recents  . 5)
                     (projects . 5)
                     (bookmarks . 3)))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-navigator t)
  (dashboard-navigator-buttons
   `(((,(nerd-icons-faicon "nf-fa-github" :height 1.1 :v-adjust 0.0)
       "GitHub"
       "Browse GitHub"
       (lambda (&rest _) (browse-url "https://github.com/JesusF10/emacs-config")))
      (,(nerd-icons-faicon "nf-fa-refresh" :height 1.1 :v-adjust 0.0)
       "Update"
       "Update packages"
       (lambda (&rest _) (elpaca-update-all)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aditional settings: ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Document Viewing (PDFs, Images)
(use-package doc-view
  :when (display-graphic-p)
  :custom
  (doc-view-resolution 200))


;;; ui.el ends here
