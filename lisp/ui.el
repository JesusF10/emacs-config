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
  (load-theme 'doom-palenight t)
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
  :if (display-graphic-p))

;; Better default font (if available)
(when (display-graphic-p)
  (cond
   ;; Try different fonts in order of preference
   ((find-font (font-spec :name "JetBrains Mono"))
    (set-face-attribute 'default nil :font "JetBrains Mono-12"))
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

;; Highlight indentation guides
(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-character ?│)
  (highlight-indent-guides-responsive 'top))


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
  :config
  (smooth-scrolling-mode 1)
  :custom
  (smooth-scroll-margin 2))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aditional settings: ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Document Viewing (PDFs, Images)
(use-package doc-view
  :when (display-graphic-p)
  :custom
  (doc-view-resolution 200))


;;; ui.el ends here
