;;; lsp.el --- LSP and programming language support -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains LSP configuration and programming language support.
;; Uncomment and customize the packages you need.

;;; Code:

;; Example LSP configuration (uncomment to use)
;; (use-package lsp-mode
;;   :ensure t
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook ((python-mode . lsp)
;;          (js-mode . lsp)
;;          (typescript-mode . lsp)
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp)

;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode)

;; (use-package company
;;   :ensure t
;;   :hook (after-init . global-company-mode)
;;   :config
;;   (setq company-idle-delay 0.1
;;         company-minimum-prefix-length 1))

;;; lsp.el ends here