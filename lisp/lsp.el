;;; lsp.el --- LSP and programming language support -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains LSP configuration and programming language support.
;; Uncomment and customize the packages you need.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree-sitter Configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package treesit
  :ensure nil
  :commands (treesit-install-language-grammar treesit-install-all-languages)
  :init
  ;; Define language sources for Tree-sitter
  (setq treesit-language-source-alist
        '((c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))))) 
  :config
  ;; Function to install all languages specified in the source list
  (defun treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
        (treesit-install-language-grammar lang)
        (message "`%s' parser was installed." lang)
        (sit-for 0.75))))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eglot Configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'prog-mode-hook #'eglot-ensure)

(bind-key (kbd "s") 'eglot 'lsp-map)
(bind-key (kbd "r") 'eglot-rename 'lsp-map)
(bind-key (kbd "a") 'eglot-code-actions 'lsp-map)
(bind-key (kbd "f") 'eglot-format 'lsp-map)
(bind-key (kbd "e e") 'flymake-show-project-diagnostics 'lsp-map)
(bind-key (kbd "e n") 'flymake-goto-next-error 'lsp-map)
(bind-key (kbd "e p") 'flymake-goto-prev-error 'lsp-map)


;;; lsp.el ends here
