;;; lsp.el --- LSP and programming language support -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains LSP configuration and programming language support.
;; Tree-sitter parsers tested with Emacs 29.3 - some may have version compatibility issues.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree-sitter Configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(use-package treesit
  :ensure nil
  :commands (treesit-install-language-grammar treesit-install-all-languages)
  :init
  ;; Language sources - tested with Emacs 29.3
  ;; Note: Some parsers may have version mismatches and require specific commits
  (setq treesit-language-source-alist
        '(;; Core languages (working well)
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (java . ("https://github.com/tree-sitter/tree-sitter-java"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
          (make . ("https://github.com/alemuller/tree-sitter-make"))
          (markdown . ("https://github.com/ikatyang/tree-sitter-markdown"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (latex . ("https://github.com/latex-lsp/tree-sitter-latex"))
          (org . ("https://github.com/milisims/tree-sitter-org"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
          
          ;; Data Science Languages (working well)
          (r . ("https://github.com/r-lib/tree-sitter-r"))                    ; Statistics, ggplot2, tidyverse
          (julia . ("https://github.com/tree-sitter/tree-sitter-julia"))      ; High-performance computing, ML.jl
          (scala . ("https://github.com/tree-sitter/tree-sitter-scala"))      ; Apache Spark, big data            ; reStructuredText documentation
          ))

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

  ;; Function to install specific language
  (defun treesit-install-language (language)
    "Install Tree-sitter grammar for LANGUAGE."
    (interactive
     (list (intern (completing-read "Language: "
                                    (mapcar 'car treesit-language-source-alist)))))
    (treesit-install-language-grammar language))

  ;; Function to clean up and reinstall problematic parsers
  (defun treesit-clean-and-reinstall (language)
    "Clean up and reinstall Tree-sitter grammar for LANGUAGE."
    (interactive
     (list (intern (completing-read "Language to reinstall: "
                                    (mapcar 'car treesit-language-source-alist)))))
    (let ((library-file (expand-file-name
                         (format "libtree-sitter-%s.so" language)
                         "~/.emacs.d/tree-sitter/")))
      (when (file-exists-p library-file)
        (delete-file library-file)
        (message "Deleted old library for %s" language))
      (treesit-install-language-grammar language)
      (message "Reinstalled %s parser" language)))

  ;; Function to check parser status
  (defun treesit-check-parsers ()
    "Check status of installed Tree-sitter parsers."
    (interactive)
    (let ((results '()))
      (dolist (lang (mapcar 'car treesit-language-source-alist))
        (condition-case err
            (if (treesit-language-available-p lang)
                (push (format "✓ %s: Working" lang) results)
              (push (format "✗ %s: Not available" lang) results))
          (error
           (push (format "✗ %s: Error - %s" lang (error-message-string err)) results))))
      (with-current-buffer (get-buffer-create "*Tree-sitter Status*")
        (erase-buffer)
        (insert "Tree-sitter Parser Status:\n\n")
        (dolist (result (reverse results))
          (insert result "\n"))
        (goto-char (point-min))
        (display-buffer (current-buffer)))))

  ;; Enable Tree-sitter modes automatically for working parsers
  (setq major-mode-remap-alist
        '(;; Core languages with working parsers
          (c++-mode . c++-ts-mode)
          (css-mode . css-ts-mode)
          (java-mode . java-ts-mode)
          (javascript-mode . js-ts-mode)
          (json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (yaml-mode . yaml-ts-mode)
          
          ;; Data Science languages with working parsers
          (r-mode . r-ts-mode)              ; R statistical computing
          (julia-mode . julia-ts-mode)      ; Julia high-performance
          (scala-mode . scala-ts-mode)      ; Scala big data
          )))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eglot Configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  :ensure nil
  :hook (prog-mode . eglot-ensure)
  :config
  (setq eglot-autoshutdown t                    ; Shutdown LSP server when last buffer is closed
        eglot-confirm-server-initiated-edits nil ; Don't ask for confirmation on LSP edits
        eglot-extend-to-xref t                  ; Extend LSP to xref functionality
        eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))

  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) .
                 ("pyright-langserver" "--stdio")))

  ;; JavaScript/TypeScript LSP support (optional)
  (when (executable-find "typescript-language-server")
    (add-to-list 'eglot-server-programs
                 '((js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode) .
                   ("typescript-language-server" "--stdio"))))
                   
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Ruff Integration (Formatter, Lint) ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (executable-find "ruff")
  (defun my/ruff-format-buffer ()
    "Format Python buffer with Ruff if available."
    (interactive)
    (when (and (derived-mode-p 'python-mode 'python-ts-mode)
               (executable-find "ruff"))
      (let ((original-point (point)))
        (condition-case err
            (progn
              (shell-command-on-region
               (point-min) (point-max)
               "ruff format -"
               (current-buffer) t)
              (goto-char original-point)
              (message "Buffer formatted with Ruff"))
          (error (message "Ruff formatting failed: %s" (error-message-string err)))))))

  (defun my/ruff-check-buffer ()
    "Check Python buffer with Ruff."
    (interactive)
    (when (and (derived-mode-p 'python-mode 'python-ts-mode)
               (executable-find "ruff"))
      (shell-command
       (format "ruff check %s" (shell-quote-argument (buffer-file-name))))))
  )
)


;;;;;;;;;;;;;;;;;
;; Python Envs ;;
;;;;;;;;;;;;;;;;;

(defun my/detect-python-venv ()
  "Detect and set Python virtual environment for LSP."
  (when-let* ((project-root (or (project-root (project-current))
                                default-directory))
              (venv-path (expand-file-name ".venv" project-root)))
    (when (file-directory-p venv-path)
      (setenv "VIRTUAL_ENV" venv-path)
      (let ((python-path (expand-file-name "bin/python" venv-path)))
        (when (file-executable-p python-path)
          (setenv "PYTHON_PATH" python-path)
          (message "Detected Python venv: %s" venv-path))))))

(add-hook 'python-mode-hook #'my/detect-python-venv)
(add-hook 'python-ts-mode-hook #'my/detect-python-venv)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Racket Configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package paredit
  :ensure t)

(use-package racket-mode
  :ensure t
  :defer t
  :hook ((racket-mode . paredit-mode))
  :bind (lsp-map)
  ())


;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP Helper Bindings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(define-prefix-command 'flymake-map nil "Flymake")
(define-key lsp-map (kbd "m") 'flymake-map)

;; Ruff manual
(bind-key (kbd "C-c l F") #'my/ruff-format-buffer)
(bind-key (kbd "C-c l L") #'my/ruff-check-buffer)

(bind-key (kbd "s") 'eglot 'lsp-map)
(bind-key (kbd "r") 'eglot-rename 'lsp-map)
(bind-key (kbd "a") 'eglot-code-actions 'lsp-map)
(bind-key (kbd "f") 'eglot-format 'lsp-map)
(bind-key (kbd "R") 'eglot-reconnect 'lsp-map)
(bind-key (kbd "S") 'eglot-shutdown 'lsp-map)

;; Debugging and troubleshooting
(bind-key (kbd "d e") 'eglot-events-buffer 'lsp-map)
(bind-key (kbd "d l") 'eglot-stderr-buffer 'lsp-map)
(bind-key (kbd "b") 'my/flymake-check-backends 'flymake-map)
(bind-key (kbd "R") 'my/flymake-reset-backends 'flymake-map)

;; Flymake diagnostic keybindings
(bind-key (kbd "e") 'flymake-show-project-diagnostics 'flymake-map)
(bind-key (kbd "n") 'flymake-goto-next-error 'flymake-map)
(bind-key (kbd "p") 'flymake-goto-prev-error 'flymake-map)
(bind-key (kbd "l") 'flymake-show-buffer-diagnostics 'flymake-map)
(bind-key (kbd "r") 'flymake-running-backends 'flymake-map)


;;; lsp.el ends here
