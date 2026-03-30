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
          (org . ("https://github.com/milisims/tree-sitter-org"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
          
          ;; Data Science file formats
          ;; CSV parser requires subdirectory path
          (csv . ("https://github.com/tree-sitter-grammars/tree-sitter-csv" "master" "csv/src"))
          
          ;; Data Science Languages (working well)
          (r . ("https://github.com/r-lib/tree-sitter-r"))
          (julia . ("https://github.com/tree-sitter/tree-sitter-julia"))
          (scala . ("https://github.com/tree-sitter/tree-sitter-scala"))
          
          ;; Document markup languages
          (typst . ("https://github.com/uben0/tree-sitter-typst"))
          ))

  :config
  ;; Function to install all languages specified in the source list
  (defun treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist))
          (success-count 0)
          (fail-count 0)
          (failed-langs '()))
      (dolist (lang languages)
        (condition-case err
            (progn
              (treesit-install-language-grammar lang)
              (setq success-count (1+ success-count))
              (message "✓ %s parser installed successfully" lang)
              (sit-for 0.5))
          (error
           (setq fail-count (1+ fail-count))
           (push lang failed-langs)
           (message "✗ %s parser failed: %s" lang (error-message-string err))
           (sit-for 0.5))))
      (message "\nInstallation complete: %d succeeded, %d failed" success-count fail-count)
      (when failed-langs
        (message "Failed parsers: %s" (mapconcat 'symbol-name (reverse failed-langs) ", ")))))

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
          
          ;; Data Science file formats with working parsers
          (csv-mode . csv-ts-mode)
          (toml-mode . toml-ts-mode)
          (markdown-mode . markdown-ts-mode)
          
          ;; Data Science languages with working parsers
          (r-mode . r-ts-mode)              ; R statistical computing
          (julia-mode . julia-ts-mode)      ; Julia high-performance
          (scala-mode . scala-ts-mode)      ; Scala big data
          )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Science File Format Support ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CSV with column alignment
(use-package csv-mode
  :ensure t
  :mode "\\.csv\\'"
  :config
  (setq csv-align-padding 1)
  (setq csv-separators '("," ";" "|" "\t")))

;; TOML configuration files
(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'")

;; YAML configuration files  
(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

;; MDX support (Markdown + JSX)
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.mdx\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Typst support (modern markup language for scientific documents)
(use-package typst-ts-mode
  :ensure t
  :mode "\\.typ\\'"
  :config
  ;; Use typst compiler if available
  (when (executable-find "typst")
    (setq typst-ts-mode-watch-options "--open")))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eglot Configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :hook (;; Auto-start for most languages
         (prog-mode . (lambda ()
                        ;; Don't auto-start for Python modes (they need env setup first)
                        (unless (derived-mode-p 'python-mode 'python-ts-mode)
                          (eglot-ensure)))))
  :config
  ;; Performance optimizations for large projects
  (setq eglot-events-buffer-size 0              ; Disable event logging (reduces memory)
        eglot-autoshutdown t                    ; Shutdown LSP server when last buffer is closed
        eglot-confirm-server-initiated-edits nil ; Don't ask for confirmation on LSP edits
        eglot-extend-to-xref t                  ; Extend LSP to xref functionality
        eglot-sync-connect nil                   ; Async connection (faster startup)
        eglot-connect-timeout 10                 ; 10s timeout for LSP connection
        eglot-send-changes-idle-time 0.5)        ; Debounce: send changes after 0.5s idle
  
  ;; Disable intrusive features for better performance
  (setq eglot-ignored-server-capabilities
        '(:documentOnTypeFormattingProvider      ; Don't format while typing
          :documentHighlightProvider))           ; Don't highlight all occurrences (can be slow)

  ;; Python LSP with Ty (Rust-based, replaces Pyright)
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) .
                 ("ty" "server")))

  ;; JavaScript/TypeScript LSP support (optional)
  (when (executable-find "typescript-language-server")
    (add-to-list 'eglot-server-programs
                 '((js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode) .
                   ("typescript-language-server" "--stdio"))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Ruff Integration (Formatter, Lint) ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Manual format function (synchronous, for explicit calls)
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

  ;; Manual check function
  (defun my/ruff-check-buffer ()
    "Check Python buffer with Ruff."
    (interactive)
    (when (and (derived-mode-p 'python-mode 'python-ts-mode)
               (executable-find "ruff"))
      (shell-command
       (format "ruff check %s" (shell-quote-argument (buffer-file-name))))))

  ;; Async Flymake backend for Ruff linting
  (defun my/ruff-flymake-backend (report-fn &rest _args)
    "Ruff Flymake backend for asynchronous linting."
    (when (and (buffer-file-name)
               (derived-mode-p 'python-mode 'python-ts-mode)
               (executable-find "ruff"))
      (let ((source-buffer (current-buffer)))
        (save-restriction
          (widen)
          (let ((proc (make-process
                       :name "ruff-flymake"
                       :buffer (generate-new-buffer " *ruff-flymake*")
                       :command (list "ruff" "check" "--output-format" "json" (buffer-file-name))
                       :connection-type 'pipe
                       :sentinel
                       (lambda (proc _event)
                         (when (eq (process-status proc) 'exit)
                           (unwind-protect
                               (if (with-current-buffer source-buffer (eq proc (process-get proc 'flymake-process)))
                                   (with-current-buffer (process-buffer proc)
                                     (goto-char (point-min))
                                     (let ((diags '()))
                                       (condition-case err
                                           (when-let ((json-data (ignore-errors (json-read))))
                                             (dolist (item (if (vectorp json-data) (append json-data nil) (list json-data)))
                                               (when-let* ((location (alist-get 'location item))
                                                           (row (alist-get 'row location))
                                                           (col (alist-get 'column location))
                                                           (message (alist-get 'message item))
                                                           (code (alist-get 'code item)))
                                                 (with-current-buffer source-buffer
                                                   (save-excursion
                                                     (goto-char (point-min))
                                                     (forward-line (1- row))
                                                     (move-to-column (1- col))
                                                     (let* ((beg (point))
                                                            (end (min (line-end-position) (+ beg 10))))
                                                       (push (flymake-make-diagnostic
                                                              source-buffer
                                                              beg end
                                                              :warning
                                                              (format "[%s] %s" code message))
                                                             diags)))))))
                                         (error nil))
                                       (funcall report-fn diags)))
                                 (flymake-log :warning "Canceling obsolete check %s" proc))
                             (kill-buffer (process-buffer proc))))))))
            (process-put proc 'flymake-process proc))))))

  ;; Auto-format on save (optional, configurable)
  (defun my/ruff-format-on-save ()
    "Format buffer with Ruff before saving."
    (when (and (derived-mode-p 'python-mode 'python-ts-mode)
               (executable-find "ruff"))
      (my/ruff-format-buffer)))

  ;; Register Ruff Flymake backend and format-on-save for Python modes
  (add-hook 'python-mode-hook
            (lambda ()
              (when (executable-find "ruff")
                (add-hook 'flymake-diagnostic-functions #'my/ruff-flymake-backend nil t)
                (add-hook 'before-save-hook #'my/ruff-format-on-save nil t))))

  (add-hook 'python-ts-mode-hook
            (lambda ()
              (when (executable-find "ruff")
                (add-hook 'flymake-diagnostic-functions #'my/ruff-flymake-backend nil t)
                (add-hook 'before-save-hook #'my/ruff-format-on-save nil t))))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Virtual Environment Management ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Pyvenv: Visual virtual environment management
(use-package pyvenv
  :ensure t
  :defer t
  :commands (pyvenv-activate pyvenv-workon pyvenv-deactivate)
  :init
  ;; Display current venv in modeline
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  
  :config
  ;; Workon home for virtualenvwrapper compatibility
  (when (getenv "WORKON_HOME")
    (setq pyvenv-workon-home (getenv "WORKON_HOME")))
  
  ;; Hook to restart LSP when venv changes
  (add-hook 'pyvenv-post-activate-hooks
            (lambda ()
              (when (bound-and-true-p eglot--managed-mode)
                (eglot-reconnect (eglot--current-server-or-lose)))))
  
  (add-hook 'pyvenv-post-deactivate-hooks
            (lambda ()
              (when (bound-and-true-p eglot--managed-mode)
                (eglot-reconnect (eglot--current-server-or-lose))))))

;; Enable pyvenv-mode globally (lightweight, only adds modeline indicator)
(add-hook 'python-mode-hook #'pyvenv-mode)
(add-hook 'python-ts-mode-hook #'pyvenv-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jupyter/Code Cells Integration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; code-cells: Jupyter-like cell execution in Python files
;; Use # %% to delimit cells (compatible with VS Code, Jupyter, Spyder)
(use-package code-cells
  :ensure t
  :defer t
  :hook ((python-mode python-ts-mode) . code-cells-mode-maybe)
  :config
  ;; Keybindings for cell navigation and execution
  (defun my/code-cells-setup ()
    "Set up code-cells keybindings."
    (local-set-key (kbd "C-c %") 'code-cells-command)
    (local-set-key (kbd "C-c C-n") 'code-cells-forward-cell)
    (local-set-key (kbd "C-c C-p") 'code-cells-backward-cell)
    (local-set-key (kbd "C-c C-e") 'code-cells-eval)
    (local-set-key (kbd "C-c C-SPC") 'code-cells-mark-cell))
  
  (add-hook 'code-cells-mode-hook #'my/code-cells-setup))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pytest Integration    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; python-pytest: Interactive test runner for pytest
(use-package python-pytest
  :ensure t
  :defer t
  :commands (python-pytest-dispatch
             python-pytest-function
             python-pytest-file
             python-pytest-last-failed
             python-pytest-repeat)
  :init
  ;; Keybindings for pytest
  (define-prefix-command 'python-test-map nil "Python Tests")
  
  :bind (:map python-mode-map
              ("C-c t" . python-test-map)
              ("C-c t t" . python-pytest-dispatch)
              ("C-c t f" . python-pytest-function)
              ("C-c t m" . python-pytest-file)
              ("C-c t a" . python-pytest)
              ("C-c t l" . python-pytest-last-failed)
              ("C-c t r" . python-pytest-repeat))
  :bind (:map python-ts-mode-map
              ("C-c t" . python-test-map)
              ("C-c t t" . python-pytest-dispatch)
              ("C-c t f" . python-pytest-function)
              ("C-c t m" . python-pytest-file)
              ("C-c t a" . python-pytest)
              ("C-c t l" . python-pytest-last-failed)
              ("C-c t r" . python-pytest-repeat))
  
  :config
  ;; Use project-specific pytest if available
  (setq python-pytest-executable "pytest")
  
  ;; Colorized output
  (when (executable-find "pytest")
    (add-to-list 'python-pytest-arguments "--color=yes")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python Debugger (pdb)        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Built-in Python debugger support
;; Use pdb (Python debugger) - no external dependencies needed
;; 
;; Usage:
;;   1. Add breakpoint in code: import pdb; pdb.set_trace()
;;   2. Run script: M-x compile RET python script.py
;;   3. When breakpoint hits, use pdb commands in compilation buffer
;;
;; Common pdb commands:
;;   n (next)      - Execute current line
;;   s (step)      - Step into function
;;   c (continue)  - Continue until next breakpoint
;;   p variable    - Print variable value
;;   l (list)      - Show current location in code
;;   q (quit)      - Exit debugger
;;
;; For visual debugging with DAP, see PYTHON_ADVANCED.md alternative tools section


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive Python REPL Configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enhanced Python REPL with IPython support
(use-package python
  :ensure nil
  :defer t
  :config
  ;; Use IPython if available, fallback to python
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "--simple-prompt -i --no-banner"
          python-shell-prompt-regexp "In \\[[0-9]+\\]: "
          python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
          python-shell-completion-setup-code
          "from IPython.core.completerlib import module_completion"
          python-shell-completion-module-string-code
          "';'.join(module_completion('''%s'''))\n"
          python-shell-completion-string-code
          "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))
  
  ;; Better indentation
  (setq python-indent-guess-indent-offset-verbose nil)
  
  ;; Don't highlight indentation errors (handled by Ruff)
  (setq python-indent-def-block-scale 1)
  
  ;; Enable shell completion
  (setq python-shell-completion-native-enable t))

;; Custom helper functions for Python REPL
(with-eval-after-load 'python
  ;; Custom popup function for Python REPL
  (defun my/python-shell-send-and-show (start end)
    "Send region to Python shell and briefly show output."
    (interactive "r")
    (python-shell-send-region start end)
    (let ((repl-window (get-buffer-window (python-shell-get-buffer) t)))
      (when repl-window
        (with-selected-window repl-window
          (goto-char (point-max))))))
  
  (defun my/python-shell-send-buffer-and-show ()
    "Send entire buffer to Python shell."
    (interactive)
    (python-shell-send-buffer)
    (message "Buffer sent to Python shell"))
  
  (defun my/python-shell-send-defun-and-show ()
    "Send current function to Python shell."
    (interactive)
    (python-shell-send-defun)
    (message "Function sent to Python shell"))
  
  ;; Keybindings for REPL interaction
  (define-key python-mode-map (kbd "C-c C-z") 'python-shell-switch-to-shell)
  (define-key python-mode-map (kbd "C-c C-c") 'my/python-shell-send-buffer-and-show)
  (define-key python-mode-map (kbd "C-c C-r") 'my/python-shell-send-and-show)
  (define-key python-mode-map (kbd "C-c C-d") 'my/python-shell-send-defun-and-show)
  (define-key python-mode-map (kbd "C-c C-l") 'python-shell-send-file)
  
  (define-key python-ts-mode-map (kbd "C-c C-z") 'python-shell-switch-to-shell)
  (define-key python-ts-mode-map (kbd "C-c C-c") 'my/python-shell-send-buffer-and-show)
  (define-key python-ts-mode-map (kbd "C-c C-r") 'my/python-shell-send-and-show)
  (define-key python-ts-mode-map (kbd "C-c C-d") 'my/python-shell-send-defun-and-show)
  (define-key python-ts-mode-map (kbd "C-c C-l") 'python-shell-send-file))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python Environment Detection   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/detect-python-env ()
  "Detect and configure Python virtual environment (uv-aware).
Detects local .venv directories, activates with pyvenv, adjusts PATH
to prioritize local tools (ty, ruff, python), and warns if tools are
missing. Starts Eglot after environment is configured."
  (when-let* ((project-root (or (project-root (project-current))
                                default-directory))
              (venv-path (expand-file-name ".venv" project-root)))
    (when (file-directory-p venv-path)
      (let* ((venv-bin (expand-file-name "bin" venv-path))
             (ty-path (expand-file-name "ty" venv-bin))
             (ruff-path (expand-file-name "ruff" venv-bin))
             (python-path (expand-file-name "python" venv-bin)))
        
        ;; Activate venv with pyvenv (updates modeline, VIRTUAL_ENV, etc.)
        (pyvenv-activate venv-path)
        
        ;; Prepend .venv/bin to PATH so Eglot finds local tools
        (setenv "PATH" (concat venv-bin path-separator (getenv "PATH")))
        
        ;; Update exec-path for Emacs subprocess calls
        (add-to-list 'exec-path venv-bin)
        
        ;; Check and warn about missing tools
        (cond
         ((not (file-executable-p ty-path))
          (message "Warning: 'ty' not found in .venv. Install with: uv pip install ty"))
         ((not (file-executable-p ruff-path))
          (message "Warning: 'ruff' not found in .venv. Install with: uv pip install ruff"))
         (t
          (message "Using local Python environment: %s (ty + ruff detected)" venv-path)))
        
        ;; Set Python interpreter path
        (when (file-executable-p python-path)
          (setq-local python-shell-interpreter python-path))
        
        ;; Start Eglot now that environment is configured
        (eglot-ensure)))))

;; Register environment detection hooks for Python modes
(add-hook 'python-mode-hook #'my/detect-python-env)
(add-hook 'python-ts-mode-hook #'my/detect-python-env)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper: Init Python Project ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/init-python-project ()
  "Initialize Python project with uv, ty, and ruff.
Creates a .venv directory and installs development tools locally."
  (interactive)
  (let ((project-root (or (project-root (project-current))
                         default-directory)))
    (when (yes-or-no-p (format "Initialize Python project in %s? " project-root))
      (async-shell-command
       (concat "cd " (shell-quote-argument project-root) " && "
               "uv venv && "
               "source .venv/bin/activate && "
               "uv pip install ty ruff && "
               "echo '=== Project initialized with ty and ruff ===' && "
               "echo 'Reload your Python buffers to detect new tools'")
       "*Python Project Init*"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Racket Configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package paredit
  :ensure t)

;; (use-package racket-mode
;;   :ensure t
;;   :defer t
;;   :hook ((racket-mode . paredit-mode)))
(use-package racket-mode
  :ensure t
  :defer t)


(setq racket-program "/home/jesusf10/.config/racket/bin/racket")  ;; or /usr/bin/racket
(setq racket-command "/home/jesusf10/.config/racket/bin/racket")  ;; alias, used in some versions
(setq racket-raco-program "/home/jesusf10/.config/racket/bin/raco")

(add-to-list 'auto-mode-alist '("\\.scm\\'" . racket-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP Helper Bindings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(define-prefix-command 'flymake-map nil "Flymake")
(define-key lsp-map (kbd "m") 'flymake-map)

;; Python venv management
(define-prefix-command 'python-venv-map nil "Python Venv")
(define-key lsp-map (kbd "v") 'python-venv-map)
(bind-key (kbd "a") #'pyvenv-activate 'python-venv-map)
(bind-key (kbd "d") #'pyvenv-deactivate 'python-venv-map)
(bind-key (kbd "w") #'pyvenv-workon 'python-venv-map)

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
