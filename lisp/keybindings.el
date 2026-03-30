;;; keybindings.el --- Custom keybindings configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains custom keybindings and key remappings.


;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliar Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun reload-config ()
  "Reload the Emacs configuration."
  (interactive)
  (load-file user-init-file))

(defun open-config-file ()
  "Open the main configuration file."
  (interactive)
  (find-file user-init-file))
 
(defun open-config-dir ()
  "Open the configuration directory in dired."
  (interactive)
  (dired user-emacs-directory))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pandoc Integration  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pandoc-convert (from-format to-format)
  "Convert current buffer using Pandoc FROM-FORMAT to TO-FORMAT."
  (interactive
   (list
    (completing-read "From format: " 
                     '("markdown" "org" "latex" "html" "docx" "rst" "typst"))
    (completing-read "To format: " 
                     '("pdf" "html" "docx" "latex" "markdown" "org" "rst" "typst"))))
  (if (not (executable-find "pandoc"))
      (message "Pandoc not found. Install with: pacman -S pandoc")
    (let* ((input-file (buffer-file-name))
           (output-file (concat (file-name-sans-extension input-file) "." to-format))
           (command (format "pandoc -f %s -t %s -o %s %s"
                            from-format to-format
                            (shell-quote-argument output-file)
                            (shell-quote-argument input-file))))
      (if (not input-file)
          (message "Buffer is not visiting a file")
        (message "Running: %s" command)
        (shell-command command)
        (message "Converted to: %s" output-file)))))

(defun pandoc-markdown-to-pdf ()
  "Convert current Markdown file to PDF using Pandoc."
  (interactive)
  (pandoc-convert "markdown" "pdf"))

(defun pandoc-org-to-pdf ()
  "Convert current Org file to PDF using Pandoc."
  (interactive)
  (pandoc-convert "org" "pdf"))

(defun pandoc-typst-to-pdf ()
  "Convert current Typst file to PDF using Pandoc."
  (interactive)
  (pandoc-convert "typst" "pdf"))

(defun pandoc-markdown-to-docx ()
  "Convert current Markdown file to DOCX using Pandoc."
  (interactive)
  (pandoc-convert "markdown" "docx"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manual Mapping Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-prefix-command 'editor-map nil "Editor")
(define-prefix-command 'visual-map nil "Visual")
(define-prefix-command 'omni-map  nil "Omni")
(define-prefix-command 'lsp-map nil "LSP")


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Binding Configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-key "C-c e" 'editor-map)
(bind-key "C-c v" 'visual-map)
(bind-key "C-c o" 'omni-map)
(bind-key "C-c l" 'lsp-map)
 
(bind-key (kbd "r") 'reload-config 'editor-map)
(bind-key (kbd "i") 'open-config-file 'editor-map)
(bind-key (kbd "R") 'restart-emacs 'editor-map)
(bind-key (kbd "I") 'open-config-dir 'editor-map)

;; Pandoc conversion bindings (C-c o p ...)
(define-prefix-command 'pandoc-map nil "Pandoc")
(bind-key (kbd "p") 'pandoc-map 'omni-map)
(bind-key (kbd "c") 'pandoc-convert 'pandoc-map)              ; C-c o p c - Custom conversion
(bind-key (kbd "m") 'pandoc-markdown-to-pdf 'pandoc-map)      ; C-c o p m - Markdown to PDF
(bind-key (kbd "o") 'pandoc-org-to-pdf 'pandoc-map)           ; C-c o p o - Org to PDF
(bind-key (kbd "t") 'pandoc-typst-to-pdf 'pandoc-map)         ; C-c o p t - Typst to PDF
(bind-key (kbd "d") 'pandoc-markdown-to-docx 'pandoc-map)     ; C-c o p d - Markdown to DOCX


;; Key Unbinding for avoiding frame suspension
(use-package frame
  :ensure nil  ;; Mark as built-in
  :config
  (unbind-key (kbd "C-z") global-map)     
  (unbind-key (kbd "C-x C-z") global-map))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aditional settings: ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Better defaults
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c r") 'revert-buffer)

;; File operations
(global-set-key (kbd "C-c F") 'find-file-other-window)


;;; keybindings.el ends here
