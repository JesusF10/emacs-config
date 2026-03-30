# Typst and Pandoc Integration Guide

## Overview

This configuration adds support for:

1. **Typst** - Modern markup language for scientific documents (alternative to LaTeX)
2. **Pandoc** - Universal document converter with convenient keybindings

## Typst Support

### What is Typst?

Typst is a modern markup language designed for scientific and technical documents. It offers:

- Fast compilation (10-100x faster than LaTeX)
- Clean, readable syntax
- Built-in scripting capabilities
- Better error messages than LaTeX
- Native support for modern features (grid layouts, SVG, etc.)

### Configuration Added

**File:** `~/.emacs.d/lisp/lsp.el`

```elisp
;; Tree-sitter parser for syntax highlighting
(typst . ("https://github.com/uben0/tree-sitter-typst"))

;; Typst major mode with Tree-sitter support
(use-package typst-ts-mode
  :ensure t
  :mode "\\.typ\\'"
  :config
  (when (executable-find "typst")
    (setq typst-ts-mode-watch-options "--open")))
```

### Installation

1. **Install Typst Tree-sitter parser:**

   ```
   M-x treesit-install-language-grammar RET typst RET
   ```

2. **Install typst-ts-mode package:**
   The package will auto-install on next Emacs restart (managed by Elpaca)

3. **System Typst compiler (already installed):**
   ```bash
   which typst
   # Output: /usr/bin/typst
   ```

### Usage

**Create a Typst document:**

```typst
#set document(title: "My Paper")
#set page(numbering: "1")

= Introduction
This is a _simple_ Typst document with *emphasis*.

== Subsection
Math equations: $ E = m c^2 $

#figure(
  table(
    columns: 2,
    [Header 1], [Header 2],
    [Data 1], [Data 2]
  ),
  caption: [Sample table]
)
```

**Compile to PDF:**

```bash
typst compile document.typ
# or with watch mode:
typst watch document.typ
```

### Features in Emacs

- **Syntax highlighting** via Tree-sitter
- **Auto-completion** (basic, via Eglot if typst-lsp is installed)
- **Live preview** with `typst-ts-mode-watch-options "--open"`

## Pandoc Integration

### What is Pandoc?

Pandoc is a universal document converter supporting 40+ formats:

- Markdown → PDF, DOCX, HTML, LaTeX
- Org-mode → PDF, DOCX, Markdown
- Typst → PDF, HTML
- LaTeX → Markdown, DOCX
- And many more combinations

### Configuration Added

**File:** `~/.emacs.d/lisp/keybindings.el`

**Functions:**

- `pandoc-convert` - Interactive conversion with format selection
- `pandoc-markdown-to-pdf` - Quick Markdown → PDF
- `pandoc-org-to-pdf` - Quick Org → PDF
- `pandoc-typst-to-pdf` - Quick Typst → PDF
- `pandoc-markdown-to-docx` - Quick Markdown → DOCX

### Keybindings

All Pandoc commands are under the **Omni prefix: C-c o p**

| Keybinding  | Command                   | Description                        |
| ----------- | ------------------------- | ---------------------------------- |
| `C-c o p c` | `pandoc-convert`          | Custom conversion (choose formats) |
| `C-c o p m` | `pandoc-markdown-to-pdf`  | Markdown → PDF                     |
| `C-c o p o` | `pandoc-org-to-pdf`       | Org → PDF                          |
| `C-c o p t` | `pandoc-typst-to-pdf`     | Typst → PDF                        |
| `C-c o p d` | `pandoc-markdown-to-docx` | Markdown → DOCX                    |

### Usage Examples

#### Example 1: Convert Markdown to PDF

1. Open `document.md` in Emacs
2. Press `C-c o p m`
3. Output: `document.pdf` created in same directory

#### Example 2: Custom Conversion

1. Open any supported file
2. Press `C-c o p c`
3. Select source format (e.g., "markdown")
4. Select target format (e.g., "html")
5. Output file created with appropriate extension

#### Example 3: Org-mode to DOCX (Custom)

1. Open `notes.org`
2. Press `C-c o p c`
3. From: `org`
4. To: `docx`
5. Output: `notes.docx`

### Supported Formats

**Common input formats:**

- markdown (including GFM, Pandoc's extended markdown)
- org (Org-mode)
- latex / tex
- html
- docx (Word documents)
- rst (reStructuredText)
- typst

**Common output formats:**

- pdf (requires LaTeX or wkhtmltopdf)
- html
- docx
- latex
- markdown
- org
- rst
- typst

### Advanced Pandoc Usage

For more complex conversions, use the shell:

```bash
# Markdown to PDF with custom template
pandoc document.md -o output.pdf --template=mytemplate.tex

# Org to HTML with table of contents
pandoc notes.org -o notes.html --toc

# Multiple inputs to single PDF
pandoc chapter1.md chapter2.md chapter3.md -o book.pdf

# With bibliography
pandoc paper.md --bibliography=refs.bib --csl=ieee.csl -o paper.pdf
```

## Workflow Examples

### Scientific Paper Workflow (Typst)

```typst
// paper.typ
#import "@preview/ieee:1.0.0": *

#show: ieee.with(
  title: "My Research Paper",
  authors: (
    (name: "Your Name", email: "you@example.com"),
  ),
  abstract: [
    This paper presents...
  ]
)

= Introduction
Research shows @citation2023 that...

= Methods
We used the following approach:
$ f(x) = integral_0^infinity e^(-x^2) dif x $

= Results
#figure(
  image("results.png", width: 80%),
  caption: [Experimental results]
)

#bibliography("refs.bib")
```

**Compile in Emacs:**

```
M-x shell-command RET typst compile paper.typ
```

Or use Pandoc to convert Markdown → Typst:

```
C-c o p c → markdown → typst
```

### Data Science Report Workflow

````markdown
# Analysis Report

## Data Loading

```python
import pandas as pd
df = pd.read_csv("data.csv")
```
````

## Visualizations

![Results](plot.png)

## Conclusion

Our analysis shows...

````

**Export options:**
1. `C-c o p m` → PDF for printing
2. `C-c o p d` → DOCX for sharing with collaborators
3. Custom: Markdown → HTML for web publishing

### Documentation Workflow (Org-mode)

```org
#+TITLE: Project Documentation
#+AUTHOR: Your Name
#+DATE: 2026-03-30

* Overview
This project implements...

** Installation
#+BEGIN_SRC bash
pip install mypackage
#+END_SRC

** Usage
See examples in [[file:examples/]]

* API Reference
** Core Functions
- =process_data()= :: Processes input data
````

**Export options:**

1. `C-c o p o` → PDF manual
2. Custom: Org → HTML for online docs
3. Custom: Org → Markdown for GitHub wiki

## Configuration Summary

### Files Modified

1. **`~/.emacs.d/lisp/lsp.el`**
   - Added Typst Tree-sitter parser (line 50)
   - Added `typst-ts-mode` package configuration (lines 176-182)

2. **`~/.emacs.d/lisp/keybindings.el`**
   - Added Pandoc utility functions (lines 28-68)
   - Added Pandoc keybinding prefix map (lines 105-111)

### New Packages to Install

- `typst-ts-mode` (will auto-install via Elpaca on restart)

### New Tree-sitter Parsers

- `typst` (install with: `M-x treesit-install-language-grammar RET typst RET`)

## Troubleshooting

### Typst compilation errors

If Typst compilation fails:

```bash
# Check Typst version
typst --version

# Update Typst (Arch Linux)
sudo pacman -S typst
```

### Pandoc not found

If you see "Pandoc not found" message:

```bash
# Check installation
which pandoc

# Install if missing (Arch Linux)
sudo pacman -S pandoc
```

### PDF generation fails

Pandoc PDF generation requires LaTeX or wkhtmltopdf:

```bash
# Install LaTeX (full)
sudo pacman -S texlive-most

# Or minimal LaTeX
sudo pacman -S texlive-core

# Alternative: wkhtmltopdf
sudo pacman -S wkhtmltopdf
```

### Typst Tree-sitter parser fails

If parser installation fails:

```bash
# Manual installation
cd /tmp
git clone https://github.com/uben0/tree-sitter-typst
cd tree-sitter-typst
gcc -fPIC -c -I. src/parser.c
gcc -fPIC -shared parser.o -o libtree-sitter-typst.so
cp libtree-sitter-typst.so ~/.emacs.d/tree-sitter/
```

## Quick Reference

### Keybinding Cheat Sheet

```
Pandoc Conversions (C-c o p):
├─ c  Custom conversion (choose formats)
├─ m  Markdown → PDF
├─ o  Org → PDF
├─ t  Typst → PDF
└─ d  Markdown → DOCX
```

### Common Commands

```elisp
;; Install Typst parser
M-x treesit-install-language-grammar RET typst RET

;; Convert current file
M-x pandoc-convert

;; Check parser status
M-x treesit-check-parsers

;; Open Typst file (auto-enables typst-ts-mode)
C-x C-f document.typ
```

## Resources

### Typst

- Official site: https://typst.app/
- Documentation: https://typst.app/docs/
- Examples: https://typst.app/universe/
- VS Code extension (for comparison): typst-lsp

### Pandoc

- Official site: https://pandoc.org/
- User's Guide: https://pandoc.org/MANUAL.html
- Demos: https://pandoc.org/demos.html
- Templates: https://github.com/jgm/pandoc-templates

### Emacs Packages

- typst-ts-mode: https://github.com/Ziqi-Yang/typst-ts-mode
- Tree-sitter Typst: https://github.com/uben0/tree-sitter-typst

---

**Date:** March 30, 2026  
**Author:** Jesus Flores Lacarra
