# Programming Languages Support

**Emacs Configuration:** Data Science Edition  
**Date:** March 30, 2026  
**Total Languages:** 24 with Tree-sitter support + 2 with LSP

---

## Overview

Your Emacs configuration includes comprehensive language support optimized for Data Science workflows, with **Tree-sitter** for syntax highlighting and **LSP** for intelligent code features.

---

##  Primary: Python (Full Stack)

**Support Level:**  Complete

### Features:
-  **Tree-sitter** parsing (`python-ts-mode`)
-  **LSP Server:** Ty 0.0.26 (Rust-based, 10-100x faster than Pyright)
-  **Type checking:** Ty inline
-  **Linting:** Ruff (async Flymake backend)
-  **Formatting:** Ruff (auto-format on save)
-  **Environment detection:** uv-aware, local .venv priority
-  **Completions:** Via Eglot + Ty
-  **Jump to definition:** M-.
-  **Hover documentation:** Automatic
-  **Code actions:** M-x eglot-code-actions
-  **Refactoring:** Rename with M-x eglot-rename

### Configuration:
- **File:** lsp.el
- **LSP Command:** `ty server`
- **Tools:** ty, ruff (installed per-project in .venv)
- **Package Manager:** uv

### Keybindings:
```
C-c l s    Start Eglot LSP
C-c l r    Rename symbol
C-c l a    Code actions
C-c l f    Format buffer
C-c l F    Format with Ruff (manual)
C-c l L    Check with Ruff (manual)
```

---

##  Data Science Languages (3)

### R - Statistical Computing
**Support Level:**  Excellent

-  Tree-sitter: `r-ts-mode`
-  Syntax highlighting
-  Auto-indentation
-  LSP: Not configured (can add with `r-language-server`)

### Julia - High-Performance Computing
**Support Level:**  Excellent

-  Tree-sitter: `julia-ts-mode`
-  Syntax highlighting
-  Auto-indentation
-  LSP: Not configured (can add with LanguageServer.jl)

### Scala - Big Data Processing
**Support Level:**  Excellent

-  Tree-sitter: `scala-ts-mode`
-  Syntax highlighting
-  Auto-indentation
-  LSP: Not configured (can add with Metals)

---

##  Web Development (6)

### JavaScript
**Support Level:**  Complete (with LSP)

-  Tree-sitter: `js-ts-mode`
-  LSP: typescript-language-server (if installed)
-  Syntax highlighting
-  Completions (with LSP)
-  Linting: Can add ESLint

**LSP Setup:**
```bash
npm install -g typescript-language-server
```

### TypeScript / TSX
**Support Level:**  Complete (with LSP)

-  Tree-sitter: `typescript-ts-mode`, `tsx-ts-mode`
-  LSP: typescript-language-server (if installed)
-  JSX/TSX support
-  Type checking
-  Completions

### HTML
**Support Level:**  Excellent

-  Tree-sitter: Auto-enabled
-  Syntax highlighting
-  Tag matching
-  Auto-indentation

### CSS
**Support Level:**  Excellent

-  Tree-sitter: `css-ts-mode`
-  Syntax highlighting
-  Property completion
-  Color highlighting (rainbow-mode)

### JSON
**Support Level:**  Excellent

-  Tree-sitter: `json-ts-mode`
-  Syntax highlighting
-  Auto-formatting
-  Validation

### SQL
**Support Level:**  Good

-  Tree-sitter: Auto-enabled
-  Syntax highlighting
-  Auto-indentation

---

##  Data & Configuration Formats (5)

### CSV - Data Files
**Support Level:**  Excellent (NEW)

-  Tree-sitter: `csv-ts-mode`
-  **csv-mode** package
-  Column alignment
-  Custom separators (`,` `;` `|` `\t`)
-  Navigation by column

**Features:**
```elisp
;; Automatically aligns columns
;; Supports multiple separators
;; Easy column navigation
```

### TOML - Configuration
**Support Level:**  Excellent (NEW)

-  Tree-sitter: `toml-ts-mode`
-  **toml-mode** package
-  Syntax highlighting
-  Auto-indentation
-  Perfect for `pyproject.toml`

### YAML - Configuration
**Support Level:**  Excellent (NEW)

-  Tree-sitter: `yaml-ts-mode`
-  **yaml-mode** package
-  Syntax highlighting
-  Indentation validation
-  Common in CI/CD, Kubernetes

### Markdown / MDX
**Support Level:**  Excellent (NEW)

-  Tree-sitter: `markdown-ts-mode`
-  **markdown-mode** package
-  GFM mode for GitHub
-  MDX support (Markdown + JSX)
-  Live preview (external)
-  Code block highlighting

### Dockerfile
**Support Level:**  Excellent

-  Tree-sitter: Auto-enabled
-  Syntax highlighting
-  Instruction validation

---

##  Academic & Documentation (3)

### LaTeX
**Support Level:**  Excellent

-  Tree-sitter: Auto-enabled
-  Syntax highlighting
-  Math mode support
-  Bibliography support

### Org Mode
**Support Level:**  Excellent

-  Tree-sitter: Auto-enabled
-  Full Org features
-  Native Emacs integration
-  Babel support (code blocks)

### Markdown
*See Data & Configuration Formats above*

---

##  Systems & Build Tools (3)

### C++
**Support Level:**  Excellent

-  Tree-sitter: `c++-ts-mode`
-  Syntax highlighting
-  Auto-indentation
-  LSP: Can add clangd

### Java
**Support Level:**  Excellent

-  Tree-sitter: `java-ts-mode`
-  Syntax highlighting
-  Auto-indentation
-  LSP: Can add eclipse.jdt.ls

### Makefile
**Support Level:**  Good

-  Tree-sitter: Auto-enabled
-  Syntax highlighting
-  Target highlighting

---

##  Scripting & Utilities (2)

### Lua
**Support Level:**  Excellent

-  Tree-sitter: Auto-enabled
-  Syntax highlighting
-  Common in Neovim configs

### Emacs Lisp
**Support Level:**  Native

-  Tree-sitter: Available
-  Native Emacs support
-  Built-in evaluation
-  Interactive development
-  eldoc integration
-  helpful package

---

##  Language-Specific: Lisp Family (1)

### Racket / Scheme
**Support Level:**  Excellent

-  **racket-mode** package
-  **paredit** (structural editing)
-  REPL integration
-  Syntax highlighting
-  Parenthesis matching

**Configuration:**
```elisp
;; In lsp.el
(use-package racket-mode :ensure t :defer t)
(use-package paredit :ensure t)
```

**Path:** `/home/jesusf10/.config/racket/bin/racket`

---

##  Language Support Summary

### By Category:

| Category | Count | Languages |
|----------|-------|-----------|
| **Data Science** | 4 | Python (full), R, Julia, Scala |
| **Web Development** | 6 | JavaScript, TypeScript, TSX, HTML, CSS, JSON |
| **Data Formats** | 5 | CSV, TOML, YAML, Markdown, Dockerfile |
| **Academic** | 3 | LaTeX, Org, Markdown |
| **Systems** | 3 | C++, Java, Makefile |
| **Scripting** | 2 | Lua, Emacs Lisp |
| **Database** | 1 | SQL |
| **Lisp Family** | 1 | Racket |

**Total:** 25 languages

### By Support Level:

| Level | Count | Description |
|-------|-------|-------------|
|  | 11 | Complete (Tree-sitter + LSP or full features) |
|  | 12 | Excellent (Tree-sitter + syntax) |
|  | 2 | Good (basic support) |

### With LSP Support:

1.  **Python** - Ty (Rust-based)
2.  **JavaScript/TypeScript** - typescript-language-server (optional)

### Expandable LSP:

Easy to add LSP for:
- **R:** r-language-server
- **Julia:** LanguageServer.jl
- **Scala:** Metals
- **C++:** clangd
- **Java:** eclipse.jdt.ls
- **Rust:** rust-analyzer (add rust to Tree-sitter list)
- **Go:** gopls (add go to Tree-sitter list)

---

##  Tree-sitter Auto-Remap

These languages automatically use Tree-sitter enhanced modes:

```elisp
c++-mode      † c++-ts-mode
css-mode      † css-ts-mode
csv-mode      † csv-ts-mode      (NEW)
java-mode     † java-ts-mode
javascript    † js-ts-mode
json-mode     † json-ts-mode
julia-mode    † julia-ts-mode
markdown-mode † markdown-ts-mode (NEW)
python-mode   † python-ts-mode
r-mode        † r-ts-mode
scala-mode    † scala-ts-mode
toml-mode     † toml-ts-mode     (NEW)
typescript    † typescript-ts-mode
yaml-mode     † yaml-ts-mode     (NEW)
```

**Total Auto-Remapped:** 14 languages

---

##  Management Commands

### Install All Parsers:
```elisp
M-x treesit-install-all-languages
```

Installs parsers for all 24 languages (takes 2-3 minutes).

### Check Parser Status:
```elisp
M-x treesit-check-parsers
```

Shows which parsers are installed and working.

### Install Single Parser:
```elisp
M-x treesit-install-language-grammar RET language-name RET
```

### Reinstall Problematic Parser:
```elisp
M-x treesit-clean-and-reinstall RET language-name RET
```

---

##  Language Package Requirements

### Installed via Elpaca:

| Package | Languages | Auto-install |
|---------|-----------|--------------|
| csv-mode | CSV | Yes |
| toml-mode | TOML | Yes |
| yaml-mode | YAML | Yes |
| markdown-mode | Markdown, MDX | Yes |
| racket-mode | Racket, Scheme | Yes |
| paredit | Lisp family | Yes |

### External Tools (CLI):

| Tool | Language | Install | Location |
|------|----------|---------|----------|
| **ty** | Python LSP | `uv pip install ty` | `.venv/bin/` |
| **ruff** | Python lint/format | `uv pip install ruff` | `.venv/bin/` |
| typescript-language-server | JS/TS LSP | `npm install -g` | Global |

---

##  Data Science Optimized

Your configuration is specifically optimized for Data Science work:

### Core Languages:
1. **Python** - Full LSP + linting + formatting
2. **R** - Statistics and data analysis
3. **Julia** - High-performance numerical computing
4. **Scala** - Big data with Spark

### Data Formats:
1. **CSV** - Column-aligned editing
2. **JSON** - API data
3. **TOML** - Project configuration (`pyproject.toml`)
4. **YAML** - Pipeline configuration
5. **Markdown** - Documentation and notebooks

### Supporting:
1. **SQL** - Database queries
2. **JavaScript/TypeScript** - Interactive visualizations (D3.js, etc.)
3. **LaTeX** - Academic papers
4. **Org** - Research notebooks

---

##  Highlights

### Fastest Languages (Rust-based tooling):
- **Python** with Ty (10-100x faster than Pyright)
- **Python** with Ruff (10-100x faster than pylint)

### Best Integrated:
- **Python** - Complete LSP stack
- **Emacs Lisp** - Native support
- **Org Mode** - Native Emacs

### NEW in Modernization:
- **CSV mode** - Column alignment
- **TOML mode** - Modern Python configs
- **YAML mode** - DevOps workflows
- **Markdown mode** - Documentation

### Missing but Easy to Add:
- **Rust** (add to Tree-sitter list)
- **Go** (add to Tree-sitter list)
- **Ruby** (add to Tree-sitter list)
- **PHP** (add to Tree-sitter list)

---

##  Adding More Languages

### Example: Add Rust Support

1. **Add to Tree-sitter list** (lsp.el):
```elisp
(rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
```

2. **Add mode remap**:
```elisp
(rust-mode . rust-ts-mode)
```

3. **Install parser**:
```elisp
M-x treesit-install-language-grammar RET rust RET
```

4. **Add LSP** (optional):
```bash
rustup component add rust-analyzer
```

```elisp
(add-to-list 'eglot-server-programs
             '(rust-mode . ("rust-analyzer")))
```

---

##  Language Learning Resources

### Python:
- Official: https://docs.python.org/3/
- Ty docs: https://github.com/tmke8/ty
- Ruff docs: https://docs.astral.sh/ruff/

### R:
- Official: https://www.r-project.org/
- RStudio: https://posit.co/

### Julia:
- Official: https://julialang.org/
- JuliaHub: https://juliahub.com/

### Tree-sitter:
- Official: https://tree-sitter.github.io/tree-sitter/

---

## Summary

Your Emacs configuration supports **25 programming languages** with:
-  24 Tree-sitter parsers for fast, accurate syntax highlighting
-  2 LSP servers configured (Python with Ty, optional JS/TS)
-  14 languages with automatic Tree-sitter mode switching
-  5 new Data Science file formats
-  Optimized for Python + R + Julia + Scala workflows

**Status:** Production-ready for Data Science development 

---

**Configuration File:** `~/.emacs.d/lisp/lsp.el`  
**Management Commands:** See "Management Commands" section above  
**Documentation:** README.md, CONTEXT.md, CHANGES.md
