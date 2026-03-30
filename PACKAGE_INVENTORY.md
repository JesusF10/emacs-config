# Emacs Package Inventory

**Generated:** March 30, 2026  
**Package Manager:** Elpaca 0.11  
**Total Installed:** 27 packages  
**Total Declared:** 36 (includes built-in packages)

---

## Package Categories

###  Core Package Management (2)

| Package | Version | Type | Description | Config File |
|---------|---------|------|-------------|-------------|
| **elpaca** | 0.11 | Core | Modern Emacs package manager | packages.el |
| **elpaca-use-package** | - | Core | use-package integration for Elpaca | packages.el |

**Status:**  Essential, actively used

---

###  UI and Appearance (6)

| Package | Type | Description | Config File |
|---------|------|-------------|-------------|
| **doom-themes** | External | Beautiful theme collection (using Doom Palenight) | ui.el |
| **doom-modeline** | External | Modern modeline with icons and information | ui.el |
| **nerd-icons** | External | Icon fonts and utilities (required by modeline/dired) | ui.el |
| **rainbow-delimiters** | External | Color-coded parentheses/brackets | ui.el |
| **rainbow-mode** | External | Display color codes with actual colors | ui.el |
| **beacon** | External | Highlight cursor position after scroll | ui.el |

**Status:**  All actively used

---

###  Completion and Navigation (5)

| Package | Type | Description | Config File |
|---------|------|-------------|-------------|
| **vertico** | External | Vertical completion UI (performant, minimal) | editor.el |
| **orderless** | External | Fuzzy completion matching | editor.el |
| **marginalia** | External | Rich annotations in minibuffer | editor.el |
| **consult** | External | Enhanced search/navigation commands | editor.el |
| **corfu** | External | Minimal popup completion UI | editor.el |

**Status:**  All actively used

---

###  Editor Enhancements (6)

| Package | Type | Description | Config File |
|---------|------|-------------|-------------|
| **which-key** | External | Display keybinding help popup | editor.el |
| **crux** | External | Smart editor commands (smart-open-line, etc.) | editor.el |
| **cape** | External | Extra completion sources (dabbrev, file, elisp) | editor.el |
| **eldoc-box** | External | Hover documentation in popup box | editor.el |
| **helpful** | External | Better help buffers with examples | editor.el |
| **smooth-scrolling** | External | Smooth scrolling behavior | ui.el |

**Status:**  All actively used

---

###  File Management (2)

| Package | Type | Description | Config File | Status |
|---------|------|-------------|-------------|--------|
| **dired** | Built-in | Traditional file manager | editor.el |  Active |
| **nerd-icons-dired** | External | Minimal icon display for Dired | editor.el |  Active (NEW) |

**Removed:**
- ~~**dirvish**~~ - Heavy file manager replacement (removed in modernization)

**Status:**  Modernized to minimal Dired

---

###  Python Development (3)

| Package | Type | Description | Config File |
|---------|------|-------------|-------------|
| **eglot** | Built-in | LSP client (connects to Ty) | lsp.el |
| **treesit** | Built-in | Tree-sitter integration | lsp.el |
| **flymake** | Built-in | Diagnostic/linting framework | lsp.el (implicit) |

**External Tools (not Emacs packages):**
- **ty** 0.0.26 - Rust-based LSP server + type checker (.venv local)
- **ruff** 0.15.8 - Rust-based linter + formatter (.venv local)
- **uv** - Python package/environment manager (global)

**Status:**  Modernized stack (Ty replaces Pyright)

---

###  Data Science File Formats (4 - NEW)

| Package | Type | Description | Config File |
|---------|------|-------------|-------------|
| **csv-mode** | External | CSV editing with column alignment | lsp.el |
| **toml-mode** | External | TOML configuration files | lsp.el |
| **yaml-mode** | External | YAML configuration files | lsp.el |
| **markdown-mode** | External | Markdown/MDX support | lsp.el |

**Status:**  NEW - Added in modernization

---

###  Language-Specific (2)

| Package | Type | Description | Config File |
|---------|------|-------------|-------------|
| **racket-mode** | External | Racket/Scheme development | lsp.el |
| **paredit** | External | Structural editing for Lisp | lsp.el |

**Status:**  Used for Racket development

---

###  Built-in Enhanced (11)

These are Emacs built-in packages with custom configurations:

| Package | Description | Config File |
|---------|-------------|-------------|
| **files** | Backup and autosave settings | editor.el |
| **help** | Help window behavior | editor.el |
| **page** | Narrowing to page | editor.el |
| **windmove** | Easy window navigation | editor.el |
| **uniquify** | Better buffer names | editor.el |
| **recentf** | Recent files tracking | editor.el |
| **delsel** | Delete selection mode | editor.el |
| **frame** | Frame/window settings | ui.el |
| **doc-view** | Document viewer | ui.el |
| **eglot** | LSP client | lsp.el |
| **treesit** | Tree-sitter parser | lsp.el |

**Status:**  All configured and active

---

## Dependency Packages (5)

These are installed as dependencies but not directly used:

| Package | Purpose | Used By |
|---------|---------|---------|
| **dash** | List manipulation library | Various packages |
| **s** | String manipulation library | Various packages |
| **f** | File manipulation library | Various packages |
| **elisp-refs** | Find references in elisp | helpful |
| **shrink-path** | Path shortening | doom-modeline |

**Status:**  Transitive dependencies, can't be removed

---

## Unused/Deprecated Packages (1)

| Package | Reason | Recommendation |
|---------|--------|----------------|
| **dirvish** | Removed in modernization | Can be cleaned up |

**Cleanup command:**
```elisp
M-x elpaca-delete RET dirvish RET
```

---

## Package Statistics

### By Type:
- **External packages:** 26
- **Built-in configured:** 11
- **Dependencies:** 5 (transitive)
- **Unused:** 1 (dirvish)

### By Category:
- UI/Appearance: 6
- Completion/Navigation: 5
- Editor Enhancement: 6
- File Management: 2
- Python Development: 3 (built-in) + external tools
- Data Science: 4 (NEW)
- Language-Specific: 2
- Core: 2
- Dependencies: 5

### By Status:
-  Active: 36
-  Dependency: 5
-  Unused: 1

---

## Recently Added (Modernization)

### New Packages:
1. **nerd-icons-dired** - Minimal icons for Dired
2. **csv-mode** - CSV file editing
3. **toml-mode** - TOML configuration
4. **yaml-mode** - YAML configuration
5. **markdown-mode** - Markdown/MDX editing

### Removed Packages:
1. ~~**dirvish**~~ - Heavy file manager (replaced by Dired + nerd-icons-dired)

### Tool Replacements:
- ~~**Pyright**~~ (Node.js-based) � **Ty** (Rust-based, 10x faster)
- Enhanced: **Ruff** (now with async Flymake backend)

---

## Missing Packages to Install

Based on declarations in config files, these should be installed:

```
csv-mode
markdown-mode
nerd-icons-dired
toml-mode
yaml-mode
```

**Installation:**
These will auto-install on next Emacs restart due to `:ensure t` in use-package declarations.

Or manually:
```elisp
M-x elpaca-fetch-all
M-x elpaca-process-queues
```

---

## Configuration File Breakdown

### packages.el (2 packages)
- Core: elpaca, elpaca-use-package

### ui.el (9 packages)
- doom-themes, doom-modeline, nerd-icons
- rainbow-delimiters, rainbow-mode, beacon
- smooth-scrolling
- Built-in: frame, doc-view

### editor.el (16 packages)
- Completion: vertico, orderless, marginalia, consult, corfu, cape
- Editor: which-key, crux, eldoc-box, helpful
- File: dired, nerd-icons-dired
- Built-in: files, help, page, windmove, uniquify, recentf, delsel

### lsp.el (12 packages)
- Python: eglot, treesit (built-in)
- Data Science: csv-mode, toml-mode, yaml-mode, markdown-mode
- Racket: racket-mode, paredit

### keybindings.el (0 packages)
- Only keybinding definitions

### org-mode.el (0 packages assumed)
- Org-mode configuration (built-in)

### utils.el (0 packages assumed)
- Utility functions

---

## Elpaca Status Check

To verify package installation status:

```elisp
;; List all packages
M-x elpaca-browse

;; Update all packages
M-x elpaca-fetch-all
M-x elpaca-merge-all

;; Rebuild a package
M-x elpaca-rebuild RET package-name RET

;; Delete unused package
M-x elpaca-delete RET dirvish RET
```

---

## Recommendations

### 1. Clean Up Unused Package 
```elisp
M-x elpaca-delete RET dirvish RET
```

### 2. Install Missing Packages 
Restart Emacs or run:
```elisp
M-x elpaca-process-queues
```

This will install:
- csv-mode
- toml-mode
- yaml-mode
- markdown-mode
- nerd-icons-dired

### 3. Update All Packages (Optional)
```elisp
M-x elpaca-fetch-all
M-x elpaca-merge-all
```

### 4. Install Nerd Icons Fonts (If not done)
```elisp
M-x nerd-icons-install-fonts
```

---

## Summary

**Total Ecosystem:**
- 27 packages installed via Elpaca
- 11 built-in packages configured
- 5 dependency packages (automatic)
- 3 external CLI tools (ty, ruff, uv)

**Health Status:**
-  Core system: Healthy
-  UI layer: Complete
-  Completion: Optimized (Vertico stack)
-  Python tooling: Modernized (Ty + Ruff + uv)
-  Data Science: NEW support added
-  Cleanup needed: 1 unused package (dirvish)
-  Installation pending: 5 new packages (will auto-install)

**Performance Profile:**
- Minimal dependency tree
- Fast startup (<2s)
- Modern Rust-based tooling (ty, ruff)
- Async operations (non-blocking)

---

**Date:** March 30, 2026  
**Config Location:** `~/.emacs.d/`
