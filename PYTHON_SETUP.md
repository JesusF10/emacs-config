# Python Development Setup Guide

This guide helps you install the required tools for Python development with your Emacs configuration.

## Required Tools Installation

### 1. Install Ruff (Linter & Formatter)

**Option A: Using pip (Recommended)**
```bash
pip install ruff
```

**Option B: Using pipx (Isolated installation)**
```bash
pipx install ruff
```

**Option C: Using system package manager**
```bash
# Ubuntu/Debian
sudo apt install ruff

# Arch Linux
sudo pacman -S ruff

# macOS with Homebrew
brew install ruff
```

### 2. Install Python LSP Server (pylsp)

**Using pip:**
```bash
pip install python-lsp-server[all]
```

**Using pipx:**
```bash
pipx install python-lsp-server[all]
```

## Verify Installation

Check that both tools are available in your PATH:

```bash
which ruff
which pylsp

# Test versions
ruff --version
pylsp --version
```

## Configuration Overview

Your Emacs setup now includes:

### **Language Server**: Pylsp
- Provides: autocomplete, go-to-definition, documentation, refactoring
- Disabled built-in linters (pycodestyle, pyflakes, flake8) to avoid conflicts with Ruff

### **Linter & Formatter**: Ruff
- Extremely fast Python linter and formatter
- Replaces: flake8, black, isort, and many other tools  
- Built-in integration without external packages
- Format on demand with `C-c C-f`
- Lint check with `C-c C-l`

### **Diagnostics**: Flymake (built-in)
- Shows errors and warnings in real-time
- Integrates seamlessly with Eglot and pylsp

## Usage

### Keybindings

**LSP Operations (prefix `C-c l`):**
- `C-c l s` - Start/connect Eglot
- `C-c l r` - Rename symbol
- `C-c l a` - Code actions
- `C-c l f` - Format buffer
- `C-c l R` - Reconnect LSP server
- `C-c l S` - Shutdown LSP server

**Error Navigation:**
- `C-c l e e` - Show project diagnostics
- `C-c l e n` - Next error
- `C-c l e p` - Previous error
- `C-c l e l` - Show buffer diagnostics

**Python-specific:**
- `C-c C-f` - Format buffer with Ruff  
- `C-c C-l` - Check buffer with Ruff linter
- `C-c l f` - Format buffer (alternative binding)
- `C-c l c` - Show buffer diagnostics
- Manual format on save (see configuration section to enable)

### Project Configuration

Create a `pyproject.toml` in your project root for Ruff configuration:

```toml
[tool.ruff]
# Enable specific rule sets
select = [
    "E",    # pycodestyle errors
    "W",    # pycodestyle warnings  
    "F",    # pyflakes
    "I",    # isort
    "B",    # flake8-bugbear
    "C4",   # flake8-comprehensions
    "UP",   # pyupgrade
]

# Ignore specific rules
ignore = [
    "E501",  # line too long (handled by formatter)
]

# Set line length
line-length = 88

# Target Python version
target-version = "py39"

```toml
[tool.ruff.format]
# Use double quotes for strings
quote-style = "double"

# Indent with spaces
indent-style = "space"
```

### Enabling Auto-format on Save

By default, Ruff formatting is manual (use `C-c C-f`). To enable automatic formatting on save, edit your `lisp/lsp.el` file and uncomment this line:

```elisp
;; Uncomment the next line if you want auto-format on save
(add-hook 'before-save-hook #'my/ruff-format-on-save)
```

Or add this to your configuration:
```elisp
(add-hook 'python-mode-hook 
          (lambda () 
            (add-hook 'before-save-hook #'my/ruff-format-on-save nil t)))
```
```

## Troubleshooting

### LSP Server Crashes (jsonrpc error, server died)

**Check tool availability:**
```bash
# In Emacs, use: M-x my/check-python-tools
# Or manually check:
which ruff
which pylsp
which pyright-langserver
```

**Common fixes:**

1. **Install missing tools:**
```bash
# Install both tools
pip install ruff python-lsp-server[all]

# Or using pipx for isolation
pipx install ruff
pipx install python-lsp-server[all]
```

2. **Restart Emacs** after installation

3. **Check Eglot status:**
   - `C-c l d e` - View Eglot events buffer for detailed errors
   - `C-c l d l` - View LSP server stderr output
   - `C-c l d t` - Check Python tools availability and Flymake status
   - `C-c l d b` - Check active Flymake backends
   - `C-c l d r` - Reset Flymake backends (fixes backend warnings)

4. **Manually reconnect:**
   - `C-c l S` - Shutdown current LSP server
   - `C-c l s` - Start Eglot again

### Ruff not found
- Ensure Ruff is installed: `pip install ruff`
- Check PATH: `which ruff`
- Restart Emacs after installation

### Pylsp not found
- Install: `pip install python-lsp-server[all]`
- Check PATH: `which pylsp`
- Restart Emacs

### Server keeps crashing
1. **Try with simpler config** - temporarily disable ruff-format:
   ```elisp
   ;; Comment out in lsp.el:
   ;; :hook (python-mode . ruff-format-on-save-mode)
   ```

2. **Use Pyright as fallback:**
   ```bash
   npm install -g pyright
   ```

3. **Check virtual environment** - ensure tools are installed in active environment

### Conflicts with other Python tools
- Disable conflicting extensions in other editors
- Ensure only one LSP server is running per Python project
- Use `M-x eglot-shutdown` and `M-x eglot` to restart if needed

### Flymake Backend Warnings
If you see warnings like "Disabling backend flymake-proc-legacy-flymake" or "python-flymake":

1. **Reset Flymake backends:**
   ```
   C-c l d r
   ```

2. **These warnings are harmless** - they occur when:
   - Legacy Flymake backends conflict with modern LSP
   - Python's built-in flymake conflicts with Eglot
   - The configuration automatically disables problematic backends

3. **Check active backends:**
   ```
   C-c l d b
   ```

### Performance issues
- Use `M-x eglot-events-buffer` to debug LSP communication
- Check `M-x flymake-running-backends` for active diagnostic tools
- Consider excluding large directories in `.gitignore`

## Alternative Setups

If you prefer different tools:

### Use Pyright instead of pylsp:
```elisp
(add-to-list 'eglot-server-programs
             '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
```

### Use Black instead of Ruff formatting:
```bash
pip install black
```

Then disable `ruff-format-on-save-mode` and use Black manually or with other packages.
