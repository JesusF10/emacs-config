# Python Development Setup Guide

This guide helps you install the required tools for Python development with your Emacs configuration.

**Philosophy:** Modern Rust-based stack (uv + ty + ruff) for maximum performance and per-project isolation.

## Quick Start (Recommended)

```bash
# 1. Install uv globally (once)
curl -LsSf https://astral.sh/uv/install.sh | sh

# 2. Create project and install tools
cd your-project/
uv venv
source .venv/bin/activate
uv pip install ty ruff

# 3. Open in Emacs - auto-detection!
emacs .
```

**Emacs helper:** `M-x my/init-python-project`

---

## Required Tools

### 1. uv - Package Manager (Rust-based)

**Install globally:**
```bash
curl -LsSf https://astral.sh/uv/install.sh | sh
uv --version
```

**Why:** 10-100x faster than pip, from Astral (Ruff/Ty creators)

### 2. Ty - LSP Server (Rust-based)

**Install per-project:**
```bash
cd your-project/
source .venv/bin/activate
uv pip install ty
```

**Verify:**
```bash
ty --version
```

**Why Ty:**
- 10-100x faster than Pyright/pylsp
- Zero Node.js dependency
- Single config: `pyproject.toml`

### 3. Ruff - Linter & Formatter (Rust-based)

**Install per-project:**
```bash
uv pip install ruff
```

**Why:** 10-100x faster than flake8/black

---

## Verification

```bash
cd your-project/
source .venv/bin/activate
which ty ruff python  # Should be .venv/bin/*
ty --version
ruff --version
```

---

## Configuration

Your Emacs auto-detects:
- `.venv` directories
- Local `ty` for LSP
- Local `ruff` for linting
- Shows `[venv:.venv]` in modeline

### Sample pyproject.toml:

```toml
[tool.ty]
strict = false  # Set true for stricter checks

[tool.ruff]
line-length = 88
select = ["E", "W", "F", "I", "B", "UP"]
target-version = "py310"
```

---

## Keybindings

**LSP (C-c l):**
- `C-c l s` - Start Eglot
- `C-c l r` - Rename symbol
- `C-c l a` - Code actions
- `C-c l R` - Reconnect LSP

**Venv (C-c l v):**
- `C-c l v a` - Activate venv
- `C-c l v d` - Deactivate

**Ruff:**
- `C-c l F` - Format buffer
- `C-c l L` - Lint check

---

## Troubleshooting

### ty not found
```bash
source .venv/bin/activate
uv pip install ty
# In Emacs: M-x eglot-reconnect
```

### LSP not starting
```elisp
M-x eglot-events-buffer  ; Check logs
M-: (getenv "VIRTUAL_ENV")  ; Should show .venv path
```

### venv not detected
```elisp
M-x pyvenv-activate RET /path/to/.venv RET
```

---

## Alternative LSP Servers

### Use Pyright (TypeScript-based):
```bash
npm install -g pyright
```
```elisp
;; In lsp.el, replace ty with:
'((python-mode python-ts-mode) . ("pyright-langserver" "--stdio"))
```

### Use pylsp (Python-based):
```bash
pip install python-lsp-server[all]
```
```elisp
;; In lsp.el:
'((python-mode python-ts-mode) . ("pylsp"))
```

---

## Resources

- Ty: https://github.com/astral-sh/ty
- Ruff: https://docs.astral.sh/ruff/
- uv: https://docs.astral.sh/uv/
- Advanced features: [PYTHON_ADVANCED.md](PYTHON_ADVANCED.md)
