# Advanced Python Development Features

This guide covers the advanced Python development features available in your Emacs configuration.

## Table of Contents
1. [Virtual Environment Management](#virtual-environment-management)
2. [Interactive Python REPL](#interactive-python-repl)
3. [Jupyter-like Workflow (Code Cells)](#jupyter-like-workflow)
4. [Testing with Pytest](#testing-with-pytest)
5. [Visual Debugging (DAP)](#visual-debugging)
6. [Performance Tips](#performance-tips)

---

## Virtual Environment Management

### Features
- **Visual indicator** in modeline showing active venv: `[venv:.venv]`
- **Automatic detection** of project `.venv` directories
- **Manual activation** of any virtualenv
- **Auto-restart LSP** when switching venvs

### Keybindings
- `C-c l v a` - Activate a venv (browse to directory)
- `C-c l v d` - Deactivate current venv
- `C-c l v w` - Workon (select from `$WORKON_HOME`)

### Usage

**Automatic (recommended):**
```bash
# 1. Create project with uv
cd ~/projects/my-project
uv venv
source .venv/bin/activate
uv pip install ty ruff pandas numpy

# 2. Open Python file in Emacs
# Venv will be auto-detected and activated
# Modeline will show: [venv:.venv]
```

**Manual activation:**
```
M-x pyvenv-activate RET ~/projects/my-project/.venv RET
```

---

## Interactive Python REPL

### Features
- **IPython integration** (falls back to standard Python)
- **Send regions/functions/buffer** to REPL
- **Persistent history** and completions
- **Non-intrusive** window management

### Keybindings
- `C-c C-z` - Start/switch to Python shell
- `C-c C-c` - Send entire buffer to REPL
- `C-c C-r` - Send region to REPL (select region first)
- `C-c C-d` - Send current function to REPL
- `C-c C-l` - Send file to REPL

### Workflow Example

```python
# example.py
import pandas as pd

def load_data():
    return pd.read_csv("data.csv")

df = load_data()
print(df.head())
```

**Workflow:**
1. Open `example.py`
2. Press `C-c C-z` to start IPython REPL
3. Press `C-c C-d` with cursor on `load_data()` to send function
4. Type in REPL: `df = load_data()`
5. Select lines with `df.head()` and press `C-c C-r`

### Installing IPython

For the best REPL experience:
```bash
# Inside your venv
uv pip install ipython
```

---

## Jupyter-like Workflow

### Features
- **Code cells** in regular `.py` files (no need for `.ipynb`)
- **Execute cells** individually like Jupyter notebooks
- **Navigate between cells** quickly
- **Compatible** with VS Code, Jupyter, Spyder

### Cell Syntax

Use `# %%` to delimit cells:

```python
# %% Cell 1: Imports
import pandas as pd
import numpy as np

# %% Cell 2: Load data
df = pd.read_csv("sales.csv")
print(f"Loaded {len(df)} rows")

# %% Cell 3: Analysis
summary = df.describe()
print(summary)
```

### Keybindings
- `C-c C-n` - Go to next cell
- `C-c C-p` - Go to previous cell
- `C-c C-e` - Execute current cell
- `C-c C-SPC` - Mark/select current cell
- `C-c %` - Cell command menu

### Workflow

1. **Create cells** with `# %%` markers
2. **Position cursor** in a cell
3. **Execute** with `C-c C-e` (sends to Python REPL)
4. **Navigate** with `C-c C-n` / `C-c C-p`

**Pro tip:** Combine with REPL (`C-c C-z`) to see results in a separate window.

---

## Testing with Pytest

### Features
- **Run tests** from Emacs (no terminal switching)
- **Execute** single function, file, or entire suite
- **Re-run failed tests** only
- **Colorized output** in compilation buffer

### Keybindings
- `C-c t t` - Pytest dispatch menu (choose what to run)
- `C-c t f` - Run test at point (current function)
- `C-c t m` - Run tests in current file
- `C-c t a` - Run all tests in project
- `C-c t l` - Re-run last failed tests
- `C-c t r` - Repeat last pytest command

### Setup

```bash
# Install pytest in your venv
uv pip install pytest pytest-cov
```

### Example Test File

```python
# test_calculations.py
def add(a, b):
    return a + b

def test_add():
    assert add(2, 3) == 5

def test_add_negative():
    assert add(-1, 1) == 0
```

### Workflow

1. **Position cursor** on `test_add` function
2. Press `C-c t f` to run that test
3. **Review output** in `*pytest*` buffer
4. If test fails, **fix code** and press `C-c t l` (re-run failed only)

---

## Visual Debugging

### Features
- **Breakpoints** with visual indicators (red dots in fringe)
- **Step through code** line by line
- **Inspect variables** in real-time
- **Eval expressions** in debug context
- **Multiple debug sessions** supported

### Required Setup

Install `debugpy` in your venv:
```bash
uv pip install debugpy
```

### Keybindings
- `C-c d b` - Toggle breakpoint at current line
- `C-c d d` - Start debugging (prompts for config)
- `C-c d l` - Debug last configuration
- `C-c d n` - Step over (next line)
- `C-c d i` - Step into function
- `C-c d o` - Step out of function
- `C-c d c` - Continue execution
- `C-c d q` - Quit/disconnect debugger
- `C-c d e` - Eval expression in debug context
- `C-c d r` - Eval selected region

### Debugging Workflow

**Example script:**
```python
# debug_example.py
def calculate_average(numbers):
    total = sum(numbers)
    count = len(numbers)
    return total / count  # Bug: will crash on empty list

data = [10, 20, 30]
result = calculate_average(data)
print(f"Average: {result}")

# Test edge case
empty = []
result2 = calculate_average(empty)  # This will crash
```

**Debug steps:**
1. **Set breakpoint** on line 2: position cursor, press `C-c d b`
2. **Start debugging**: `C-c d d`, select "Python :: Run Configuration"
3. **Execution stops** at breakpoint
4. **Inspect** `numbers` variable in *Locals* window
5. **Step through**: Press `C-c d n` repeatedly
6. **Evaluate**: Press `C-c d e`, type `count > 0` to check condition
7. **Continue**: Press `C-c d c` to run until next breakpoint

**UI Layout during debug:**
- Main window: source code with execution indicator
- Left panel: local variables and their values
- Bottom panel: debug console for expressions
- Breakpoints visible as red dots in fringe

### Debug Templates

Two built-in templates available:

1. **Python :: Run Configuration** - Debug current script
2. **Python :: Run pytest** - Debug test functions

Access via `C-c d d` and select template.

---

## Performance Tips

### Native Compilation

Your Emacs has **native compilation enabled**. Compile packages for better performance:

```elisp
;; In Emacs, run once:
M-x my/native-compile-packages

;; This compiles all packages (~5-10 minutes)
;; Significantly improves runtime speed (10-30% faster)
```

Compile your config:
```elisp
M-x my/native-compile-config
```

### Garbage Collection

The configuration includes optimized GC settings:
- **Startup**: GC deferred for fast init
- **Runtime**: 16MB threshold (balanced)
- **Minibuffer**: GC paused during completion
- **Focus-out**: GC runs when Emacs loses focus

**No action needed** - automatic optimization.

### LSP Performance

For large projects (>10k LOC), Eglot is optimized with:
- **Async connection**: non-blocking startup
- **Debouncing**: changes sent after 0.5s idle
- **Disabled**: documentHighlight (can be slow)
- **Event logging**: disabled to save memory

If Eglot feels slow, try:
```elisp
;; Increase debounce time (more delay, less CPU)
(setq eglot-send-changes-idle-time 1.0)
```

### Project-specific Optimizations

Create `.dir-locals.el` in project root:

```elisp
;; Disable certain features for this project
((python-mode . ((eglot-workspace-configuration
                  . ((:python :analysis :typeCheckingMode "off"))))))
```

---

## Quick Reference Card

### Virtual Environments
| Keybinding | Action |
|------------|--------|
| `C-c l v a` | Activate venv |
| `C-c l v d` | Deactivate venv |
| `C-c l v w` | Workon (from $WORKON_HOME) |

### REPL
| Keybinding | Action |
|------------|--------|
| `C-c C-z` | Start/switch to REPL |
| `C-c C-c` | Send buffer |
| `C-c C-r` | Send region |
| `C-c C-d` | Send function |

### Code Cells
| Keybinding | Action |
|------------|--------|
| `C-c C-n` | Next cell |
| `C-c C-p` | Previous cell |
| `C-c C-e` | Execute cell |
| `C-c C-SPC` | Mark cell |

### Testing
| Keybinding | Action |
|------------|--------|
| `C-c t t` | Pytest menu |
| `C-c t f` | Test function |
| `C-c t m` | Test file |
| `C-c t a` | All tests |
| `C-c t l` | Failed tests |
| `C-c t r` | Repeat last |

### Debugging
| Keybinding | Action |
|------------|--------|
| `C-c d b` | Toggle breakpoint |
| `C-c d d` | Start debug |
| `C-c d n` | Step over |
| `C-c d i` | Step into |
| `C-c d o` | Step out |
| `C-c d c` | Continue |
| `C-c d q` | Quit debugger |
| `C-c d e` | Eval expression |

---

## Troubleshooting

### "debugpy not found"
```bash
uv pip install debugpy
```

### "IPython not found" (using standard Python REPL)
```bash
uv pip install ipython
```

### "pytest not found"
```bash
uv pip install pytest
```

### Breakpoints not working
1. Ensure `debugpy` is installed in active venv
2. Check venv is activated: look for `[venv:...]` in modeline
3. Restart Emacs if venv was just created

### REPL not using venv Python
1. Verify venv activation: `M-x pyvenv-activate`
2. Restart REPL: `C-u C-c C-z` (with prefix argument)
3. Check `python-shell-interpreter` variable: `C-h v python-shell-interpreter`

### Slow performance
1. Run native compilation: `M-x my/native-compile-packages`
2. Increase Eglot debounce: `(setq eglot-send-changes-idle-time 1.0)`
3. Disable event logging: `(setq eglot-events-buffer-size 0)` (already set)

---

**For basic Python setup (LSP, linting, formatting), see `PYTHON_SETUP.md`**
