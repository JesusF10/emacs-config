# Advanced Python Development Features

This guide covers the advanced Python development features available in your Emacs configuration.

## Table of Contents

1. [Virtual Environment Management](#virtual-environment-management)
2. [Interactive Python REPL](#interactive-python-repl)
3. [Jupyter-like Workflow (Code Cells)](#jupyter-like-workflow)
4. [Testing with Pytest](#testing-with-pytest)
5. [Debugging with pdb](#debugging-with-pdb)
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

## Debugging with pdb

### Features

- **Built-in Python debugger** (no external dependencies)
- **Step through code** line by line
- **Inspect variables** in real-time
- **Eval expressions** in debug context
- **Works with any Python installation**

### No Setup Required

`pdb` is part of Python standard library - it's already installed!

### Usage

**Method 1: Add breakpoint in code**

```python
# debug_example.py
def calculate_average(numbers):
    import pdb; pdb.set_trace()  # Execution will stop here
    total = sum(numbers)
    count = len(numbers)
    return total / count

data = [10, 20, 30]
result = calculate_average(data)
```

**Method 2: Run with debugger from start**

```bash
python -m pdb your_script.py
```

### Common pdb Commands

In the pdb prompt:

| Command        | Description                      |
| -------------- | -------------------------------- |
| `n` (next)     | Execute current line, go to next |
| `s` (step)     | Step into function call          |
| `c` (continue) | Continue until next breakpoint   |
| `p variable`   | Print variable value             |
| `pp variable`  | Pretty-print variable            |
| `l` (list)     | Show code around current line    |
| `w` (where)    | Show stack trace                 |
| `u` (up)       | Move up in stack                 |
| `d` (down)     | Move down in stack               |
| `b 10`         | Set breakpoint at line 10        |
| `cl 1`         | Clear breakpoint 1               |
| `q` (quit)     | Exit debugger                    |

### Debugging Workflow in Emacs

**Example script:**

```python
# debug_example.py
def calculate_average(numbers):
    import pdb; pdb.set_trace()
    total = sum(numbers)
    count = len(numbers)
    if count == 0:
        return 0
    return total / count

data = [10, 20, 30]
result = calculate_average(data)
print(f"Average: {result}")

# Test edge case
empty = []
result2 = calculate_average(empty)
```

**Debug in Emacs:**

1. Open file in Emacs
2. Run: `M-x compile RET python debug_example.py RET`
3. When pdb stops at breakpoint, you see:
   ```
   > /path/to/debug_example.py(3)calculate_average()
   -> total = sum(numbers)
   (Pdb)
   ```
4. Type commands in compilation buffer:
   ```
   (Pdb) p numbers
   [10, 20, 30]
   (Pdb) n
   -> count = len(numbers)
   (Pdb) p total
   60
   (Pdb) c
   Average: 20.0
   ```

### Alternative: Python 3.7+ Breakpoint

Modern Python (3.7+) has a better way:

```python
def calculate_average(numbers):
    breakpoint()  # Cleaner than import pdb; pdb.set_trace()
    total = sum(numbers)
    return total / len(numbers)
```

### Pro Tips

**1. Conditional breakpoints:**

```python
if condition:
    import pdb; pdb.set_trace()
```

**2. Post-mortem debugging:**

```python
import pdb
try:
    risky_operation()
except Exception:
    pdb.post_mortem()  # Debug after exception
```

**3. IPython debugger (better pdb):**

```bash
# Install in venv
uv pip install ipdb

# Use in code
import ipdb; ipdb.set_trace()  # Colored output, tab completion
```

### Visual Debugging Alternatives

For a GUI debugging experience similar to VS Code/PyCharm, consider:

**Option 1: realgud (Emacs package)**

- Graphical debugger interface
- Works with pdb, gdb, lldb
- Install: `M-x elpaca-try RET realgud RET`

**Option 2: External debugger**

- VS Code with Remote SSH
- PyCharm Community Edition
- pudb (terminal UI debugger): `uv pip install pudb`

## Quick Reference Card

### Virtual Environments

| Keybinding  | Action                     |
| ----------- | -------------------------- |
| `C-c l v a` | Activate venv              |
| `C-c l v d` | Deactivate venv            |
| `C-c l v w` | Workon (from $WORKON_HOME) |

### REPL

| Keybinding | Action               |
| ---------- | -------------------- |
| `C-c C-z`  | Start/switch to REPL |
| `C-c C-c`  | Send buffer          |
| `C-c C-r`  | Send region          |
| `C-c C-d`  | Send function        |

### Code Cells

| Keybinding  | Action        |
| ----------- | ------------- |
| `C-c C-n`   | Next cell     |
| `C-c C-p`   | Previous cell |
| `C-c C-e`   | Execute cell  |
| `C-c C-SPC` | Mark cell     |

### Testing

| Keybinding | Action        |
| ---------- | ------------- |
| `C-c t t`  | Pytest menu   |
| `C-c t f`  | Test function |
| `C-c t m`  | Test file     |
| `C-c t a`  | All tests     |
| `C-c t l`  | Failed tests  |
| `C-c t r`  | Repeat last   |

### Debugging

| Keybinding                                | Action                 |
| ----------------------------------------- | ---------------------- |
| Built-in pdb                              | Use in code/terminal   |
| `import pdb; pdb.set_trace()`             | Set breakpoint         |
| `breakpoint()`                            | Python 3.7+ breakpoint |
| `M-x compile RET python -m pdb script.py` | Debug from Emacs       |

---

## Troubleshooting

### "IPython not found" (using standard Python REPL)

```bash
uv pip install ipython
```

### "pytest not found"

```bash
uv pip install pytest
```

### pdb not stopping at breakpoints

1. Ensure you're running script (not importing module)
2. Use `python -m pdb script.py` to start with debugger
3. Check breakpoint syntax: `import pdb; pdb.set_trace()` or `breakpoint()`

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
