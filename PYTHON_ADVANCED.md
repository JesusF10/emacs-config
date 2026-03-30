# Advanced Python Development Features

This guide covers the advanced Python development features available in your Emacs configuration.

## Table of Contents

1. [Python Commander (Quick Start)](#python-commander)
2. [Virtual Environment Management](#virtual-environment-management)
3. [Interactive Python REPL](#interactive-python-repl)
4. [Jupyter-like Workflow (Code Cells)](#jupyter-like-workflow)
5. [Testing with Pytest](#testing-with-pytest)
6. [Debugging with pdb](#debugging-with-pdb)
7. [Performance Tips](#performance-tips)
8. [Quick Reference Card](#quick-reference-card)

---

## Python Commander

**TL;DR:** Press `C-c p` in any Python buffer to access all Python commands in one place.

### What is Python Commander?

Python Commander is a **unified command hub** that brings together all Python development features into a single, discoverable menu using Emacs' built-in `transient` system (similar to Magit's interface).

### Why Use Python Commander?

- 🎯 **One entry point:** `C-c p` gives you access to everything
- 🗺️ **Discoverable:** Visual menu shows all available commands
- 🧠 **Reduced cognitive load:** No need to memorize 20+ keybindings
- ⚡ **Context-aware:** Only shows commands relevant to your current setup
- 🔄 **Organized:** Commands grouped by functionality (REPL, Cells, Tests, Tools, LSP)

### Quick Start

```
1. Open any Python file
2. Press C-c p
3. Choose category and command from menu
```

### Menu Structure

```
Python Commander (C-c p)
│
├─ 🔄 REPL          → z/c/r/d/l (Shell, Send buffer/region/function/file)
├─ 📦 Cells         → n/p/e/m/% (Navigate, Eval, Mark, Menu)
├─ 🧪 Tests         → t/f/F/T/L (Dispatch, Function, File, All, Last failed)
├─ 🔧 Tools         → R/C/v/V/D (Ruff format/check, Venv activate/deactivate, Detect env)
└─ 📚 LSP           → g/h/R/A/= (Go to def, Hover, Rename, Actions, Format)
```

### Keyboard Navigation

Once inside Python Commander:
- **Press a letter** to execute that command (e.g., `c` to send buffer)
- **Press `q`** to quit the menu
- **Press `C-g`** to cancel

### Example Workflows

**Workflow 1: Quick REPL interaction**
```
C-c p  →  c  (send buffer to REPL)
```

**Workflow 2: Execute cell and navigate**
```
C-c p  →  e  (eval current cell)
C-c p  →  n  (go to next cell)
```

**Workflow 3: Run tests and format**
```
C-c p  →  f  (run test at point)
C-c p  →  R  (format buffer with Ruff)
```

### High-Frequency Commands (Direct Access)

For maximum speed, these commands have **direct keybindings** outside the menu:

| Key | Command | Why Direct? |
|-----|---------|-------------|
| `C-c C-z` | Switch to REPL | Used constantly |
| `C-c C-c` | Send buffer | High frequency |
| `C-c %` | Cell menu | Has own transient |
| `C-c t` | Test prefix | Well organized |
| `C-c l` | LSP/Tools prefix | Well organized |

**Best practice:** Use direct keybindings for daily commands, Python Commander for everything else.

### Context-Aware Features

Python Commander adapts to your setup:

- **Cells section** only appears if `code-cells-mode` is active
- **Tests commands** only appear if `pytest` is installed
- **Tools commands** check for `ruff` availability
- **LSP section** only shows when Eglot/LSP is running
- **Venv commands** only if `pyvenv` is available

This keeps the menu clean and focused on what you can actually use.

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

**Primary access via Python Commander (`C-c p`):**
- `C-c p n` - Go to next cell
- `C-c p p` - Go to previous cell
- `C-c p e` - Execute current cell
- `C-c p m` - Mark/select current cell
- `C-c p a` - Execute all cells above current

**Direct access:**
- `C-c %` - Cell command menu (opens transient with all cell operations)

**Pro tip:** Use `C-c %` for quick access to cell-specific commands, or `C-c p` for unified Python workflow.

### Workflow

1. **Create cells** with `# %%` markers
2. **Position cursor** in a cell
3. **Execute** with `C-c p e` or `C-c %` → `e` (sends to Python REPL)
4. **Navigate** with `C-c p n` / `C-c p p` or use `C-c %` menu

**Pro tip:** Combine with REPL (`C-c C-z`) to see results in a separate window.

**Alternative:** Use `C-c %` to access the full cell menu with additional operations like duplicate, move, copy cells.

---

## Testing with Pytest

### Features

- **Run tests** from Emacs (no terminal switching)
- **Execute** single function, file, or entire suite
- **Re-run failed tests** only
- **Colorized output** in compilation buffer

### Keybindings

**Test prefix (`C-c t`):**
- `C-c t t` - Pytest dispatch menu (choose what to run)
- `C-c t f` - Run test at point (current function)
- `C-c t m` - Run tests in current file
- `C-c t a` - Run all tests in project
- `C-c t l` - Re-run last failed tests
- `C-c t r` - Repeat last pytest command

**Via Python Commander (`C-c p`):**
- `C-c p t` - Pytest dispatch
- `C-c p f` - Run test function
- `C-c p F` - Run test file
- `C-c p T` - Run all tests
- `C-c p L` - Re-run failed tests

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

## Quick Reference Card

### Python Commander Hub

| Key | Menu | Description |
|-----|------|-------------|
| `C-c p` | Python Commander | **Main entry point** - Unified Python command menu |
| `C-c %` | Cell Commands | Cell-specific transient menu |
| `C-c t` | Test Prefix | Pytest commands prefix |
| `C-c l` | LSP/Tools Prefix | LSP and linting/formatting commands |

### High-Frequency Direct Keys

| Key | Command | Category |
|-----|---------|----------|
| `C-c C-z` | Switch to REPL | REPL |
| `C-c C-c` | Send buffer to REPL | REPL |
| `C-c C-r` | Send region to REPL | REPL |
| `C-c C-d` | Send function to REPL | REPL |
| `C-c C-l` | Send file to REPL | REPL |

### Python Commander Commands

After pressing `C-c p`, use these keys:

#### REPL (🔄)
- `z` - Switch to REPL
- `c` - Send buffer
- `r` - Send region
- `d` - Send function
- `l` - Send file

#### Cells (📦) - *only if code-cells-mode active*
- `n` - Next cell
- `p` - Previous cell
- `e` - Eval cell
- `m` - Mark cell
- `%` - Cell menu (transient)
- `a` - Eval all cells above

#### Tests (🧪) - *only if pytest available*
- `t` - Test dispatch menu
- `f` - Test function at point
- `F` - Test current file
- `T` - Test all
- `L` - Test last failed

#### Tools (🔧)
- `R` - Ruff format buffer
- `C` - Ruff check buffer
- `v` - Activate venv
- `V` - Deactivate venv
- `D` - Detect environment

#### LSP (📚) - *only if LSP active*
- `g` - Go to definition
- `h` - Hover documentation
- `R` - Rename symbol
- `A` - Code actions
- `=` - Format buffer

### Workflow Recommendations

**For daily REPL work:**
```
Use direct keys: C-c C-z, C-c C-c, C-c C-r
```

**For cell-based exploration:**
```
C-c % (cell menu) or C-c p (Python Commander)
```

**For testing workflow:**
```
C-c t f/m/a (test prefix) or C-c p t/f/F/T (Commander)
```

**For discovering new commands:**
```
C-c p (shows all available options)
```

**For one-off operations:**
```
C-c p → choose category → press letter
Example: C-c p → R (format with Ruff)
```

### Memory Aid

Think of it as **layers of access**:

1. **Layer 1 (Fastest):** Direct keys for daily tasks (`C-c C-z`, `C-c C-c`)
2. **Layer 2 (Organized):** Prefixes for grouped commands (`C-c t`, `C-c %`)
3. **Layer 3 (Hub):** Commander for everything else (`C-c p`)

**When in doubt, press `C-c p` and explore!**

---

**For basic Python setup (LSP, linting, formatting), see `PYTHON_SETUP.md`**
