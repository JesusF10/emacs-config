# Installation Guide - Post Update

## What Was Fixed

The previous commit had issues with pseudo-packages that don't exist in package repositories:
- ❌ `python-popup` (not a real package - fixed by using `with-eval-after-load`)
- ❌ `dap-ui` (part of dap-mode, not separate - fixed)

**Status:** ✅ All package issues resolved in commit `6a68151`

---

## New Packages to Install

When you restart Emacs, Elpaca will automatically install these 4 new packages:

1. **pyvenv** - Virtual environment management
2. **code-cells** - Jupyter-like code cells (execute blocks with `# %%`)
3. **python-pytest** - Pytest integration
4. **dap-mode** - Visual debugger with breakpoints

---

## Installation Steps

### 1. Restart Emacs

```bash
# Close any running Emacs instances
killall emacs

# Start Emacs (GUI mode)
emacs &
```

**What will happen:**
- Elpaca will detect new packages in configuration
- Automatic installation will start
- You'll see messages in `*elpaca-log*` buffer
- Installation takes ~30-60 seconds

### 2. Verify Installation

After Emacs finishes loading, check packages:

```elisp
;; Run in Emacs scratch buffer or with M-:
M-x elpaca-status

;; Or manually check each package:
M-: (package-installed-p 'pyvenv) RET       ; should return 't
M-: (package-installed-p 'code-cells) RET   ; should return 't
M-: (package-installed-p 'python-pytest) RET ; should return 't
M-: (package-installed-p 'dap-mode) RET     ; should return 't
```

### 3. Optional: Install Python Dependencies

For full functionality, install these tools **per-project** (not global):

```bash
cd your-python-project/
source .venv/bin/activate  # Activate your venv

# For debugging
uv pip install debugpy

# For IPython REPL (better than standard Python REPL)
uv pip install ipython

# For testing
uv pip install pytest
```

---

## Troubleshooting

### Issue: "Package not found" or "Failed to install"

**Solution 1 - Refresh package archives:**
```elisp
M-x elpaca-try
```

**Solution 2 - Manual installation:**
```elisp
M-x elpaca-try RET pyvenv RET
M-x elpaca-try RET code-cells RET
M-x elpaca-try RET python-pytest RET
M-x elpaca-try RET dap-mode RET
```

**Solution 3 - Check Elpaca log:**
```elisp
M-x elpaca-log
```
Look for error messages in the `*elpaca-log*` buffer.

### Issue: Configuration won't load

**Check syntax errors:**
```bash
emacs --batch --eval "(load-file \"~/.emacs.d/init.el\")" 2>&1 | grep -i error
```

If no errors, the configuration is valid.

### Issue: Keybindings don't work

This happens if packages aren't loaded yet. Solutions:

1. **Open a Python file** to trigger package loading:
   ```
   C-x C-f test.py RET
   ```

2. **Manually load packages:**
   ```elisp
   M-x require RET pyvenv RET
   M-x require RET code-cells RET
   ```

3. **Restart Emacs** and wait for full initialization

### Issue: Native compilation warnings

These are normal and can be ignored:
```
Warning (comp): ... might not be defined at runtime
```

To silence them, they're already configured in `init.el`:
```elisp
(setq native-comp-async-report-warnings-errors nil)
```

---

## What to Expect After Installation

### Immediate Changes

1. **Modeline indicator** - When in a Python buffer with `.venv`, you'll see: `[venv:.venv]`

2. **New keybindings available**:
   - `C-c C-z` - Start Python REPL
   - `C-c t t` - Pytest menu
   - `C-c d b` - Toggle breakpoint
   - `C-c l v a` - Activate venv manually

3. **Code cells detection** - Files with `# %%` markers will enable code-cells-mode

### Performance Improvements

- **Smoother editing** - GC optimizations reduce lag
- **Faster completion** - Minibuffer GC deferral
- **Better LSP** - Eglot optimizations for large projects

To see GC improvements:
```elisp
;; Check current GC settings
M-: gc-cons-threshold RET
;; Should show: 16777216 (16MB)

;; Old value was: 2000000 (2MB)
;; 8x larger = fewer GC pauses
```

---

## Next Steps

Once installed:

1. **Read documentation**:
   - `PYTHON_ADVANCED.md` - Comprehensive guide to new features
   - `PYTHON_SETUP.md` - Basic Python setup (LSP, linting)

2. **Test a feature**:
   ```python
   # test.py
   # %% Cell 1
   import pandas as pd
   
   # %% Cell 2
   df = pd.DataFrame({'a': [1,2,3]})
   print(df)
   ```
   
   - Open in Emacs
   - `C-c C-z` to start REPL
   - `C-c C-e` to execute cell

3. **Try debugging**:
   - Set breakpoint with `C-c d b`
   - Start debugger with `C-c d d`
   - Step through with `C-c d n`

4. **Run tests**:
   - Create a test file: `test_example.py`
   - `C-c t f` to run test at cursor
   - `C-c t a` to run all tests

---

## Verification Checklist

Use this to verify everything works:

```
□ Emacs starts without errors
□ *elpaca-log* shows successful package installations
□ Open Python file shows [venv:...] in modeline (if .venv exists)
□ C-c C-z starts Python REPL
□ C-c t t shows pytest menu (if pytest installed)
□ C-c d b toggles breakpoint indicator
□ M-x my/native-compile-packages is available
□ GC threshold is 16MB: M-: gc-cons-threshold returns 16777216
```

---

## Getting Help

If you encounter issues:

1. **Check logs**:
   ```elisp
   M-x elpaca-log          ; Package installation log
   M-x eglot-events-buffer ; LSP communication log
   M-x messages            ; General Emacs messages
   ```

2. **Test in clean environment**:
   ```bash
   emacs -Q -l ~/.emacs.d/init.el
   ```

3. **Report specific errors** with:
   - Error message from `*Messages*` buffer
   - Output from `M-x emacs-version`
   - Operating system: `uname -a`

---

**Installation complete!** 🎉

Your Emacs is now configured with professional-grade Python development tools.
