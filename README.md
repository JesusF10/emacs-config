# Emacs Configuration

A clean, modular Emacs configuration using the [Elpaca](https://github.com/progfolio/elpaca) package manager with enhanced LSP support and Tree-sitter integration.

## Structure

```
.
├── init.el              # Main entry point
├── early-init.el        # Early initialization
├── lisp/                # Configuration modules
│   ├── packages.el      # Package management (Elpaca setup)
│   ├── ui.el            # UI and appearance settings
│   ├── editor.el        # Editor enhancements and completion
│   ├── keybindings.el   # Custom keybindings
│   ├── lsp.el           # LSP, Tree-sitter, and Python development
│   ├── org-mode.el      # Org-mode configuration
│   └── utils.el         # Utility functions
├── PYTHON_SETUP.md      # Python basic setup guide (LSP, linting, formatting)
├── PYTHON_ADVANCED.md   # Advanced Python features (REPL, debugging, testing, Jupyter)
└── README.md            # This file
```

## Installation

1. **Backup your existing configuration** (if any):

   ```bash
   mv ~/.emacs.d ~/.emacs.d.backup
   ```

2. **Clone this repository**:

   ```bash
   git clone https://github.com/JesusF10/emacs-config.git ~/.emacs.d
   ```

3. **Start Emacs**:

   ```bash
   emacs
   ```

   On first startup, Elpaca will automatically bootstrap and install required packages.

4. **Install icon fonts** (⚠️ CRITICAL - Required for proper icon display):

   Icons are used throughout the UI including:
   - File icons in Dired
   - Flymake error/warning/info indicators in the fringe
   - Mode-line indicators
   - Completion candidates

   **Without these fonts installed, you will see square boxes with hexadecimal codes instead of icons.**

   **Method 1 - Automatic (Recommended):**

   ```
   M-x nerd-icons-install-fonts
   ```

   Press `y` when prompted to download and install the fonts.

   **Method 2 - Manual Installation:**

   ```bash
   cd /tmp
   git clone https://github.com/rainstormstudio/nerd-icons.el.git
   cd nerd-icons.el/fonts
   mkdir -p ~/.local/share/fonts
   cp *.ttf ~/.local/share/fonts/
   fc-cache -f -v
   ```

   **⚠️ IMPORTANT**: After installing fonts, **restart Emacs completely** to see icons properly displayed.

5. **Python Development Setup** (per-project, recommended):

   ```bash
   # Install uv globally (package/env manager)
   curl -LsSf https://astral.sh/uv/install.sh | sh

   # For each project, create isolated environment:
   cd your-project/
   uv venv
   source .venv/bin/activate  # Linux/Mac (.venv\Scripts\activate on Windows)

   # Install development tools locally
   uv add ty ruff

   # Install your project dependencies
   uv add pandas numpy scikit-learn jupyter  # Example

   # Quick init helper: M-x my/init-python-project (inside Emacs)
   ```

6. \*\*Additional Language Servers:

- Bash
  ```bash
  # Install Bash Language Server globally
  npm install -g bash-language-server
  ```

## Current Package Configuration

This configuration includes the following packages, organized by functionality:

### Core Package Management

- **[Elpaca](https://github.com/progfolio/elpaca)**: Modern package manager for Emacs with better performance and reliability
- **[elpaca-use-package](https://github.com/progfolio/elpaca)**: Integration between Elpaca and use-package syntax

### UI and Appearance
- **[doom-themes](https://github.com/doomemacs/themes)**: Collection of beautiful themes (using Doom Material Dark)
- **[doom-modeline](https://github.com/seagle0128/doom-modeline)**: Modern and feature-rich mode-line with icons
- **[nerd-icons](https://github.com/rainstormstudio/nerd-icons.el)**: Modern icon fonts and utilities for displaying file type icons (⚠️ run `M-x nerd-icons-install-fonts` after installation)
- **[nerd-icons-completion](https://github.com/rainstormstudio/nerd-icons-completion)**: Icons in completion candidates (Vertico/Consult)
- **[nerd-icons-dired](https://github.com/rainstormstudio/nerd-icons-dired)**: Minimal icon display in Dired
- **[dashboard](https://github.com/emacs-dashboard/emacs-dashboard)**: Modern startup screen with recent files, projects, and bookmarks
- **[rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters)**: Color-coded parentheses, brackets, and braces
- **[rainbow-mode](https://elpa.gnu.org/packages/rainbow-mode.html)**: Display color codes with their actual colors
- **[beacon](https://github.com/Malabarba/beacon)**: Highlight cursor position when scrolling
- **[git-gutter](https://github.com/emacsorphanage/git-gutter)**: Show git diff indicators in the fringe (added/modified/deleted lines)
- **[indent-guide](https://github.com/zk-phi/indent-guide)**: Visual indentation guides for better code structure visibility
- **Window transparency**: Semi-transparent window (95% opacity) for aesthetic appeal

### Completion and Navigation

- **[vertico](https://github.com/minad/vertico)**: Performant and minimalistic vertical completion UI
- **[orderless](https://github.com/oantolin/orderless)**: Fuzzy completion style allowing flexible matching
- **[marginalia](https://github.com/minad/marginalia)**: Rich annotations in the minibuffer for better context
- **[consult](https://github.com/minad/consult)**: Enhanced search and navigation commands

### File Management

- **Dired**: Built-in file manager (traditional, fast)
- **[nerd-icons-dired](https://github.com/rainstormstudio/nerd-icons-dired)**: Minimal icon display in Dired

### Editor Enhancements

- **[which-key](https://github.com/justbur/emacs-which-key)**: Display available keybindings in popup for better discoverability

### Language Support and Development

- **Eglot**: Built-in LSP client for intelligent code features (configured for Python with Ty)
- **Tree-sitter**: Built-in syntax parsing and highlighting for 20+ languages (Python, Markdown, CSV, TOML, YAML, and more)
- **Ruff Integration**: Python formatting and linting support with async Flymake backend
- **Virtual Environment Detection**: Automatic Python .venv detection and PATH adjustment


### Built-in Emacs Features Enhanced

- **Electric Pair Mode**: Automatic bracket/quote pairing
- **Show Paren Mode**: Highlight matching parentheses
- **Global HL Line Mode**: Highlight current line
- **Display Line Numbers Mode**: Show line numbers globally
- **Recent Files Mode**: Track and quickly access recently opened files
- **Save Place Mode**: Remember cursor position in files
- **Auto Revert Mode**: Automatically reload files changed externally
- **Delete Selection Mode**: Replace selected text when typing
- **Uniquify**: Better buffer names for files with same name

## Customization

Each module in the `lisp/` directory handles a specific aspect of the configuration:

- **packages.el**: Elpaca package manager setup and use-package integration
- **ui.el**: Themes (Doom Material Dark), fonts (JetBrains Mono/Fira Code), icons, Flymake fringe indicators, and visual appearance
- **editor.el**: Completion framework (Vertico/Consult/Marginalia), file management (Dired), and editing behavior
- **keybindings.el**: Custom keybindings and key mappings
- **lsp.el**: Language servers (Eglot+Ty), Tree-sitter parsers (Python/CSV/Markdown/TOML/YAML), Ruff async integration, uv-based environment detection, and Data Science tooling
- **org-mode.el**: Org-mode configuration and settings
- **utils.el**: Utility functions and miscellaneous settings

## Key Features

### Language Server Protocol (LSP) Support

- **Eglot**: Built-in LSP client for modern language features
- **Ty**: Rust-based Python language server (10-100x faster than Pyright) for type checking, completions, diagnostics, and navigation
- **Ruff Integration**: Fast Python linter and formatter with async Flymake backend and auto-format on save
- **Virtual Environment Detection**: Automatically detects local `.venv` directories, adjusts PATH, warns if tools missing
- **Flymake Integration**: Real-time error checking and diagnostics (async, non-blocking)
- **uv Support**: Automatic detection of uv-managed environments with per-project isolation

### Tree-sitter Language Support

Pre-configured for enhanced syntax highlighting and parsing.

### Package Management

- Uses Elpaca for modern, fast package management
- Compatible with use-package syntax
- Automatic package installation on startup

### UI Improvements
- Beautiful Doom Material Dark theme with modern color schemes
- Modern startup dashboard with recent files, projects, and quick navigation
- Rich icon support with nerd-icons (requires `M-x nerd-icons-install-fonts`)
  - File icons in Dired
  - Icons in completion candidates (Vertico/Consult)
  - Mode-line indicators
- Custom Flymake fringe indicators with clear error/warning/info icons
- Git integration visual indicators:
  - Diff markers in fringe (added/modified/deleted lines via git-gutter)
- Code structure enhancements:
  - Rainbow-colored brackets and delimiters for better readability
  - Visual indentation guides for clear code structure
- Window aesthetics:
  - Semi-transparent window (95% opacity)
  - Smooth scrolling
  - Cursor position highlighting (beacon)
- Clean, minimal interface with doom-modeline
- Smart font selection (JetBrains Mono → Fira Code → Source Code Pro → Consolas)

### Enhanced Completion and Navigation

- **Vertico**: Vertical completion interface with fuzzy matching
- **Consult**: Enhanced search commands (consult-line, consult-git-grep)
- **Marginalia**: Rich annotations in minibuffer
- **Orderless**: Flexible completion matching

### Python Development Workflow

This configuration is optimized for professional Python development with **two comprehensive guides**:

📘 **[PYTHON_SETUP.md](PYTHON_SETUP.md)** - Basic setup (LSP, linting, formatting)  
📗 **[PYTHON_ADVANCED.md](PYTHON_ADVANCED.md)** - Advanced features (REPL, debugging, testing, Jupyter-like workflow)

#### Core Features

**Per-project Isolation:**
- Automatic `.venv` detection and activation
- Visual venv indicator in modeline: `[venv:.venv]`
- Tool isolation (ty, ruff, debugpy per project)

**Interactive Development:**
- IPython REPL integration with send-region/function/buffer
- Jupyter-like code cells (execute blocks with `# %%` markers)
- Live variable inspection and evaluation

**Testing & Debugging:**
- Pytest integration (run function/file/all tests)
- Visual debugger with breakpoints (DAP-mode)
- Step through code, inspect variables, eval expressions

**Performance Optimized:**
- Native compilation support (10-30% faster)
- Optimized garbage collection (16MB threshold)
- Async LSP with debouncing (0.5s idle time)

#### File Format Support

- **CSV**: Column-aligned editing (Tree-sitter enhanced)
- **Markdown/MDX**: Technical documentation
- **TOML**: Modern Python config (`pyproject.toml`)
- **YAML**: Data pipelines and config files
- **Python**: Full LSP + type checking + formatting

#### Workflow Example

```bash
# 1. Create project and initialize environment
cd ~/projects/my-analysis
uv venv
source .venv/bin/activate

# 2. Install tools locally (per-project)
uv pip install ty ruff pandas numpy scikit-learn

# 3. Open in Emacs
emacs .

# Emacs auto-detects:
# - .venv/bin/ty    -> Eglot uses local LSP
# - .venv/bin/ruff  -> Flymake uses local linter
# - .venv/bin/python -> Local interpreter
```

#### Quick Setup Helper

Inside Emacs, use `M-x my/init-python-project` to automatically:

1. Create `.venv` with uv
2. Install ty + ruff locally
3. Prepare project for development

#### Benefits Over Global Installation

- **Version Control**: Different projects use different tool versions
- **Reproducibility**: `uv lock` creates deterministic lockfiles
- **Isolation**: Changes in one project don't affect others
- **Portability**: Other developers replicate environment with `uv sync`

### Editor Enhancements

- Modern completion with fuzzy matching
- Auto-completion with rich annotations
- Electric pair mode for brackets
- Enhanced backup system with versioning
- Recent files tracking with quick access
- Auto-revert mode for file changes
- Smart window management and navigation
- Which-key for discoverable keybindings

### Python Development Features

- **LSP Support**: Full language server integration with Ty (Rust-based, 10-100x faster than Pyright)
- **Ruff Integration**: Fast linting with async Flymake backend and auto-format on save
- **Virtual Environment**: Automatic `.venv` detection with PATH adjustment
- **Tree-sitter**: Enhanced syntax highlighting for Python
- **Flymake**: Real-time diagnostics and error checking (non-blocking)
- **uv Integration**: Automatic detection of uv-managed environments

#### Python LSP Keybindings (via `lsp-map` prefix)

- `s`: Start Eglot LSP server
- `r`: Rename symbol
- `a`: Code actions
- `f`: Format buffer
- `R`: Reconnect to server
- `S`: Shutdown server

#### Debugging and Diagnostics

- `d e`: Show Eglot events buffer
- `d l`: Show LSP stderr buffer
- `d b`: Check Flymake backends
- `d r`: Reset Flymake backends

#### Error Navigation

- `e e`: Show project diagnostics
- `e n`: Go to next error
- `e p`: Go to previous error
- `e l`: Show buffer diagnostics
- `e r`: Show running backends

#### Manual Ruff Commands

- `C-c l F`: Format buffer with Ruff
- `C-c l L`: Check buffer with Ruff

### Keybindings

#### LSP and Python Development

All LSP commands are available through the `lsp-map` prefix:

**Core LSP Commands:**

- `s`: Start Eglot LSP server
- `r`: Rename symbol
- `a`: Code actions
- `f`: Format buffer
- `R`: Reconnect to server
- `S`: Shutdown server

**Debugging and Troubleshooting:**

- `d e`: Show Eglot events buffer
- `d l`: Show LSP stderr buffer
- `d b`: Check Flymake backends
- `d r`: Reset Flymake backends

**Error Navigation:**

- `e e`: Show project diagnostics
- `e n`: Go to next error
- `e p`: Go to previous error
- `e l`: Show buffer diagnostics
- `e r`: Show running backends

**Manual Ruff Commands:**

- `C-c l F`: Format buffer with Ruff
- `C-c l L`: Check buffer with Ruff

#### File and Buffer Navigation

- `C-c C-f`: Traditional find-file
- `C-c F`: Find file in other window

#### Dired File Management

- `C-x d`: Open Dired in current directory
- `C-x 4 d`: Open Dired in other window

**Native Dired Keybindings** (when in Dired buffer):

- `n`/`p`: Move to next/previous line
- `RET`: Open file or directory
- `^`: Go to parent directory
- `m`: Mark file
- `u`: Unmark file
- `d`: Flag file for deletion
- `x`: Execute deletions
- `C`: Copy file
- `R`: Rename/move file
- `D`: Delete file immediately
- `+`: Create directory
- `g`: Refresh buffer
- `q`: Quit Dired
- `?` or `h`: Show help

#### Completion and Search (Consult)

- `C-c l`: Search line in current buffer (consult-line)
- `C-c G`: Git grep in project (consult-git-grep)

#### Configuration Management

- `C-c e r`: Restart emacs
- `C-c e R`: Reload configuration file
- `C-c e i`: Open configuration file
- `C-c e u`: Upgrade all installed packages

#### Tree-sitter Management

- `M-x treesit-install-all-languages`: Install all available parsers
- `M-x treesit-check-parsers`: Check parser status
- `M-x treesit-clean-and-reinstall`: Reinstall problematic parser

### Tree-sitter Language Support

#### Installing Tree-sitter Grammars

After initial setup, install language grammars:

```
M-x treesit-install-language-grammar
```

Or install all available grammars at once:

```
M-x treesit-install-all-languages
```

#### Utility Functions

- `M-x treesit-check-parsers`: Check status of installed parsers
- `M-x treesit-clean-and-reinstall`: Reinstall problematic parsers

#### Compatibility Notes (Emacs 29.3)

- **Working parsers**: C++, CSS, Python, JavaScript, TypeScript, YAML, R, Julia, Scala, Go, Java, JSON, HTML, Markdown, TOML, LaTeX, Org
- **Infrastructure**: HCL/Terraform, Nix, Protocol Buffers, reStructuredText
- **Version compatibility issues**: Some parsers (bash, c, rust) may have version mismatches with Emacs 29.3

The configuration automatically remaps major modes to their Tree-sitter equivalents where available.

## Requirements

- GNU Emacs 27.1 or later (29.3+ recommended for full Tree-sitter support)
- Git (for package installation)
- **Nerd Fonts** (REQUIRED for icons): Install via `M-x nerd-icons-install-fonts` after first startup
  - Without this, you'll see square boxes with hexadecimal codes instead of icons
  - Used for file icons in Dired, Flymake error indicators, mode-line, and completion UI

### Optional Dependencies

- **ripgrep**: Fast project-wide searching (used by Consult)



## Troubleshooting

### Icons Not Displaying

If you see square boxes with hexadecimal numbers/symbols instead of icons (in Dired, Flymake indicators, or anywhere else):

1. **Install icon fonts**:

   ```
   M-x nerd-icons-install-fonts
   ```

   Press `y` when prompted to confirm the download and installation.

2. **Restart Emacs completely** after font installation (closing and reopening is required for fonts to load)

3. **Verify font installation**:

   ```bash
   fc-list | grep -i "nerd\|symbols"
   ```

   You should see several "Symbols Nerd Font" entries.

4. **Manual font installation** (if automatic method fails):

   ```bash
   cd /tmp
   git clone https://github.com/rainstormstudio/nerd-icons.el.git
   cd nerd-icons.el/fonts
   mkdir -p ~/.local/share/fonts
   cp *.ttf ~/.local/share/fonts/
   fc-cache -f -v
   ```

   Then restart Emacs.

5. **Still not working?**
   - Ensure you're running Emacs in GUI mode (not terminal)
   - Check that your system supports TrueType fonts
   - Try running `M-x nerd-icons-install-fonts` again if the download was interrupted

### Python LSP Issues

If you encounter LSP connection problems:

1. **Check Python tools availability**:

   ```bash
   which ty        # Should return .venv/bin/ty or global path
   which ruff      # Should return .venv/bin/ruff or global path
   ```

2. **Virtual environment detection**:
   - Ensure you have a `.venv` directory in your project root
   - Check Emacs detected it: `M-x getenv RET PATH`
   - You should see `.venv/bin` at the start of PATH
   - If not detected, restart Emacs in the project directory

3. **Missing tools warning**:
   - If you see "ty not found" or "ruff not found" warnings:
     ```bash
     cd your-project/
     source .venv/bin/activate
     uv pip install ty ruff
     ```
   - Restart Eglot: `M-x eglot-reconnect`

4. **LSP server crashes**:
   - Check `*eglot stderr*` buffer for error messages
   - Use `M-x eglot-reconnect` to restart the server
   - Use `M-x eglot-shutdown` then `M-x eglot` to fully restart

5. **Ty not found error**:

   ```bash
   # Install in current project
   cd your-project/
   source .venv/bin/activate
   uv pip install ty

   # Or use Emacs helper
   M-x my/init-python-project
   ```

6. **Performance issues**:
   - Ty is 10-100x faster than Pyright for large projects
   - If LSP feels slow, check `*eglot stderr*` for errors
   - Ensure ty is installed locally (not falling back to global/missing)
   - Run native compilation: `M-x my/native-compile-packages` (one-time, 5-10 min)

7. **Advanced Python features troubleshooting**:
   - **Debugger not working**: Install `debugpy` in venv: `uv pip install debugpy`
   - **Tests not running**: Install pytest: `uv pip install pytest`
   - **REPL using wrong Python**: Restart with `C-u C-c C-z` or reactivate venv
   - **Code cells not detected**: Ensure cells start with `# %%` (with space)
   - See [PYTHON_ADVANCED.md](PYTHON_ADVANCED.md) for detailed troubleshooting

### Tree-sitter Parser Issues

If syntax highlighting is broken:

1. **Check parser status**:

   ```
   M-x treesit-check-parsers
   ```

2. **Reinstall problematic parsers**:

   ```
   M-x treesit-clean-and-reinstall
   ```

3. **Version compatibility**: Some parsers may not work with Emacs 29.3. Check the output of `treesit-check-parsers` for errors.

### Completion Issues

If you see duplicate completion interfaces:

- This configuration uses Vertico + Consult + Marginalia for completion
- Ensure no conflicting completion systems are active
- Restart Emacs if switching from other completion setups

### Performance Issues

If Emacs feels slow:

- Increase `gc-cons-threshold` in `init.el` (already optimized)
- Disable heavy visual features temporarily
- Check for conflicting packages
- Use `M-x profiler-start` to identify performance bottlenecks

## Contributing

Feel free to fork this repository and customize it for your needs. If you have improvements or bug fixes, pull requests are welcome!

## Author: Jesus Flores Lacarra

⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⣀⣠⣤⡤⠤⠴⠶⠦⣤⣤⣤⣀⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⣀⣀⣀⣀⠀⠀⢀⣠⠶⢞⣻⣷⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⢀⣠⣶⠾⠛⠋⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠙⠛⠲⠶⢤⣤⣤⣤⣀⣀⣀⣠⣴⡾⠿⠛⠛⠋⠉⣉⣉⣉⣉⡿⠋⣡⣾⣿⣿⡿⠀⠀⠀
⠀⠀⠀⠀⠀⢀⣠⣴⠟⠋⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠚⠉⠉⠉⠉⠉⠛⠛⠀⠀⠀⠀⠶⣿⣿⣿⣿⡟⢁⣾⣿⡿⢋⣿⣿⡆⠀⠀
⠀⠀⠀⢠⣴⠿⠋⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣀⠀⠈⠙⠃⠘⠛⠋⠀⣼⣿⡿⠁⠀⠀
⠀⠀⢠⣾⠋⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⠏⠀⠀⠀⠀⠀⠀⠀⠀⠙⣿⡇⠀⠀⠀
⠀⠀⣿⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⠀⠀⠀⠀⣀⡀⠀⠀⠀⠀⠈⢷⠀⠀⠀
⠀⣼⠏⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⠀⠀⠀⠀⣼⣿⣿⣷⣦⣤⡄⢸⠀⠀⠀
⢰⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢹⣇⠀⠀⠀⣿⣿⣿⣿⣿⣿⣷⡾⠀⠀⠀
⢠⡿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⡖⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣸⣿⣷⣄⠀⠈⠙⠿⣿⣿⠉⢠⡇⠀⠀⠀
⣸⡇⠀⠀⠀⠀⠀⠀⠀⠀⢀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⣷⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⡿⠛⠛⠿⣷⡄⠀⠀⠈⠁⠀⠀⣇⠀⠀⠀
⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⣧⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⣿⡅⠀⠀⠀⠈⠻⣄⠀⠀⠀⠀⠀⠸⡄⠀⠀
⢻⡇⠀⠀⠀⠀⠀⠀⠀⠀⢸⣧⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⣷⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣼⣿⣿⣄⠀⠀⠀⠀⠹⣧⡀⠀⢀⣤⣤⣷⠀⠀
⠸⣧⠀⣰⣶⣶⣶⣦⣦⣤⣬⣿⣆⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⣿⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢰⣿⣿⣿⣿⣦⣀⠀⠀⣠⣿⣿⣶⣾⣿⣯⣁⠀⠀
⠀⠸⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣆⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⣷⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡞⠉⠉⠉⠉⠉⠉⠉⠛⠛⠛⠿⢿⣿⣿⣿⠿⠿⣦
⠀⠀⢻⣿⠟⣩⣴⣶⣿⣿⣷⣾⣿⡈⢧⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⠀⠈⠳⢤⠀⠀⠀⠀⠀⠀⠀⠀⢸⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠈⣇⣾⣿⣿⣿⣿⣿⣿⣿⣿⣧⠀⠳⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⠀⠀⠀⠈⢳⡄⠀⠀⠀⠀⠀⣠⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠸⣿⣿⡿⠛⠉⣠⣤⣤⣬⣽⣇⠀⠹⣆⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⠀⠀⠀⠀⠀⢹⣦⣄⠀⠀⡼⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⢿⡟⠀⣴⣿⣿⣿⣿⣿⣿⣿⣧⡀⠘⢦⠀⠀⠀⠀⠀⠀⢀⣾⠃⠀⠀⠀⠀⠀⠀⢿⣿⣆⢸⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠘⣆⢸⣿⣿⣿⣿⠟⠋⠉⠉⠉⠻⣄⠈⢷⣄⠀⠀⠀⢠⡾⠁⠀⠀⣀⣀⣀⠀⠀⠘⣿⣿⣿⣧⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠈⠻⣿⣿⡟⠁⠀⢀⣠⣤⣶⣶⣿⣆⠀⢻⣶⣶⣶⣿⣶⣶⣿⣿⣿⣿⣿⣿⣄⠀⠈⠛⠿⠿⠿⠷⠆⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠈⠻⣄⠀⢠⣿⣿⣿⣿⡿⠿⠿⣦⠈⠛⠿⠿⠿⠿⠿⠿⣿⣿⣿⣍⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠳⢿⣿⣿⡿⠃⠀⠀⣀⣬⣇⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠉⠓⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠹⣿⠁⠀⢠⣾⣿⣿⡿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠳⠤⢿⣿⣿⠟⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
