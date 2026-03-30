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
├── PYTHON_SETUP.md      # Python development setup guide
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

4. **Install icon fonts** (required for proper icon display):
   
   **Method 1 - Automatic (Recommended):**
   ```
   M-x nerd-icons-install-fonts
   ```
   
   **Method 2 - Manual Installation:**
   ```bash
   cd /tmp
   git clone https://github.com/rainstormstudio/nerd-icons.el.git
   cd nerd-icons.el/fonts
   mkdir -p ~/.local/share/fonts
   cp *.ttf ~/.local/share/fonts/
   fc-cache -f -v
   ```
   
   After installing fonts, restart Emacs to see icons properly displayed.

5. **Python Development Setup** (per-project, recommended):
   ```bash
   # Install uv globally (package/env manager)
   curl -LsSf https://astral.sh/uv/install.sh | sh
   
   # For each project, create isolated environment:
   cd your-project/
   uv venv
   source .venv/bin/activate  # Linux/Mac (.venv\Scripts\activate on Windows)
   
   # Install development tools locally
   uv pip install ty ruff
   
   # Install your project dependencies
   uv pip install pandas numpy scikit-learn jupyter  # Example
   
   # Quick init helper: M-x my/init-python-project (inside Emacs)
   ```

6. **Additional Language Servers:
 - Bash
   ``` bash
   # Install Bash Language Server globally
   npm install -g bash-language-server
   ```

## Current Package Configuration

This configuration includes the following packages, organized by functionality:

### Core Package Management
- **[Elpaca](https://github.com/progfolio/elpaca)**: Modern package manager for Emacs with better performance and reliability
- **[elpaca-use-package](https://github.com/progfolio/elpaca)**: Integration between Elpaca and use-package syntax

### UI and Appearance
- **[doom-themes](https://github.com/doomemacs/themes)**: Collection of beautiful themes (using Doom Palenight)
- **[nerd-icons](https://github.com/rainstormstudio/nerd-icons.el)**: Modern icon fonts and utilities for displaying file type icons
- **[rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters)**: Color-coded parentheses, brackets, and braces

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

### Data Science Tooling
- **[csv-mode](https://elpa.gnu.org/packages/csv-mode.html)**: CSV file editing with column alignment
- **[toml-mode](https://github.com/dryman/toml-mode.el)**: TOML configuration file support
- **[yaml-mode](https://github.com/yoshiki/yaml-mode)**: YAML configuration file support
- **[markdown-mode](https://jblevins.org/projects/markdown-mode/)**: Markdown and MDX file editing

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
- **ui.el**: Themes (Doom Palenight), fonts (JetBrains Mono/Fira Code), icons, and visual appearance
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
- Beautiful Doom Palenight theme with modern color schemes
- Rich icon support with nerd-icons
- Rainbow-colored brackets and delimiters for better code readability
- Clean, minimal interface
- Smart font selection (JetBrains Mono → Fira Code → Source Code Pro → Consolas)

### Enhanced Completion and Navigation
- **Vertico**: Vertical completion interface with fuzzy matching
- **Consult**: Enhanced search commands (consult-line, consult-git-grep)
- **Marginalia**: Rich annotations in minibuffer
- **Orderless**: Flexible completion matching

### Data Science Workflow

This configuration is optimized for Data Science work with per-project isolation and modern Rust-based tooling:

#### File Format Support
- **CSV**: Column-aligned editing with `csv-mode` (Tree-sitter enhanced)
- **Markdown/MDX**: Technical documentation and notebooks
- **TOML**: Modern Python project configuration (`pyproject.toml`)
- **YAML**: Data pipelines and configuration files
- **Python**: Full LSP support with type checking and formatting

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
- Font support for icons (automatically installed via `nerd-icons-install-fonts`)

### Optional Dependencies
- **ripgrep**: Fast project-wide searching (used by Consult)

### Python Development Requirements
For full Python development features, install tools **per-project** (recommended):

#### Quick Setup (Recommended)
```bash
# 1. Install uv globally (one-time setup)
curl -LsSf https://astral.sh/uv/install.sh | sh

# 2. For each project, create isolated environment
cd your-project/
uv venv
source .venv/bin/activate  # Linux/Mac (.venv\Scripts\activate on Windows)

# 3. Install development tools locally
uv pip install ty ruff

# 4. Install your project dependencies
uv pip install pandas numpy scikit-learn jupyter  # Example

# 5. Open project in Emacs (auto-detects .venv tools)
emacs .
```

#### Alternative: Use Emacs Helper
Inside Emacs in your project directory:
```
M-x my/init-python-project
```
This automatically runs steps 2-3 above.

#### Why Per-Project Installation?
- **Isolation**: Each project has independent tool versions
- **Reproducibility**: Lock files ensure consistent environments
- **Portability**: Other developers replicate environment with `uv sync`
- **No Global Conflicts**: Project A with ruff 0.4, Project B with ruff 0.6

#### Tool Requirements
- **ty**: Rust-based Python type checker and LSP server (replaces Pyright)
- **ruff**: Fast Python linter and formatter
- **uv**: Python package and environment manager (replaces pip/virtualenv)

The configuration automatically detects and warns if tools are missing from your local `.venv`.

## Troubleshooting

### Icons Not Displaying
If you see square boxes or strange characters instead of icons:

1. **Install icon fonts**:
   ```
   M-x nerd-icons-install-fonts
   ```

2. **Restart Emacs** completely after font installation

3. **Manual font installation** (if automatic fails):
   ```bash
   cd /tmp
   git clone https://github.com/rainstormstudio/nerd-icons.el.git
   cd nerd-icons.el/fonts
   mkdir -p ~/.local/share/fonts
   cp *.ttf ~/.local/share/fonts/
   fc-cache -f -v
   ```

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

## Migration from Pyright to Ty

If you're upgrading from an older version of this configuration that used Pyright:

### Why the Change?
- **Performance**: Ty is 10-100x faster (50ms vs 500ms startup, 20ms vs 200ms per-file analysis)
- **Rust-native**: No Node.js dependency, single binary
- **Unified tooling**: All Python tools (ty, ruff, uv) are Rust-based
- **Better integration**: Native `pyproject.toml` configuration (no `pyrightconfig.json`)

### Migration Steps

1. **Uninstall Pyright** (optional, won't interfere):
   ```bash
   npm uninstall -g pyright
   ```

2. **Install uv globally** (one-time):
   ```bash
   curl -LsSf https://astral.sh/uv/install.sh | sh
   ```

3. **For each existing project**:
   ```bash
   cd your-project/
   
   # Create new .venv with uv (or keep existing)
   uv venv
   source .venv/bin/activate
   
   # Install ty and ruff locally
   uv pip install ty ruff
   
   # Install your existing dependencies
   uv pip install -r requirements.txt  # If you have one
   # OR install directly: uv pip install pandas numpy scikit-learn
   ```

4. **Update configuration files**:
   - **Remove** `pyrightconfig.json` if you have one
   - **Add** to `pyproject.toml` (if needed):
     ```toml
     [tool.ty]
     strict = true
     ignore-missing-imports = ["sklearn", "tensorflow"]
     
     [tool.ruff]
     line-length = 88
     select = ["E", "F", "I"]
     ```

5. **Restart Emacs** in your project directory:
   ```bash
   cd your-project/
   emacs .
   ```

6. **Verify detection**:
   - Open a Python file
   - Check modeline shows "Eglot" (LSP connected)
   - Run `M-x getenv RET PATH` and verify `.venv/bin` is at the start
   - Check `*Messages*` buffer for "Using local ty" message

### Key Differences

| Feature | Pyright (Old) | Ty (New) |
|---------|---------------|----------|
| Runtime | Node.js | Native binary |
| Startup | ~500ms | ~50ms (10x faster) |
| Config | `pyrightconfig.json` | `pyproject.toml` |
| Installation | `npm install -g pyright` | `uv pip install ty` |
| Detection | Global only | Local `.venv` preferred |

### Troubleshooting Migration

**Error: "ty: command not found"**
- Ensure you installed ty in your project's `.venv`
- Restart Eglot: `M-x eglot-reconnect`

**Error: "Eglot can't find server program 'ty'"**
- Check `.venv/bin/ty` exists: `ls -la .venv/bin/ty`
- Verify PATH: `M-x getenv RET PATH` (should start with your project's `.venv/bin`)
- Use helper: `M-x my/init-python-project`

**Old Pyright config not working**
- Ty uses `pyproject.toml` instead of `pyrightconfig.json`
- Migrate settings to `[tool.ty]` section
- Delete `pyrightconfig.json` (no longer needed)

**Performance not improved**
- Ensure Eglot is using local ty, not falling back to global Pyright
- Check `*eglot stderr*` for actual command being run
- Look for "Using local ty from .venv" message in `*Messages*`

## Contributing

Feel free to fork this repository and customize it for your needs. If you have improvements or bug fixes, pull requests are welcome!

## Written by: Jesus Flores Lacarra

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
