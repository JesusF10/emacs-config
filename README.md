# Emacs Configuration

A clean, modular Emacs configuration using the [Elpaca](https://github.com/progfolio/elpaca) package manager with enhanced LSP support and Tree-sitter integration.

## Features

- **Modern Package Management**: Uses Elpaca for fast and reliable package installation
- **Modular Design**: Configuration split into logical modules for easy maintenance
- **Performance Optimized**: Startup performance optimizations included
- **Well Documented**: Each module is documented and easy to understand
- **Beautiful UI**: Doom themes, modern modeline, and visual enhancements
- **Enhanced Navigation**: Fuzzy finding with Vertico, Consult, and Dirvish file management
- **Smart Editing**: Auto-completion, syntax highlighting, and intelligent features
- **Tree-sitter Support**: Modern syntax highlighting and parsing for 20+ languages
- **LSP Integration**: Language Server Protocol support with Eglot for Python (Pyright) and Ruff formatting
- **Virtual Environment Detection**: Automatic Python virtual environment discovery and activation

## Structure

```
.
├── init.el              # Main entry point
├── early-init.el        # Early initialization (optional)
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

5. **Python Development Setup** (optional):
   ```bash
   # Install Python LSP tools globally
   npm install -g pyright
   
   # Or install in your project virtual environment
   pip install python-lsp-server ruff
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
- **[dirvish](https://github.com/alexluigit/dirvish)**: Modern file manager replacing dired with enhanced visuals and features

### Editor Enhancements
- **[which-key](https://github.com/justbur/emacs-which-key)**: Display available keybindings in popup for better discoverability

### Language Support and Development
- **Eglot**: Built-in LSP client for intelligent code features (configured for Python with Pyright)
- **Tree-sitter**: Built-in syntax parsing and highlighting for 20+ languages
- **Ruff Integration**: Python formatting and linting support
- **Virtual Environment Detection**: Automatic Python venv activation

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
- **editor.el**: Completion framework (Vertico/Consult/Marginalia), file management (Dirvish), and editing behavior
- **keybindings.el**: Custom keybindings and key mappings
- **lsp.el**: Language servers (Eglot+Pyright), Tree-sitter parsers, Ruff integration, and Python development
- **org-mode.el**: Org-mode configuration and settings
- **utils.el**: Utility functions and miscellaneous settings

## Key Features

### Language Server Protocol (LSP) Support
- **Eglot**: Built-in LSP client for modern language features
- **Pyright**: TypeScript-based Python language server for completions, diagnostics, and navigation
- **Ruff Integration**: Fast Python linter and formatter with manual formatting commands
- **Virtual Environment Detection**: Automatically detects and activates `.venv` in project directories
- **Flymake Integration**: Real-time error checking and diagnostics

### Tree-sitter Language Support
Pre-configured for enhanced syntax highlighting and parsing:

#### Core Programming Languages
- **Python**: Full development support with LSP and Tree-sitter
- **JavaScript/TypeScript**: Modern web development with TSX support
- **C/C++**: Systems programming
- **Go**: Cloud-native development
- **Java**: Enterprise applications
- **Rust**: Memory-safe systems programming (commented due to version issues)

#### Data Science & Analytics
- **R**: Statistical computing and data analysis
- **Julia**: High-performance scientific computing
- **Scala**: Big data processing with Apache Spark

#### Web Development
- **HTML/CSS**: Frontend development
- **JSON**: Configuration and data files

#### DevOps & Infrastructure
- **YAML**: Configuration files and CI/CD
- **HCL/Terraform**: Infrastructure as code
- **Dockerfile**: Container definitions
- **Protocol Buffers**: API definitions

#### Documentation & Markup
- **Markdown**: Documentation and notes
- **LaTeX**: Academic documents
- **Org**: Emacs org-mode files
- **reStructuredText**: Python documentation

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
- **Dirvish**: Modern file manager with enhanced visuals

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
- **LSP Support**: Full language server integration with Pyright
- **Ruff Integration**: Fast linting and formatting
- **Virtual Environment**: Automatic detection and activation
- **Tree-sitter**: Enhanced syntax highlighting
- **Flymake**: Real-time diagnostics and error checking

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

#### File Management (Dirvish)
- `C-c v f`: Open dirvish file manager
- `C-c v s`: Open dirvish in side panel

#### Dirvish Navigation (when active)
- `j/k`: Move down/up
- `h/l`: Go to parent directory/enter directory
- `q`: Quit dirvish
- `?`: Show help menu

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
- **fd**: Enhanced file finding (used by Dirvish for large directories)

### Python Development Requirements
For full Python development features, see **[PYTHON_SETUP.md](PYTHON_SETUP.md)** for detailed installation instructions:

#### Global Installation (Recommended)
```bash
# Install Pyright globally
npm install -g pyright

# Optional: Install Python tools globally
pip install ruff python-lsp-server
```

#### Virtual Environment Installation
```bash
# In your project directory
python -m venv .venv
source .venv/bin/activate  # Linux/Mac
# .venv\Scripts\activate  # Windows

# Install Python development tools
pip install ruff python-lsp-server
```

The configuration automatically detects and activates `.venv` directories in your project root.

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
   which pyright-langserver  # Should return a path
   which ruff                # Should return a path
   ```

2. **Virtual environment issues**:
   - Ensure you start Emacs from an activated virtual environment, or
   - Place a `.venv` directory in your project root
   - The configuration automatically detects `.venv` directories

3. **LSP server crashes**: 
   - Check `*eglot stderr*` buffer for error messages
   - Use `M-x eglot-reconnect` to restart the server
   - Use `M-x eglot-shutdown` then `M-x eglot` to fully restart

4. **pyright not found error**:
   ```bash
   # Install globally
   npm install -g pyright
   ```

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
