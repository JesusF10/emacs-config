# Emacs Configuration

A clean, modular Emacs configuration using the [Elpaca](https://github.com/progfolio/elpaca) package manager.

## Features

- **Modern Package Management**: Uses Elpaca for fast and reliable package installation
- **Modular Design**: Configuration split into logical modules for easy maintenance
- **Performance Optimized**: Startup performance optimizations included
- **Well Documented**: Each module is documented and easy to understand

## Structure

```
.
├── init.el              # Main entry point
├── early-init.el        # Early initialization (optional)
├── lisp/                # Configuration modules
│   ├── packages.el      # Package management (Elpaca setup)
│   ├── ui.el            # UI and appearance settings
│   ├── editor.el        # Editor enhancements
│   ├── keybindings.el   # Custom keybindings
│   ├── lsp.el           # LSP and programming support
│   ├── org-mode.el      # Org-mode configuration
│   └── utils.el         # Utility functions
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

## Packages

This configuration includes the following packages, organized by functionality:

### Core Package Management
- **[Elpaca](https://github.com/progfolio/elpaca)**: Modern package manager for Emacs with better performance and reliability
- **[elpaca-use-package](https://github.com/progfolio/elpaca)**: Integration between Elpaca and use-package syntax

### UI and Appearance
- **[catppuccin-theme](https://github.com/catppuccin/emacs)**: Soothing pastel theme with multiple flavors (using Mocha variant)
- **[nerd-icons](https://github.com/rainstormstudio/nerd-icons.el)**: Modern icon fonts and utilities for displaying file type icons
- **[doom-modeline](https://github.com/seagle0128/doom-modeline)**: Modern, beautiful modeline with rich information display

### Completion and Navigation
- **[vertico](https://github.com/minad/vertico)**: Performant and minimalistic vertical completion UI
- **[orderless](https://github.com/oantolin/orderless)**: Fuzzy completion style allowing flexible matching
- **[marginalia](https://github.com/minad/marginalia)**: Rich annotations in the minibuffer for better context

### File Management
- **[dirvish](https://github.com/alexluigit/dirvish)**: Modern file manager replacing dired with enhanced visuals and features
- **dired**: Built-in Emacs directory editor (enhanced by dirvish)

### Editor Enhancements
- **[which-key](https://github.com/justbur/emacs-which-key)**: Display available keybindings in popup for better discoverability

### Development Tools (Optional/Template)
These packages are included as commented templates in `lsp.el`:
- **[lsp-mode](https://github.com/emacs-lsp/lsp-mode)**: Language Server Protocol client for intelligent code features
- **[lsp-ui](https://github.com/emacs-lsp/lsp-ui)**: UI enhancements for lsp-mode
- **[company](https://github.com/company-mode/company-mode)**: Text completion framework

### Built-in Emacs Features Enhanced
- **Electric Pair Mode**: Automatic bracket/quote pairing
- **Show Paren Mode**: Highlight matching parentheses
- **Global HL Line Mode**: Highlight current line
- **Display Line Numbers Mode**: Show line numbers in programming modes
- **Recent Files Mode**: Track and quickly access recently opened files
- **Save Place Mode**: Remember cursor position in files
- **Auto Revert Mode**: Automatically reload files changed externally

## Customization

Each module in the `lisp/` directory handles a specific aspect of the configuration:

- **packages.el**: Add new packages here using `use-package` syntax
- **ui.el**: Customize themes, fonts, and visual appearance
- **editor.el**: Configure editing behavior and text manipulation
- **keybindings.el**: Add custom keybindings
- **lsp.el**: Configure language servers and programming language support
- **org-mode.el**: Customize Org-mode settings
- **utils.el**: Add utility functions and miscellaneous settings

## Key Features

### Package Management
- Uses Elpaca for modern, fast package management
- Compatible with use-package syntax
- Automatic package installation on startup

### UI Improvements
- Clean, minimal interface
- Line numbers enabled
- Syntax highlighting
- Matching parentheses highlighting

### Editor Enhancements
- Auto-completion
- Electric pair mode for brackets
- Recent files tracking
- Auto-revert mode for file changes

### Keybindings

#### Configuration Management
- `C-c C-r`: Reload configuration
- `C-c C-e`: Open configuration file
- `C-c C-d`: Open configuration directory

#### File and Buffer Navigation
- `C-c f`: Fuzzy find files (`consult-find`)
- `C-c b`: Switch buffers with fuzzy matching (`consult-buffer`)
- `C-c s`: Search within current buffer (`consult-line`)
- `C-c S`: Search across project files (`consult-ripgrep`)
- `C-c C-f`: Traditional find-file
- `C-c F`: Find file in other window

#### File Management (Dirvish)
- `C-c C-f`: Open dirvish file manager
- `C-c C-s`: Open dirvish in side panel

#### Dirvish Navigation (when active)
- `j/k`: Move down/up
- `h/l`: Go to parent directory/enter directory
- `q`: Quit dirvish
- `?`: Show help menu

#### Window Navigation
- `C-c h/j/k/l`: Move between windows (left/down/up/right)

#### Other Enhancements
- Standard Emacs keybindings with modern completion
- Which-key popup shows available keybindings after delay

## Requirements

- GNU Emacs 27.1 or later
- Git (for package installation)
- Font support for icons (automatically installed via `nerd-icons-install-fonts`)
- Optional: `ripgrep` for fast project-wide searching
- Optional: `fd` for enhanced file finding (used by dirvish for large directories)

## Contributing

Feel free to fork this repository and customize it for your needs. If you have improvements or bug fixes, pull requests are welcome!

## Author: Jesus Flores Lacarra
