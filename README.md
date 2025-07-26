# Emacs Configuration

A clean, modular Emacs configuration using the [Elpaca](https://github.com/progfolio/elpaca) package manager.

## Features

- **Modern Package Management**: Uses Elpaca for fast and reliable package installation
- **Modular Design**: Configuration split into logical modules for easy maintenance
- **Performance Optimized**: Startup performance optimizations included
- **Well Documented**: Each module is documented and easy to understand
- **Beautiful UI**: Doom themes, modern modeline, and visual enhancements
- **Enhanced Navigation**: Fuzzy finding, file management, and quick window switching
- **Smart Editing**: Auto-completion, syntax highlighting, and intelligent features

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
- **[doom-themes](https://github.com/doomemacs/themes)**: Collection of beautiful themes (using Doom One)
- **[nerd-icons](https://github.com/rainstormstudio/nerd-icons.el)**: Modern icon fonts and utilities for displaying file type icons
- **[doom-modeline](https://github.com/seagle0128/doom-modeline)**: Modern, beautiful modeline with rich information display
- **[rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters)**: Color-coded parentheses, brackets, and braces
- **[highlight-indent-guides](https://github.com/DarthFennec/highlight-indent-guides)**: Visual indentation guides
- **[rainbow-mode](https://github.com/emacs-mirror/rainbow-mode)**: Display colors for color codes (CSS, HTML)
- **[beacon](https://github.com/Malabarba/beacon)**: Highlight cursor position when scrolling
- **[smooth-scrolling](https://github.com/aspiers/smooth-scrolling)**: Better, more fluid scrolling experience

### Completion and Navigation
- **[vertico](https://github.com/minad/vertico)**: Performant and minimalistic vertical completion UI
- **[orderless](https://github.com/oantolin/orderless)**: Fuzzy completion style allowing flexible matching
- **[marginalia](https://github.com/minad/marginalia)**: Rich annotations in the minibuffer for better context

### File Management
- **[dirvish](https://github.com/alexluigit/dirvish)**: Modern file manager replacing dired with enhanced visuals and features
- **dired**: Built-in Emacs directory editor (enhanced by dirvish)

### Editor Enhancements
- **[which-key](https://github.com/justbur/emacs-which-key)**: Display available keybindings in popup for better discoverability
- **Enhanced Built-in Features**: Improved backup system, auto-revert, electric pairs, and more

### Window and Navigation Management
- **[ace-window](https://github.com/abo-abo/ace-window)**: Quick window switching with visual overlays
- **winner-mode**: Undo/redo window layout changes
- **windmove**: Navigate between windows with directional keys

### Development Tools (Optional/Template)
These packages are included as commented templates in `lsp.el`:
- **[lsp-mode](https://github.com/emacs-lsp/lsp-mode)**: Language Server Protocol client for intelligent code features
- **[lsp-ui](https://github.com/emacs-lsp/lsp-ui)**: UI enhancements for lsp-mode
- **[company](https://github.com/company-mode/company-mode)**: Text completion framework

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
- **Winner Mode**: Undo/redo window configurations
- **Windmove**: Navigate between windows with arrow keys

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
- Beautiful Doom themes with modern color schemes
- Rich icon support with nerd-icons
- Rainbow-colored brackets and delimiters for better code readability
- Visual indentation guides
- Modern modeline with git status, file info, and project context
- Smooth scrolling and cursor highlighting
- Clean, minimal interface
- Smart font selection (JetBrains Mono → Fira Code → Source Code Pro → Consolas)

### Editor Enhancements
- Modern completion with fuzzy matching
- Auto-completion with rich annotations
- Electric pair mode for brackets
- Enhanced backup system with versioning
- Recent files tracking with quick access
- Auto-revert mode for file changes
- Smart window management and navigation

### Keybindings

#### Configuration Management
- `C-c e r`: Restart emacs
- `C-c e R`: Reload configuration file
- `C-c e i`: Open configuration file
- `C-c e u`: Upgrade all installed packages

#### File and Buffer Navigation
- `C-c C-f`: Traditional find-file
- `C-c F`: Find file in other window

#### File Management (Dirvish)
- `C-c v f`: Open dirvish file manager
- `C-c v s`: Open dirvish in side panel

#### Dirvish Navigation (when active `dirvish-mode-map`)
- `j/k`: Move down/up
- `h/l`: Go to parent directory/enter directory
- `q`: Quit dirvish
- `?`: Show help menu

#### Window and Navigation Management
- `Shift <left>`: Move to left window
- `Shift <right>': Move to right window
- `Shift <up>': Move to upper window
- `Shift <down>': Move to bottom window

#### Visual and UI Features
- Rainbow delimiters: Automatic color-coding of brackets
- Indent guides: Visual indentation lines in programming modes
- Beacon: Cursor highlighting when scrolling
- Color display: See actual colors for CSS/HTML color codes

#### Other Enhancements
- Smart font selection based on availability
- Enhanced backup system with version control
- Improved completion with fuzzy matching
- Which-key popup shows available keybindings after delay

## Requirements

- GNU Emacs 27.1 or later
- Git (for package installation)
- Font support for icons (automatically installed via `nerd-icons-install-fonts`)
- Optional: `ripgrep` for fast project-wide searching
- Optional: `fd` for enhanced file finding (used by dirvish for large directories)

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

### Completion Issues
If you see duplicate completion interfaces:
- This configuration uses Vertico for completion
- Ensure no conflicting completion systems are active
- Restart Emacs if switching from other completion setups

### Performance Issues
If Emacs feels slow:
- Increase `gc-cons-threshold` in `init.el` (already optimized)
- Disable heavy visual features temporarily
- Check for conflicting packages

## Contributing

Feel free to fork this repository and customize it for your needs. If you have improvements or bug fixes, pull requests are welcome!

## Author: Jesus Flores Lacarra
