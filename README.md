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
├── lisp/               # Configuration modules
│   ├── packages.el     # Package management (Elpaca setup)
│   ├── ui.el          # UI and appearance settings
│   ├── editor.el      # Editor enhancements
│   ├── keybindings.el # Custom keybindings
│   ├── lsp.el         # LSP and programming support
│   ├── org-mode.el    # Org-mode configuration
│   └── utils.el       # Utility functions
└── README.md           # This file
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
- `C-c C-r`: Reload configuration
- `C-c C-e`: Open configuration file
- `C-c C-d`: Open configuration directory
- Standard Emacs keybindings with some improvements

## Requirements

- GNU Emacs 27.1 or later
- Git (for package installation)

## Contributing

Feel free to fork this repository and customize it for your needs. If you have improvements or bug fixes, pull requests are welcome!

## License

This configuration is provided as-is under the MIT license. See individual package licenses for their respective terms.
## Author: Jesus Flores Lacarra

## TODO
