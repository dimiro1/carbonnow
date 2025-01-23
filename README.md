# carbonnow

**carbonnow** is an Emacs package that lets you quickly create beautiful code screenshots using [Carbon](https://carbon.now.sh), all without leaving Emacs.

## Features
- Capture code snippets effortlessly with `M-x carbonnow-create-snapshot`
- Customize themes, fonts, padding, shadows, and other styling options
- Automatically detects the programming language from the major mode
- Supports all Carbon themes and fonts
- Fine-tune appearance with various configuration options

## Installation

Clone the repository and add it to your Emacs setup:

```elisp
(add-to-list 'load-path "/path/to/carbonnow.el")
(require 'carbonnow)
```

If you use `straight.el`, you can install it with:

```elisp
(straight-use-package
 '(carbonnow :type git :host github :repo "dimiro1/carbonnow"))
```

Or with `use-package` and `straight.el`:

```elisp
(use-package carbonnow
  :straight (:type git :host github :repo "dimiro1/carbonnow")
  :bind ("C-c c s" . carbonnow-create-snapshot))
```

## Usage

1. Highlight the code snippet you want to capture.
2. Run `M-x carbonnow-create-snapshot`.
3. Your browser will open with Carbon, pre-filled with your code and settings.

## Customization

Customize the look and feel by running:

```elisp
M-x customize-group RET carbonnow RET
```

Options include:

- `carbonnow-theme`: Pick from various syntax themes.
- `carbonnow-font-family`: Choose from various monospace fonts.
- `carbonnow-font-size`: Adjust text size.
- `carbonnow-line-height`: Tweak line spacing.
- `carbonnow-background`: Set a custom background color.
- `carbonnow-window-theme`: Change the window decoration style.
- `carbonnow-drop-shadow`: Enable or disable drop shadows.
- `carbonnow-line-numbers`: Toggle line numbers.
- `carbonnow-watermark`: Show or hide the Carbon watermark.

## Credits

This package integrates with [Carbon](https://carbon.now.sh), a fantastic tool for sharing beautiful code snippets as images.

