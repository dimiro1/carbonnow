;;; carbonnow.el --- Create beautiful code screenshots using Carbon -*- lexical-binding: t -*-

;; Copyright (C) 2025 Claudemiro A. F. Neto

;; Author: Claudemiro A. F. Neto <dimiro1@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, convenience, multimedia
;; URL: https://github.com/dimiro1/carbon-now-emacs

;;; Commentary:

;; This package provides integration with Carbon (https://carbon.now.sh)
;; for creating beautiful code screenshots directly from Emacs.
;;
;; Features:
;; - Create code screenshots with M-x carbonnow-create-snapshot
;; - Extensive customization of visual styles
;; - Automatic language detection
;; - Support for all Carbon themes and fonts
;;
;; Usage:
;;   1. Select region of code
;;   2. M-x carbonnow-create-snapshot
;;   3. Browser opens with Carbon interface
;;
;; Configuration:
;;   M-x customize-group RET carbonnow RET
;;

;;; Code:

(require 'url)

(defgroup carbonnow nil
  "Customization group for Carbon code screenshot settings."
  :group 'tools
  :prefix "carbonnow-")

;; Custom validation functions
(defun carbonnow--validate-px-value (value)
  "Validate that VALUE is a proper pixel value (e.g., '20px')."
  (if (string-match-p "^[0-9]+px$" value)
      t
    (error "Value must be in pixel format (e.g., '20px')")))

(defun carbonnow--validate-percentage (value)
  "Validate that VALUE is a proper percentage (e.g., '133%')."
  (if (string-match-p "^[0-9]+%$" value)
      t
    (error "Value must be a percentage (e.g., '100%')")))

(defun carbonnow--validate-color (value)
  "Validate that VALUE is a proper color name or hex code."
  (if (or (string-match-p "^#[0-9a-fA-F]\\{6\\}$" value)  ; hex color
          (string-match-p "^[a-zA-Z]+$" value))           ; color name
      t
    (error "Value must be a color name or hex code (e.g., 'gray' or '#FF0000')")))

(defun carbonnow--validate-url (value)
  "Validate that VALUE begins with http:// or https://."
  (if (string-match-p "\\`https?://" value)
      t
    (error "URL must start with http:// or https://")))

;; Custom types with validation
(define-widget 'carbonnow-px-type 'string
  "Custom type for pixel values"
  :validate (lambda (widget)
              (carbonnow--validate-px-value (widget-value widget))))

(define-widget 'carbonnow-percentage-type 'string
  "Custom type for percentage values"
  :validate (lambda (widget)
              (carbonnow--validate-percentage (widget-value widget))))

(define-widget 'carbonnow-color-type 'string
  "Custom type for color values"
  :validate (lambda (widget)
              (carbonnow--validate-color (widget-value widget))))

(define-widget 'carbonnow-url-type 'string
  "Custom type for URL values"
  :validate (lambda (widget)
              (carbonnow--validate-url (widget-value widget))))

;; Customizable variables with validation
(defcustom carbonnow-base-url "https://carbon.now.sh/"
  "Base URL for the Carbon service."
  :type 'carbonnow-url-type
  :group 'carbonnow)

(defcustom carbonnow-titlebar "Made with carbon-now-emacs"
  "Title bar text."
  :type 'string
  :group 'carbonnow)

(defcustom carbonnow-background "gray"
  "Background color for the code screenshot."
  :type 'carbonnow-color-type
  :group 'carbonnow)

(defcustom carbonnow-drop-shadow-blur "68px"
  "Blur radius for the drop shadow."
  :type 'carbonnow-px-type
  :group 'carbonnow)

(defcustom carbonnow-drop-shadow nil
  "Whether to enable drop shadow."
  :type 'boolean
  :group 'carbonnow)

(defcustom carbonnow-drop-shadow-offset-y "20px"
  "Vertical offset for the drop shadow."
  :type 'carbonnow-px-type
  :group 'carbonnow)

(defcustom carbonnow-font-size "18px"
  "Font size for the code."
  :type 'carbonnow-px-type
  :group 'carbonnow)

(defcustom carbonnow-line-height "133%"
  "Line height for the code."
  :type 'carbonnow-percentage-type
  :group 'carbonnow)

(defcustom carbonnow-line-numbers t
  "Whether to show line numbers."
  :type 'boolean
  :group 'carbonnow)

(defcustom carbonnow-watermark nil
  "Whether to show the Carbon watermark."
  :type 'boolean
  :group 'carbonnow)

(defcustom carbonnow-window-theme "sharp"
  "Window theme style."
  :type 'string
  :group 'carbonnow)

(defcustom carbonnow-padding-horizontal "0px"
  "Horizontal padding."
  :type 'string
  :group 'carbonnow)

(defcustom carbonnow-padding-vertical "0px"
  "Vertical padding."
  :type 'string
  :group 'carbonnow)

(defcustom carbonnow-font-family "JetBrains Mono"
  "Font family for the code.
Available options are predefined monospace fonts optimized for code display."
  :type '(choice
          (const "Anonymous Pro")
          (const "Cascadia Code")
          (const "Droid Sans Mono")
          (const "Fantasque Sans Mono")
          (const "Fira Code")
          (const "Hack")
          (const "IBM Plex Mono")
          (const "Inconsolata")
          (const "JetBrains Mono")
          (const "Monoid")
          (const "Source Code Pro")
          (const "Space Mono")
          (const "Ubuntu Mono")
          (string :tag "Custom font"))
  :group 'carbonnow)

;; Add choices for theme
(defcustom carbonnow-theme "synthwave-84"
  "Theme for syntax highlighting.
Available options are various color themes optimized for code display."
  :type '(choice
          (const "3024-night")
          (const "a11y-dark")
          (const "base16-dark")
          (const "base16-light")
          (const "blackboard")
          (const "cobalt")
          (const "dracula")
          (const "duotone-dark")
          (const "hopscotch")
          (const "lucario")
          (const "material")
          (const "monokai")
          (const "night-owl")
          (const "nord")
          (const "oceanic-next")
          (const "one-dark")
          (const "one-light")
          (const "panda-syntax")
          (const "paraiso-dark")
          (const "seti")
          (const "shades-of-purple")
          (const "solarized dark")
          (const "solarized light")
          (const "synthwave-84")
          (const "twilight")
          (const "verminal")
          (const "vscode")
          (const "yeti")
          (const "zenburn")
          (string :tag "Custom theme"))
  :group 'carbonnow)

(defun carbonnow--sanitize-options (options)
  "Convert options alist to format needed for url-build-query-string."
  (when (listp options)
    (mapcar (lambda (pair)
              (list (car pair)
                    (cond
                     ((eq (cdr pair) t) "true")
                     ((eq (cdr pair) nil) "false")
                     (t (format "%s" (cdr pair))))))
            options)))

(defun carbonnow--get-language ()
  "Get the corresponding Carbon language for the current buffer's major mode."
  (let* ((mode-name (symbol-name major-mode))
         (base-name (replace-regexp-in-string
                     "\\(?:-ts\\)?-mode\\'" "" ; Remove -ts-mode or -mode suffix
                     mode-name)))
    (if (string= base-name "fundamental")
	"auto"
      (or (cond
           ((string= base-name "emacs-lisp") "lisp")
           ((string= base-name "sh") "shell")
           ((string= base-name "js2") "javascript")
           ((string= base-name "js") "javascript")
           ((string= base-name "tsx") "jsx")
           ((string= base-name "tuareg") "ocaml")
           ((string= base-name "ess-r") "r")
           ((string= base-name "scss") "sass")
           ((string= base-name "web") "handlebars")
           ((string= base-name "c++") "cpp"))
          base-name
          "auto"))))

(defun carbonnow--build-options-alist ()
  "Build options alist from customized variables."
  (list
   (cons "dsblur" carbonnow-drop-shadow-blur)
   (cons "dyoff"  carbonnow-drop-shadow-offset-y)
   (cons "l"  (carbonnow--get-language))
   (cons "bg" carbonnow-background)
   (cons "ds" carbonnow-drop-shadow)
   (cons "fm" carbonnow-font-family)
   (cons "fs" carbonnow-font-size)
   (cons "lh" carbonnow-line-height)
   (cons "ln" carbonnow-line-numbers)
   (cons "t"  carbonnow-theme)
   (cons "tb" carbonnow-titlebar)
   (cons "wm" carbonnow-watermark)
   (cons "wt" carbonnow-window-theme)
   (cons "ph" carbonnow-padding-horizontal)
   (cons "pv" carbonnow-padding-vertical)))

(defun carbonnow--build-url (code)
  "Build the Carbon URL with current settings and CODE parameter.
CODE is the source code to be rendered in the Carbon screenshot."
  (let* ((options (carbonnow--build-options-alist))
         (sanitized-options (carbonnow--sanitize-options options))
         (code-param (list (list "code" (url-hexify-string code))))
         (all-params (append sanitized-options code-param))
         (query-string (url-build-query-string all-params)))
    (concat carbonnow-base-url "?" query-string)))

;;;###autoload
(defun carbonnow-create-snapshot (start end)
  "Create a Carbonnow snapshot URL from the selected region.
Opens the resulting URL in a web browser."
  (interactive "r")
  (when (use-region-p)
    (let* ((code (buffer-substring-no-properties start end))
           (url (carbonnow--build-url code)))
      (browse-url url))))

(provide 'carbonnow)
;;; carbonnow.el ends here

