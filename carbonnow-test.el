;;; carbonnow-test.el --- Tests for carbonnow.el -*- lexical-binding: t -*-

(require 'ert)
(require 'carbonnow)

(ert-deftest carbonnow-test-validate-px ()
  (should (carbonnow--validate-px-value "20px"))
  (should-error (carbonnow--validate-px-value "20"))
  (should-error (carbonnow--validate-px-value "px"))
  (should-error (carbonnow--validate-px-value "20 px")))

(ert-deftest carbonnow-test-validate-percentage ()
  (should (carbonnow--validate-percentage "100%"))
  (should-error (carbonnow--validate-percentage "100"))
  (should-error (carbonnow--validate-percentage "%"))
  (should-error (carbonnow--validate-percentage "100 %")))

(ert-deftest carbonnow-test-validate-color ()
  (should (carbonnow--validate-color "#FF0000"))
  (should (carbonnow--validate-color "gray"))
  (should-error (carbonnow--validate-color "#FF"))
  (should-error (carbonnow--validate-color "gray-100"))
  (should-error (carbonnow--validate-color "123")))

(ert-deftest carbonnow-test-validate-url ()
  (should (carbonnow--validate-url "https://example.com"))
  (should (carbonnow--validate-url "http://example.com"))
  (should-error (carbonnow--validate-url "example.com"))
  (should-error (carbonnow--validate-url "ftp://example.com")))

(ert-deftest carbonnow-test-sanitize-options ()
  (should (equal '((a "true") (b "false") (c "123") (d "test"))
		 (carbonnow--sanitize-options
                  '((a . t) (b . nil) (c . 123) (d . "test")))))
  (should (equal nil (carbonnow--sanitize-options "not-a-list")))
  (should (equal nil (carbonnow--sanitize-options nil))))

(ert-deftest carbonnow-test-build-options-alist ()
  (let ((carbonnow-drop-shadow-blur "68px")
	(carbonnow-drop-shadow-offset-y "20px")
	(carbonnow-background "gray")
	(carbonnow-drop-shadow nil)
	(carbonnow-font-family "JetBrains Mono")
	(carbonnow-font-size "18px")
	(carbonnow-line-height "133%")
	(carbonnow-line-numbers t)
	(carbonnow-theme "synthwave-84")
	(carbonnow-titlebar "Made with carbon-now-emacs")
	(carbonnow-watermark nil)
	(carbonnow-window-theme "sharp")
	(carbonnow-padding-horizontal "0px")
	(carbonnow-padding-vertical "0px"))
    (cl-letf (((symbol-function 'carbonnow--get-language)
               (lambda () "lisp")))
      (should (equal
               '(("dsblur" . "68px")
                 ("dyoff" . "20px")
                 ("l" . "lisp")
                 ("bg" . "gray")
                 ("ds" . nil)
                 ("fm" . "JetBrains Mono")
                 ("fs" . "18px")
                 ("lh" . "133%")
                 ("ln" . t)
                 ("t" . "synthwave-84")
                 ("tb" . "Made with carbon-now-emacs")
                 ("wm" . nil)
                 ("wt" . "sharp")
                 ("ph" . "0px")
                 ("pv" . "0px"))
               (carbonnow--build-options-alist))))))

(ert-deftest carbonnow-test-get-language ()
  (should
   (equal "lisp"
          (with-temp-buffer (emacs-lisp-mode) (carbonnow--get-language))))
  (should
   (equal "go"
          (with-temp-buffer (go-ts-mode) (carbonnow--get-language))))
  (should
   (equal "javascript"
          (with-temp-buffer (js-mode) (carbonnow--get-language))))
  (should
   (equal "python"
          (with-temp-buffer (python-mode) (carbonnow--get-language))))
  (should
   (equal "auto"
          (with-temp-buffer (fundamental-mode) (carbonnow--get-language)))))

(ert-deftest carbonnow-test-build-url ()
  "Test URL building with handling for different url-build-query-string behaviors.
Emacs 30+ encodes '%' as '%' while older versions encodes as '%25'."
  (cl-letf (((symbol-function 'carbonnow--get-language)
             (lambda () "lisp")))
    (let ((result (carbonnow--build-url "(+ 1 2)")))
      (should (or
               (equal result "https://carbon.now.sh/?dsblur=68px&dyoff=20px&l=lisp&bg=gray&ds=false&fm=JetBrains%20Mono&fs=18px&lh=133%&ln=true&t=synthwave-84&tb=Made%20with%20carbon-now-emacs&wm=false&wt=sharp&ph=0px&pv=0px&code=%28%2B%201%202%29")
               (equal result "https://carbon.now.sh/?dsblur=68px&dyoff=20px&l=lisp&bg=gray&ds=false&fm=JetBrains%20Mono&fs=18px&lh=133%25&ln=true&t=synthwave-84&tb=Made%20with%20carbon-now-emacs&wm=false&wt=sharp&ph=0px&pv=0px&code=%2528%252B%25201%25202%2529"))))))

(ert-deftest carbonnow-test-create-snapshot ()
  (let ((opened-url nil))
    (cl-letf (((symbol-function 'browse-url)
               (lambda (url) (setq opened-url url))))
      (with-temp-buffer
	(transient-mark-mode 1)
	(emacs-lisp-mode)
	(insert "(+ 1 2)")
	(set-mark (point-min))
        (goto-char (point-max))
	(carbonnow-create-snapshot (point-min) (point-max))
	(should
	 (equal
	  (carbonnow--build-url "(+ 1 2)")
	  opened-url))))))
