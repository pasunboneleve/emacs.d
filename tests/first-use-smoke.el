;;; first-use-smoke.el --- Exercise actual file activation -*- lexical-binding: t; -*-
;;; Commentary:
;; Use the same disposable configuration as bootstrap-smoke.el.
;;; Code:
(load (expand-file-name "bootstrap-smoke.el" (file-name-directory load-file-name)) nil t)
(with-temp-buffer
  (setq buffer-file-name (expand-file-name "sample.json" temporary-file-directory))
  (insert "{\"answer\": 42}\n")
  (set-auto-mode)
  (unless (eq major-mode (if (treesit-available-p) 'json-ts-mode 'js-mode))
    (error "JSON first use selected %s" major-mode)))
(with-temp-buffer
  (setq buffer-file-name (expand-file-name "sample.md" temporary-file-directory))
  (insert "# A document\n")
  (set-auto-mode)
  (unless (eq major-mode 'markdown-mode)
    (error "Markdown first use selected %s" major-mode)))
(cl-letf (((symbol-function 'be/eglot-ensure) #'ignore))
	 (with-temp-buffer
	   (setq buffer-file-name (expand-file-name "Sample.java" temporary-file-directory))
	   (insert "class Sample {}\n")
	   (set-auto-mode)
	   (unless (eq major-mode 'java-mode)
	     (error "Java first use selected %s" major-mode)))
	 (with-temp-buffer
	   (setq buffer-file-name (expand-file-name "sample.jsx" temporary-file-directory))
	   (insert "const greeting = <span>Hello</span>;\n")
	   (set-auto-mode)
	   (unless (eq major-mode 'jtsx-jsx-mode)
	     (error "JavaScript first use selected %s" major-mode))
	   (font-lock-ensure))
	 (with-temp-buffer
	   (setq buffer-file-name (expand-file-name "sample.tsx" temporary-file-directory))
	   (insert "const greeting = <span>Hello</span>;\n")
	   (set-auto-mode)
	   (unless (eq major-mode 'jtsx-tsx-mode)
	     (error "TypeScript first use selected %s" major-mode))
	   (font-lock-ensure)))
(when (or (elpaca-get 'sly) (elpaca-get 'racket-mode))
  (error "Unrelated language packages installed during first use"))
(with-temp-buffer
  (let ((emacs-lisp-mode-hook nil) (prog-mode-hook nil))
    (emacs-lisp-mode))
  (call-interactively #'be/apheleia-mode)
  (unless (bound-and-true-p apheleia-mode)
    (error "Formatting first use did not enable Apheleia")))
(let ((default-directory (elpaca<-source-dir (elpaca-get 'apheleia))))
  (unless (equal (car (process-lines "git" "rev-parse" "--is-shallow-repository")) "true")
    (error "First-use package clone did not use shallow history")))
(princ (format "First-use JSON, Markdown, Java, JavaScript, and TSX passed on Emacs %s\n" emacs-version))
;;; first-use-smoke.el ends here
