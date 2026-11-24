;;; be-assets.el --- First-use fonts and tree-sitter grammars -*- lexical-binding: t; -*-
;;; Commentary:
;; Grammar releases below generate ABI 14 parsers, supported by Emacs 29+.
;;; Code:
(require 'cl-lib)
(require 'treesit)

(defconst be/grammar-sources
  '((bash "https://github.com/tree-sitter/tree-sitter-bash" "v0.20.5")
    (c "https://github.com/tree-sitter/tree-sitter-c" "v0.20.8")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.20.5")
    (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp" "v0.20.0")
    (css "https://github.com/tree-sitter/tree-sitter-css" "v0.20.0")
    (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
    (html "https://github.com/tree-sitter/tree-sitter-html" "v0.20.0")
    (java "https://github.com/tree-sitter/tree-sitter-java" "v0.20.2")
    (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.4")
    (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc" "v0.21.0")
    (json "https://github.com/tree-sitter/tree-sitter-json" "v0.24.8")
    (python "https://github.com/tree-sitter/tree-sitter-python" "v0.20.4")
    (rust "https://github.com/tree-sitter/tree-sitter-rust" "v0.20.4")
    (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src")
    (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src")
    (typst "https://github.com/uben0/tree-sitter-typst"))
  "Grammar sources and compatible releases; add exceptions here only.")

(defvar be/grammar-failures nil)

(defun be/grammar-compiler (command &rest _)
  "Provision a C++ compiler only when the grammar's scanner needs one."
  (when (equal command "c++")
    (unless (be/ensure-tool 'c++)
      (error "A C++ compiler is required for this grammar"))))

(advice-add 'treesit--call-process-signal :before #'be/grammar-compiler)

(defun be/install-grammar (language directory source)
  "Install LANGUAGE from SOURCE into DIRECTORY across supported Emacs APIs."
  (if (equal (func-arity #'treesit-install-language-grammar) '(1 . 1))
      ;; Emacs 29's public command has no output-directory argument.
      (apply #'treesit--install-language-grammar-1 directory source)
    (treesit-install-language-grammar language directory)))

(defun be/grammar-directory (language source)
  "Return the cache directory for LANGUAGE's SOURCE on this Emacs version."
  (expand-file-name (format "tree-sitter/%s/%s-%s/" emacs-major-version language
                            (or (nth 2 source) "head"))
                    user-emacs-directory))

(defun be/ensure-grammar (language)
  "Install LANGUAGE before selecting its mode; return nil on failure.
Do not repeatedly retry a failed download in the same session."
  (when (treesit-available-p)
    (let* ((source (copy-tree (assq language be/grammar-sources)))
           (revision (and (>= emacs-major-version 30)
                          (alist-get language '((javascript . "v0.23.1")
                                                (jsdoc . "v0.23.2")
                                                (tsx . "v0.23.2")
                                                (typescript . "v0.23.2")))))
           (_ (when revision (setf (nth 2 source) revision)))
           (directory (be/grammar-directory language source)))
      (add-to-list 'treesit-extra-load-path directory)
      (or (and (file-directory-p directory)
               (directory-files directory nil "\\.\\(?:so\\|dylib\\)\\'")
               (treesit-language-available-p language))
          (unless (memq language be/grammar-failures)
            (condition-case err
                (let ((treesit-language-source-alist (list source)))
                  (unless source (error "No grammar source configured for %s" language))
                  (unless (and (be/ensure-tool 'git) (be/ensure-tool 'cc))
                    (error "Compiler or Git unavailable for %s grammar" language))
                  (setcar (cdr source) (be/git-url (cadr source)))
                  (make-directory directory t)
                  (be/with-git-urls (list (cadr source))
                                    #'be/install-grammar language directory source)
                  (unless (treesit-language-available-p language)
                    (error "Grammar %s did not load; check its ABI and source revision" language))
                  t)
              (error
               (push language be/grammar-failures)
               (display-warning 'be-assets (error-message-string err) :warning)
               nil)))))))

(defvar be/fonts-ready nil)

(defun be/ensure-icon-fonts (&optional frame)
  "Install missing icon fonts once a graphical FRAME needs them."
  (when (and (display-graphic-p frame) (not be/fonts-ready))
    (with-selected-frame (or frame (selected-frame))
      (condition-case err
          (progn
            (require 'all-the-icons)
            (require 'nerd-icons)
            (unless (find-font (font-spec :family "all-the-icons"))
              (all-the-icons-install-fonts t))
            (unless (find-font (font-spec :family "Symbols Nerd Font Mono"))
              (nerd-icons-install-fonts t))
            (clear-font-cache)
            (setq be/fonts-ready t))
        (error (display-warning 'be-assets (error-message-string err) :warning))))))

(provide 'be-assets)
;;; be-assets.el ends here
