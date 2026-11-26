;;; languages.el --- Language entry points -*- lexical-binding: t; -*-
;;; Commentary:
;; This file registers entry points.  Package declarations live in language files.
;;; Code:
(require 'be-lazy)

(be/lazy-command be/apheleia-mode "mixins/formatting.el" apheleia-mode)
(global-set-key (kbd "C-c t a") #'be/apheleia-mode)

(use-package eglot
  :ensure nil
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits t)
  (eglot-sync-connect nil))

(defconst be/language-tools
  '(((python-mode python-ts-mode) . pyright-langserver)
    ((js-mode js-ts-mode jtsx-jsx-mode jtsx-tsx-mode jtsx-typescript-mode)
     . typescript-language-server)
    ((go-mode go-ts-mode) . gopls)
    ((rust-mode rust-ts-mode rustic-mode) . rust-analyzer)
    ((terraform-mode) . terraform-ls)
    ((helm-mode) . helm_ls)
    ((yaml-mode yaml-ts-mode) . yaml-language-server)
    ((typst-ts-mode) . tinymist)
    ((java-mode java-ts-mode) . jdtls)
    ((csharp-mode csharp-ts-mode) . omnisharp)
    ((scala-mode) . metals)
    ((haskell-mode haskell-ts-mode) . haskell-language-server-wrapper)
    ((elm-mode) . elm-language-server))
  "Servers which can be provisioned automatically for Eglot.")

(defun be/eglot-ensure ()
  "Start Eglot only after its configured language server is available."
  (unless (file-remote-p default-directory)
    (let ((tool (cdr (cl-find-if (lambda (entry) (memq major-mode (car entry)))
                               be/language-tools)))
          (buffer (current-buffer))
          (mode major-mode))
      (when (or (null tool)
                (be/ensure-tool
                 tool nil
                 (lambda ()
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (when (eq major-mode mode) (be/eglot-ensure)))))))
        (condition-case err
            (eglot-ensure)
          (error (display-warning 'eglot (error-message-string err) :warning)))))))

;; Built-in editing needs no language package downloads.
(add-hook 'emacs-lisp-mode-hook #'flymake-mode)
(defun be/parinfer-mode ()
  "Enable structural Lisp editing when its optional package is available."
  (interactive)
  (condition-case err
      (progn
        (be/load-module "mixins/languages/elisp-config.el")
        (parinfer-rust-mode 1))
    (error (display-warning 'be-lazy (error-message-string err) :warning))))
(add-hook 'emacs-lisp-mode-hook #'be/parinfer-mode)
(add-to-list 'auto-mode-alist '("\\.avdl\\'" . avro-mode))
(autoload 'avro-mode "avro-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.tq\\'" . technique-mode))
(autoload 'technique-mode "technique-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(?:ini\\|lock\\|service\\|desktop\\)\\'" . conf-mode))

(be/lazy-mode be/python-mode "\\.py\\'" "mixins/languages/python-config.el"
              python-ts-mode python python-mode)
(add-to-list 'interpreter-mode-alist '("python[0-9.]*" . be/python-mode))
(be/lazy-mode be/js-mode "\\.\\(?:[cm]?js\\|jsx\\)\\'" "mixins/languages/web-config.el"
              jtsx-jsx-mode (javascript jsdoc) js-mode)
(be/lazy-mode be/tsx-mode "\\.tsx\\'" "mixins/languages/web-config.el"
              jtsx-tsx-mode (tsx jsdoc) js-mode)
(be/lazy-mode be/ts-mode "\\.[cm]?ts\\'" "mixins/languages/web-config.el"
              jtsx-typescript-mode (typescript jsdoc) js-mode)
(be/lazy-mode be/html-mode "\\.html?\\'" "mixins/languages/web-config.el"
              html-ts-mode html html-mode)
(be/lazy-mode be/css-mode "\\.css\\'" "mixins/languages/web-config.el"
              css-ts-mode css css-mode)
(be/lazy-mode be/json-mode "\\.\\(?:json\\|tfstate\\|jshintrc\\)\\'" "mixins/languages/json-config.el"
              json-ts-mode json js-mode)
(be/lazy-mode be/csharp-mode "\\.cs\\'" "mixins/languages/csharp-config.el"
              csharp-ts-mode c-sharp csharp-mode)
(be/lazy-mode be/csproj-mode "\\.csproj\\'" "mixins/languages/csharp-config.el" csproj-mode)
(be/lazy-mode be/go-mode "\\.go\\'" "mixins/languages/go-config.el" go-mode)
(be/lazy-mode be/rust-mode "\\.rs\\'" "mixins/languages/rust-config.el" rustic-mode)
(be/lazy-mode be/haskell-mode "\\.\\(?:hs\\|lhs\\)\\'" "mixins/languages/haskell-config.el" haskell-mode)
(be/lazy-mode be/cabal-mode "\\.cabal\\'" "mixins/languages/haskell-config.el" haskell-cabal-mode)
(be/lazy-mode be/scala-mode "\\.\\(?:scala\\|sc\\)\\'" "mixins/languages/scala-config.el" scala-mode)
(be/lazy-mode be/java-mode "\\.java\\'" "mixins/languages/java-config.el" java-mode)
(be/lazy-mode be/sql-mode "\\.sql\\'" "mixins/languages/sql-config.el" sql-mode)
(be/lazy-mode be/csv-mode "\\.csv\\'" "mixins/languages/csv-config.el" csv-mode)
(be/lazy-mode be/markdown-mode "\\.\\(?:md\\|markdown\\|udon\\)\\'" "mixins/languages/markdown-config.el" markdown-mode)
(be/lazy-mode be/mermaid-mode "\\.mmd\\'" "mixins/languages/markdown-config.el" mermaid-mode)
(be/lazy-mode be/yaml-mode "\\.\\(?:ya?ml\\|kubeconfig\\)\\'" "mixins/languages/yaml-config.el" yaml-mode)
(be/lazy-mode be/graphviz-mode "\\.\\(?:dot\\|gv\\)\\'" "mixins/languages/graphviz-config.el" graphviz-dot-mode)
(be/lazy-mode be/lisp-mode "\\.\\(?:cl\\|lisp\\)\\'" "mixins/languages/common-lisp-config.el" lisp-mode)
(be/lazy-command be/sly "mixins/languages/common-lisp-config.el" sly)
(be/lazy-mode be/hoon-mode "\\.hoon\\'" "mixins/languages/hoon-config.el" hoon-mode)
(be/lazy-mode be/typst-mode "\\.typ\\'" "mixins/languages/typst-config.el"
              typst-ts-mode typst text-mode)
(be/lazy-mode be/dhall-mode "\\.dhall\\'" "mixins/languages/dhall-config.el" dhall-mode)
(be/lazy-mode be/dockerfile-mode "Dockerfile\\'" "mixins/languages/dockerfile-config.el" dockerfile-mode)
(be/lazy-mode be/elm-mode "\\.elm\\'" "mixins/languages/elm-config.el" elm-mode)
(be/lazy-mode be/handlebars-mode "\\.hbs\\'" "mixins/languages/handlebars-config.el" handlebars-mode)
(be/lazy-mode be/gherkin-mode "\\.feature\\'" "mixins/languages/pickle-config.el" pickle-mode)
(be/lazy-mode be/racket-mode "\\.rkt\\'" "mixins/languages/racket-config.el" racket-mode)
(be/lazy-mode be/theta-mode "\\.theta\\'" "mixins/languages/theta-config.el" theta-mode)
(be/lazy-mode be/terraform-mode "\\.\\(?:tf\\|tfvars\\|tofu\\)\\'" "mixins/languages/terraform-config.el" terraform-mode)
(be/lazy-mode be/vimrc-mode "\\(?:\\.vim\\|vimrc\\)\\'" "mixins/languages/vimrc-config.el" vimrc-mode)
(be/lazy-mode be/udev-mode "\\.rules\\'" "mixins/languages/udev-config.el" udev-mode)

(provide 'languages)
;;; languages.el ends here
