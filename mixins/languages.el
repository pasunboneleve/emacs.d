;;; languages.el --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Programming language support -- LSP and individual languages.
;;; Code:

;;; formatter

(use-package apheleia
  :bind ("C-c t a" . apheleia-mode)
  ;; :hook
  ;; (apheleia-mode . (lambda () (indent-tabs-mode -1)))
  :config
  ;; Set custom formatting commands
  (dolist (formatter-cmd
           '((shfmt . ("shfmt" "-i" "2" "-ci" "-sr"))
             (bun . ("bun" "format" (or (buffer-file-name) (buffer-name))))))

    (add-to-list apheleia-formatters formatter-cmd))

  ;; Set custom formatters for modes
  (dolist (formatter-mode '((emacs-lisp-mode . lisp-indent)))
    ;; (tsx-ts-mode . prettier)
    ;; (typescript-mode . prettier)
    ;; (typescript-ts-mode . prettier)


    (add-to-list apheleia-mode-alist formatter-mode))
  :ensure-system-package ((shfmt) (bun)))

;;; LSP

(use-package lsp-mode
  ;; lsp does not define this variable by
  ;; default, so we have to set it here
  :custom
  (lsp-enable-snippet nil)
  :init
  (setq
   ;; easier than debugging right now https://emacs-lsp.github.io/lsp-mode/page/file-watchers/
   lsp-enable-file-watchers nil
   ;; give lsp enough memory
   read-process-output-max (* 1024 1024) ;; 1mb)
   ;; Probably overkill, but doesn't hurt.
   ;; LSP should use better deserialisation.
   ;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
   lsp-use-plists t
   ;; Yes, I want to restart
   lsp-restart 'auto-restart
   lsp-enable-folding nil
   lsp-enable-snippet nil)
  :bind-keymap ("s-l" . lsp-command-map)
  :hook (lsp-managed-mode . (lambda ()
                              (add-hook 'before-save-hook 'lsp-format-buffer nil t)
                              (add-hook 'before-save-hook 'lsp-organize-imports nil t))))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-use-webkit nil)
  :config
  (progn
    (define-key lsp-ui-mode-map
                [remap haskell-mode-jump-to-def-or-tag] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map
                [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map
                [remap xref-find-references] #'lsp-ui-peek-find-references))
  :commands lsp-ui-mode)

(use-package lsp-treemacs)

;; eglot

(use-package eglot
  :ensure nil
  :after eldoc
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits t)
  (eglot-sync-connect nil)) ; don't block while connecting to server

(use-package eglot-booster
  :ensure (eglot-booster :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config
  (eglot-booster-mode)
  :ensure-system-package
  (emacs-lsp-booster . "cargo install emacs-lsp-booster"))

;; ein
(use-package ein)
;; jupyter installation (with Rust repl)
;;
;; python -m venv .venv
;; source .venv/bin/activate
;; pip install jupyterlab
;; cargo install evcxr_jupyter
;; evcxr_jupyter --install

;;; languages

(use-package avro-mode
  :ensure nil
  :custom
  (tab-width 4)
  :mode "\\.avdl$")

(use-package technique-mode
  :ensure nil
  :custom
  (tab-width 4)
  :mode "\\.tq$")

;; ini files
(use-package conf-mode
  :ensure nil
  :mode "\\.ini\\'\\|\\.lock\\'\\|\\.service\\'\\|\\.desktop\\'")

;; csv files
(load-file (concat user-emacs-directory "mixins/languages/csv-config.el"))

;; Dhall
(use-package dhall-mode
  :mode "\\.dhall\\'")

(use-package dockerfile-mode)

;; elm
(use-package elm-mode
  :hook (elm-mode . eglot-ensure))

;; go
(use-package go-mode
  :hook
  (go-mode . eglot-ensure)
  :ensure-system-package ("/usr/bin/gopls" . golang-x-tools-gopls))

;; graphviz
(use-package graphviz-dot-mode
  :custom
  (graphviz-dot-mode-indent-width 2)
  (graphviz-dot-auto-indent-on-semi t)
  (graphviz-dot-preview-extension "png")
  :config
  (with-eval-after-load 'markdown-mode
   (require 'markdown-mermaid)
   (define-key markdown-mode-map (kbd "C-c C-c") #'markdown-mermaid-preview))
  :mode ("\\.dot\\'" "\\.gv\\'")
  :bind (:map graphviz-dot-mode-map
              ("C-c p" . graphviz-dot-preview)))

;; mermaid
(use-package markdown-mermaid
  :ensure (markdown-mermaid :type git :host github :repo "pasunboneleve/markdown-mermaid")
  :bind (:map markdown-mode-map
              ("C-c m" . markdown-mermaid-preview)))

;; handlebars
(use-package handlebars-mode)

;; Haskell
(load-file (concat user-emacs-directory "mixins/languages/haskell-config.el"))

;; JSON
(add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))
(use-package json-ts-mode
  :ensure nil
  :mode "\\.tfstate\\|\\.lock\\|\\.json\\|\\.jshintrc\\'"
  :interpreter "json-ts-mode"
  :custom (js-indent-level 2))

;; java
(use-package lsp-java
  :hook (java-mode . eglot-ensure))

;;; javascript & web
(load-file (concat user-emacs-directory "mixins/languages/web-config.el"))

;; Gherkin
(use-package pickle
  :mode "\\.feature\\'"
  :interpreter "pickle-mode")

;; Lisp
(use-package emacs-lisp-mode
  :ensure nil
  :mode "\\.el\\'"
  :hook
  (emacs-lisp-mode . flymake-mode)
  (lisp-mode . aggressive-indent-mode))

(use-package lisp-mode
  :after rainbow-delimiters-mode
  :ensure nil
  :mode "\\.cl\\|\\.lisp\\'"
  :hook
  (lisp-mode . rainbow-delimiters-mode)
  (lisp-interaction-mode . rainbow-delimiters-mode))

(use-package sly
  :init
  (setq inferior-lisp-program "sbcl")
  :hook
  (sly-mode . rainbow-delimiters-mode)
  (sly-mode . electric-pair-mode)
  :bind (:map sly-prefix-map
         ("M-h" . sly-documentation-lookup))
  :ensure-system-package sbcl)

;; elpaca can't build track-changes, which is a dependency
(use-package parinfer-rust-mode
  :delight
  :custom
  (parinfer-rust-troublesome-modes ;; removed electric-pair-mode from list
   '(hungry-delete-mode global-hungry-delete-mode))
  (parinfer-rust-auto-download t)
  (electric-pair-local-mode -1)
  (parinfer-rust-preferred-mode "paren")
  :hook
  (emacs-lisp-mode
   ielm-mode
   lisp-mode
   slime-repl-mode
   lisp-interaction-mode
   racket-mode)
  (before-save . delete-trailing-whitespace))

;; Markdown
(use-package markdown-mode
  :hook
  (markdown-mode . display-line-numbers-mode)
  (markdown-mode . auto-fill-mode)
  (markdown-mode . display-line-numbers-mode)
  :mode "\\.md$\\|\\.markdown\\|\\.udon$"
  :interpreter "markdown-mode"
  :hook flyspell)

(use-package markdown-mermaid
  :ensure nil                    ; It is a local file, not a package
  :after markdown-mode           ; Load this only after markdown-mode loads
  :bind (:map markdown-mode-map  ; Bind this key specifically in markdown buffers
              ("C-c m" . markdown-mermaid-preview)))

(use-package mermaid-mode
  :ensure t
  :defer t
  :mode ("\\.mmd\\'" . mermaid-mode) ;; Load for standalone .mmd files
  :init
  ;; Crucial: Tell markdown-mode to use mermaid-mode for coloring
  ;; inside ```mermaid ... ``` blocks.
  (with-eval-after-load 'markdown-mode
    (add-to-list 'markdown-code-lang-modes '("mermaid" . mermaid-mode))))

;; Python
(load-file (concat user-emacs-directory "mixins/languages/python-config.el"))

;; nix
(use-package nix-mode
  :after magit
  :requires magit-section
  :config
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
  (if (featurep 'lsp-mode)
      (progn  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nil"))
              (lsp-register-client
               (make-lsp-client :new-connection (lsp-stdio-connection '("nil"))
                                :major-modes '(nix-mode)
                                :server-id 'nix))))
  :ensure-system-package (nil . "nix-env -i nil")
  :hook
  (nix-mode . eglot-ensure)
  :mode "\\.nix\\'")

;; Racket
(use-package racket-mode
  :mode "\\.rkt\\'"
  :interpreter "racket-mode")

;; Rust
(use-package rust-mode
  :ensure t
  :custom
  (rust-mode-treesitter-derive t))

(use-package rustic
  :after (rust-mode)
  :config
  (setq rustic-format-on-save t)
  :custom
  (rustic-lsp-client 'eglot)
  (rustic-cargo-use-last-stored-arguments t)
  :ensure-system-package (rustup))

;; scala
(load-file (concat user-emacs-directory "mixins/languages/scala-config.el"))

(use-package sh-script
  :ensure nil
  :mode (("\\.zsh\\'" . sh-mode)
         ("\\.sh\\'" . sh-mode)
         ("zshrc\\'" . sh-mode)
         ("zshenv\\'" . sh-mode))
  :bind (:map sh-mode-map
              ("C-c C-e" . sh-execute-region))
  :custom (sh-basic-offset 2))

(use-package theta-mode
  :ensure (theta-mode
           :host github
           :repo "target/theta-idl"
           :branch "stage"
           :files ("emacs/theta-mode.el"))
  :mode ("\\.theta\\'" . theta-mode))

(use-package yaml-pro
  :after yaml-mode)
(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)
         ("user-data\\'" . yaml-mode)
         ("\\.kubeconfig\\'" . yaml-mode))
  :hook
  (yaml-mode . yaml-pro-mode)
  (helm-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(helm-mode "helm_ls" "serve"))
  :ensure-system-package (helm_ls . "nix-env -i helm-ls"))

(define-derived-mode helm-mode yaml-mode "helm"
  "Major mode for editing kubernetes helm templates.")


(use-package terraform-mode
  :config
  (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls")))
  :hook
  (terraform-mode . eglot-ensure)
  :ensure-system-package terraform-ls)

(use-package udev-mode)

;;; To use the hoon lsp server, build it from
;;; https://github.com/bct/hoon-language-server/tree/fix-issue-30
;;; and install it via [nvm](https://github.com/nvm-sh/nvm).

;;; Point to your planet with the right credentials, of course.

(use-package hoon-mode
  :ensure (hoon-mode
           :host github
           :protocol ssh
           :repo "dmvianna/hoon-mode.el"
           :branch "dev"
           :files (:defaults "*.json"))
  :custom
  (hoon-lsp-enable nil)
  (hoon-lsp-code "lidlut-tabwed-pillex-ridrup")
  (hoon-lsp-planet "zod")
  (hoon-lsp-port "8080")
  :bind (:map hoon-mode-map
              ("C-c r" . hoon-eval-region-in-herb)
              ("C-c b" . hoon-eval-buffer-in-herb))
  :init
  ;; lsp-mode
  (push '(hoon-mode . "hoon") lsp-language-id-configuration)
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "hoon-language-server")
    :activation-fn (lsp-activate-on "hoon")
    :server-id 'hoon-language-server))
  :hook
  (hoon-mode . eldoc-box-hover-mode))
  ;;; This won't work, eglot-code-action-organize-imports must know
  ;;; where the import lines are (they're not at line 1)
;; (hoon-mode . (lambda ()
;;                (add-hook 'before-save-hook
;;                          (apply-partially #'eglot-code-action-organize-imports 1)
;;                          nil 'local)))

(use-package hoon-ts-mode
  :after combobulate
  :ensure (hoon-ts-mode
           :host github
           :repo "urbit-pilled/hoon-ts-mode")
  :custom
  (treesit-font-lock-level 4)
  :init
  (add-to-list 'combobulate-setup-functions-alist
               '(hoon . combobulate-hoon-setup)))

;;; SQL
(load-file (concat user-emacs-directory "mixins/languages/sql-config.el"))

;;; vim
(use-package vimrc-mode)

(provide 'languages)
;;; languages.el ends here.
