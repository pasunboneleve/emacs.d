;;; hoon-config.el --- Hoon support -*- lexical-binding: t; -*-
;;; Commentary:
;; Loaded on first use.
;;; Code:

(use-package hoon-mode
  :ensure (hoon-mode
           :host github
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

(provide 'hoon-config)
;;; hoon-config.el ends here
