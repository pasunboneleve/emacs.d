;;; graphviz-config.el --- Graphviz support -*- lexical-binding: t; -*-
;;; Commentary:
;; Loaded on first use.
;;; Code:

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

(provide 'graphviz-config)
;;; graphviz-config.el ends here
