;;; markdown-config.el --- Markdown support -*- lexical-binding: t; -*-
;;; Commentary:
;; Loaded on first use.
;;; Code:

(use-package markdown-mode
  :mode "\\.\\(?:md\\|markdown\\|udon\\)\\'"
  :hook ((markdown-mode . display-line-numbers-mode)
         (markdown-mode . auto-fill-mode)))
(use-package markdown-mermaid
  :custom (markdown-mermaid-mmdc-path "mmdc")
  :ensure (:host github :repo "pasunboneleve/markdown-mermaid")
  :after markdown-mode
  :bind (:map markdown-mode-map ("C-c m" . markdown-mermaid-preview)))
(use-package mermaid-mode
  :mode "\\.mmd\\'")

(be/command-tools 'markdown-mermaid-preview '("mmdc"))

(provide 'markdown-config)
;;; markdown-config.el ends here
