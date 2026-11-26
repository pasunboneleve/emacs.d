;;; rust-config.el --- Rust support -*- lexical-binding: t; -*-
;;; Commentary:
;; Loaded on first use.
;;; Code:

(use-package rust-mode
  :custom (rust-mode-treesitter-derive nil))
(use-package rustic
  :after rust-mode
  :custom
  (rustic-lsp-client 'eglot)
  (rustic-lsp-setup-p nil)
  (rustic-format-on-save t)
  (rustic-cargo-use-last-stored-arguments t)
  :config
  (remove-hook 'rustic-mode-hook #'rustic-setup-lsp)
  (add-hook 'rustic-mode-hook #'be/eglot-ensure))

(provide 'rust-config)
;;; rust-config.el ends here
