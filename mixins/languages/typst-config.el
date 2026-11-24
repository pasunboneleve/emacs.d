;;; typst-config.el --- Typst support -*- lexical-binding: t; -*-
;;; Commentary:
;; Loaded on first use.
;;; Code:

(use-package typst-ts-mode
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(typst-ts-mode . ("tinymist")))))
(use-package typst-watch
  :after typst-ts-mode
  :ensure (:host github :repo "pasunboneleve/typst-watch"))
(add-hook 'typst-ts-mode-hook #'be/eglot-ensure)

(provide 'typst-config)
;;; typst-config.el ends here
