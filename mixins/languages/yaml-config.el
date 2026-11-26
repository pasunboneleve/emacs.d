;;; yaml-config.el --- Yaml support -*- lexical-binding: t; -*-
;;; Commentary:
;; Loaded on first use.
;;; Code:

(use-package yaml-mode)
(use-package yaml-pro
  :after yaml-mode
  :hook (yaml-mode . yaml-pro-mode))
(define-derived-mode helm-mode yaml-mode "Helm"
  "Edit Helm templates.")
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(helm-mode "helm_ls" "serve")))
(add-hook 'helm-mode-hook #'be/eglot-ensure)
(dolist (hook '(yaml-mode-hook yaml-ts-mode-hook))
  (add-hook hook #'be/eglot-ensure))
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((yaml-mode yaml-ts-mode) . ("yaml-language-server" "--stdio"))))

(provide 'yaml-config)
;;; yaml-config.el ends here
