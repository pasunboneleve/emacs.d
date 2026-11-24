;;; common-lisp-config.el --- Common-Lisp support -*- lexical-binding: t; -*-
;;; Commentary:
;; Loaded on first use.
;;; Code:

(use-package lisp-mode
  :ensure nil
  :mode "\\.cl\\|\\.lisp\\'"
  :hook
  (lisp-mode . be/parinfer-mode)
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

(provide 'common-lisp-config)
;;; common-lisp-config.el ends here
