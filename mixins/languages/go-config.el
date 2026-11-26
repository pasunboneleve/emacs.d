;;; go-config.el --- Go support -*- lexical-binding: t; -*-
;;; Commentary:
;; Loaded on first use.
;;; Code:

(use-package go-mode
  :ensure-system-package gopls
  :hook (go-mode . be/eglot-ensure))

(provide 'go-config)
;;; go-config.el ends here
