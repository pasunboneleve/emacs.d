;;; dhall-config.el --- Dhall support -*- lexical-binding: t; -*-
;;; Commentary:
;; Loaded on first use.
;;; Code:

(use-package dhall-mode
  :ensure-system-package dhall
  :mode "\\.dhall\\'")

(be/command-tools 'dhall-format-buffer '("dhall"))


(provide 'dhall-config)
;;; dhall-config.el ends here
