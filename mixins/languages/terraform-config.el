;;; terraform-config.el --- Terraform support -*- lexical-binding: t; -*-
;;; Commentary:
;; Loaded on first use.
;;; Code:

(use-package terraform-mode
  :ensure-system-package terraform-ls
  :hook (terraform-mode . be/eglot-ensure))

(provide 'terraform-config)
;;; terraform-config.el ends here
