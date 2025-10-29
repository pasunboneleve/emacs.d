;;; Package --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Python configuration
;;; Code:

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(use-package poetry
  :ensure-system-package (poetry))

(use-package uv-mode
  :delight " UV"
  :hook (python-ts-mode . uv-mode-auto-activate-hook)
  :ensure-system-package (uv))

(use-package python-isort
  :after python
  :hook
  (python . python-isort-on-save-mode)
  (python-ts-mode . python-isort-on-save-mode)
  :ensure-system-package (isort . python3-isort))

(use-package ruff-format
  :ensure (ruff-format
           :type git
           :host github
           :repo "JoshHayes/emacs-ruff-format")
  :hook (python-ts-mode . ruff-format-on-save-mode)
  :ensure-system-package (ruff))

(use-package flymake-ruff
  :ensure (flymake-ruff
           :type git
           :host github
           :repo "erickgnavar/flymake-ruff")
  :ensure-system-package (ruff))

(use-package python-ts-mode
  :mode ("\\.py\\'" . python-ts-mode)
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs '((python-ts-mode . "pyright")))
  :hook
  (python . eglot-ensure)
  :ensure-system-package (pyright-langserver . "uv tool install pyright"))

(provide 'python-config)
;;; python-config.el ends here
