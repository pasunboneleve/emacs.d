;;; python-config.el --- Python support -*- lexical-binding: t; -*-
;;; Commentary:
;; Ruff formats and checks; Eglot uses Pyright.
;;; Code:
(defun be/python-uv ()
  "Activate the local Python environment when uv is available."
  (unless (file-remote-p default-directory)
    (when (be/ensure-tool 'uv nil
                          (let ((buffer (current-buffer)))
                            (lambda ()
                              (when (buffer-live-p buffer)
                                (with-current-buffer buffer
                                  (when (derived-mode-p 'python-mode 'python-ts-mode)
                                    (be/python-uv)))))))
      (require 'uv-mode)
      (uv-mode-auto-activate-hook))))

(use-package uv-mode
  :delight " UV"
  :hook ((python-mode . be/python-uv)
         (python-ts-mode . be/python-uv)))
(use-package ruff-format
  :ensure (:host github :repo "JoshHayes/emacs-ruff-format")
  :hook ((python-mode . ruff-format-on-save-mode)
         (python-ts-mode . ruff-format-on-save-mode))
  :ensure-system-package ruff)
(use-package flymake-ruff
  :ensure (:host github :repo "erickgnavar/flymake-ruff")
  :hook ((python-mode . flymake-ruff-load)
         (python-ts-mode . flymake-ruff-load)))
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio"))))
(add-hook 'python-mode-hook #'be/eglot-ensure)
(add-hook 'python-ts-mode-hook #'be/eglot-ensure)
(be/command-tools 'ruff-format-buffer '("ruff"))
(provide 'python-config)
;;; python-config.el ends here
