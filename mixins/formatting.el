;;; formatting.el --- On-demand formatting -*- lexical-binding: t; -*-
;;; Commentary:
;; Language-specific tools stay with their language configuration.
;;; Code:
(use-package apheleia
  :commands apheleia-mode
  :config
  (add-to-list 'apheleia-mode-alist '(emacs-lisp-mode . lisp-indent)))
(provide 'formatting)
;;; formatting.el ends here
