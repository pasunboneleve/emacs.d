;;; formatting.el --- On-demand formatting -*- lexical-binding: t; -*-
;;; Commentary:
;; Language-specific tools stay with their language configuration.
;;; Code:
(use-package apheleia
  :commands apheleia-mode
  :config
  (dolist (entry '((emacs-lisp-mode . lisp-indent) (sql-mode . sqlfluff)
                   (jtsx-jsx-mode . prettier-javascript) (jtsx-tsx-mode . prettier-typescript)
                   (jtsx-typescript-mode . prettier-typescript) (scala-mode . scalafmt)))
    (add-to-list 'apheleia-mode-alist entry))
  (require 'be-formatters)
  (advice-add 'apheleia--run-formatter-process :around #'be/apheleia-tools)
  (advice-add 'apheleia-script--formatter-not-available-p :around #'be/apheleia-npx-missing))
(provide 'formatting)
;;; formatting.el ends here
