;;; elisp-config.el --- Structural Lisp editing -*- lexical-binding: t; -*-
;;; Commentary:
;; Editing Lisp needs Parinfer, not a Common Lisp runtime or REPL.
;;; Code:
(use-package parinfer-rust-mode
  :custom
  (parinfer-rust-auto-download t)
  (parinfer-rust-preferred-mode "paren")
  (parinfer-rust-troublesome-modes '(hungry-delete-mode global-hungry-delete-mode)))
(provide 'elisp-config)
;;; elisp-config.el ends here
