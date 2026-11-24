;;; theta-config.el --- Theta support -*- lexical-binding: t; -*-
;;; Commentary:
;; Loaded on first use.
;;; Code:

(use-package theta-mode
  :ensure (theta-mode
           :host github
           :repo "target/theta-idl"
           :branch "stage"
           :files ("emacs/theta-mode.el"))
  :mode ("\\.theta\\'" . theta-mode))

(provide 'theta-config)
;;; theta-config.el ends here
