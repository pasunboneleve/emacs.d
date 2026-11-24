;;; racket-config.el --- Racket support -*- lexical-binding: t; -*-
;;; Commentary:
;; Loaded on first use.
;;; Code:

(use-package racket-mode
  :mode "\\.rkt\\'"
  :interpreter "racket-mode")

(provide 'racket-config)
;;; racket-config.el ends here
