;;; racket-config.el --- Racket support -*- lexical-binding: t; -*-
;;; Commentary:
;; Loaded on first use.
;;; Code:

(use-package racket-mode
  :ensure-system-package racket
  :mode "\\.rkt\\'"
  :interpreter "racket-mode")

(be/command-tools 'racket-run '("racket"))

(provide 'racket-config)
;;; racket-config.el ends here
