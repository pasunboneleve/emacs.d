;;; pickle-config.el --- Pickle support -*- lexical-binding: t; -*-
;;; Commentary:
;; Loaded on first use.
;;; Code:

(use-package pickle
  :mode "\\.feature\\'"
  :interpreter "pickle-mode")
;; Racket

(provide 'pickle-config)
;;; pickle-config.el ends here
