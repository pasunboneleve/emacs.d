;;; elm-config.el --- Elm support -*- lexical-binding: t; -*-
;;; Commentary:
;; Loaded on first use.
;;; Code:

(use-package elm-mode
  :hook (elm-mode . be/eglot-ensure))
;; handlebars

(provide 'elm-config)
;;; elm-config.el ends here
