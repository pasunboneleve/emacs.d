;;; Package --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Haskell configuration.
;;; Code:

(use-package haskell-cabal
  :ensure nil
  :hook
  (haskell-mode . envrc-mode)
  (haskell-cabal-mode . format-all-mode)
  (format-all-mode . format-all-ensure-formatter))

(use-package haskell-mode
  :delight "λ"
  :after haskell-font-lock
  :hook
  (haskell-mode . eglot-ensure)
  ;; stack install stylish-haskell
  :custom (haskell-stylish-on-save nil)
  :bind (:map haskell-mode-map
              ("C-c h" . hoogle)
              ("C-c s" . haskell-mode-stylish-buffer)
              ("C-c C-c" . haskell-compile)
              ("C-c t" . (lambda ()
                           (interactive)
                           (save-some-buffers t)
                           (setq-local
                            haskell-compile-cabal-build-command
                            "cabal test"
                            haskell-compile-stack-build-command
                            "stack test")
                           (haskell-compile))))
  :ensure-system-package (fourmolu . "stack install fourmolu"))

(provide 'haskell-config)
;;; haskell-config.el ends here.
