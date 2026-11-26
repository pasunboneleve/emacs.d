;;; Package --- Summary  -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; early init file
;;; Code:

;; Undeclared Lisp uses lexical scope; explicit file-local choices still win.
(setq-default lexical-binding t)

;;; no secrets should get saved in plaintext
(setq auth-sources '("secrets:Login" "~/.authinfo.gpg"))

;; Cold package installation can outlast startup; keep GC active throughout it.
(setq gc-cons-threshold (* 64 1024 1024)
      gc-cons-percentage 0.1)

;; Configuration updates must supersede compiled copies in every startup mode.
(setq load-prefer-newer t)

(unless (or (daemonp) noninteractive)
  ;; Site files tend to use `load-file', which emits "Loading X..." messages in
  ;; the echo area, which in turn triggers a redisplay. Redisplays can have a
  ;; substantial effect on startup times and in this case happens so early that
  ;; Emacs may flash white while starting up.
  (define-advice load-file (:override (file) silence)
    (load file nil 'nomessage))

  ;; Undo our `load-file' advice above, to limit the scope of any edge cases it
  ;; may introduce down the road.
  (define-advice startup--load-user-init-file (:before (&rest _) init-doom)
    (advice-remove #'load-file #'load-file@silence)))

;;; disable package.el
(setq package-enable-at-startup nil)

;; Premature redisplays can substantially affect startup times and produce
;; ugly flashes of unstyled Emacs.
(setq-default inhibit-redisplay t
              inhibit-message t)
;; We'll reset it later in init.el

(provide 'early-init)
;;; early-init.el ends here
