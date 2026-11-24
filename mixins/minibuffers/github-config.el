;;; github-config.el --- On-demand GitHub client -*- lexical-binding: t; -*-
;;; Commentary:
;; Consult-gh queries the CLI during loading, so provision it before requiring Lisp.
;;; Code:
(unless (be/ensure-tool 'gh)
  (user-error "The GitHub CLI is unavailable; see *Emacs dependency installation*"))
(use-package consult-gh
  :ensure (:host github :repo "armindarvish/consult-gh")
  :demand t
  :custom (consult-gh-default-clone-directory "~/src/vendor/"))
(provide 'github-config)
;;; github-config.el ends here
