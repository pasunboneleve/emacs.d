;;; be-tool-worker.el --- Batch entry point for private installers -*- lexical-binding: t; -*-
;;; Commentary:
;; Launched by be-install; never loaded into the user's interactive Emacs.
;;; Code:
(setq gc-cons-threshold (* 64 1024 1024) gc-cons-percentage 0.1)
(add-to-list 'load-path (file-name-directory load-file-name))
(setq user-emacs-directory (getenv "BE_TOOL_EMACS_DIRECTORY"))
(require 'be-system)
(let ((name (pop command-line-args-left))
      (installer (read (pop command-line-args-left))))
  (unless (be/ensure-tool name installer)
    (when-let ((buffer (get-buffer "*Emacs dependency installation*")))
      (princ (with-current-buffer buffer (buffer-string))))
    (error "Installation failed for %s" name)))
;;; be-tool-worker.el ends here
