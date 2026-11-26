;;; check-lisp.el --- Check configuration reader syntax -*- lexical-binding: t; -*-
;;; Commentary:
;; Parse every tracked configuration source without evaluating its side effects.
;;; Code:
(let* ((root (file-name-directory (directory-file-name (file-name-directory load-file-name))))
       (files (append (list (expand-file-name "init.el" root)
                            (expand-file-name "early-init.el" root))
                      (directory-files-recursively (expand-file-name "local-packages" root) "\\.el\\'")
                      (directory-files-recursively (expand-file-name "mixins" root) "\\.el\\'")
                      (directory-files-recursively (expand-file-name "tests" root) "\\.el\\'"))))
  (dolist (file files)
    (with-temp-buffer
      (insert-file-contents file)
      (emacs-lisp-mode)
      (check-parens)
      (goto-char (point-min))
      (condition-case err
          (while t (read (current-buffer)))
        (end-of-file nil)
        (error (error "%s:%d: %s" file (line-number-at-pos) (error-message-string err))))))
  (princ (format "Parsed %d Lisp files\n" (length files))))
;;; check-lisp.el ends here
