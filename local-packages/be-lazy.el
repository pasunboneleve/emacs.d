;;; be-lazy.el --- First-use configuration entry points -*- lexical-binding: t; -*-
;;; Commentary:
;; Delay evaluating package declarations, not just loading installed packages.
;;; Code:
(require 'cl-lib)

(defvar be/loaded-modules nil)
(defvar be/lazy-associations nil)
(defvar be/allow-tool-installation)

(defun be/restore-lazy-modes (&optional _package)
  "Keep every registered entry point ahead of newly installed autoloads."
  (setq auto-mode-alist
        (append be/lazy-associations
                (cl-set-difference auto-mode-alist be/lazy-associations :test #'equal))))

(defun be/load-module (file)
  "Load configuration FILE once and finish its package queue.
Do not mark a failed load as complete; invoking the entry point can retry it."
  (unless (member file be/loaded-modules)
    (let ((previous (mapcar #'car (elpaca--queued)))
          (be/allow-tool-installation
           (and be/allow-tool-installation (not (file-remote-p default-directory)))))
      (load (expand-file-name file user-emacs-directory) nil 'nomessage)
      (elpaca-wait)
      (be/restore-lazy-modes)
      (let ((failed (cl-loop for (id . package) in (elpaca--queued)
                             when (and (not (memq id previous))
                                       (eq (elpaca<-status package) 'failed)) collect id)))
        (when failed
          (error "Packages failed: %S; inspect M-x elpaca-log and retry M-x elpaca-rebuild" failed))))
    (push file be/loaded-modules)))

(defmacro be/lazy-command (command file target)
  "Define COMMAND to load FILE, then invoke TARGET interactively."
  `(defun ,command ()
     ,(format "Load %s and invoke `%s'." file target)
     (interactive)
     (be/load-module ,file)
     (call-interactively #',target)))

(defmacro be/lazy-mode (command regexp file target &optional grammar fallback)
  "Register COMMAND for REGEXP; load FILE before entering TARGET.
When GRAMMAR cannot be installed, use FALLBACK instead of TARGET."
  `(progn
     (defun ,command ()
       ,(format "Load %s and enter `%s'." file target)
       (interactive)
       (condition-case err
           (progn
             (be/load-module ,file)
             (if ,(if grammar `(cl-every #'be/ensure-grammar ',(if (listp grammar) grammar (list grammar))) t)
                 (funcall #',target)
               (funcall #',(or fallback 'fundamental-mode))))
         (error
          (display-warning 'be-lazy (error-message-string err) :warning)
          (funcall #',(or fallback 'fundamental-mode)))))
     (add-to-list 'be/lazy-associations (cons ,regexp #',command))
     (be/restore-lazy-modes)))

(provide 'be-lazy)
;;; be-lazy.el ends here
