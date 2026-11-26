;;; startup-default-tests.el --- Lexical defaults and bootstrap imports -*- lexical-binding: t; -*-
;;; Commentary:
;; Child Emacs processes keep startup defaults independent of ERT's bindings.
;;; Code:
(require 'ert)
(defconst be/defaults-test-root
  (file-name-directory (directory-file-name (file-name-directory load-file-name))))

(defun be/check-startup-forms (forms)
  "Evaluate startup FORMS in a fresh Emacs, reporting its diagnostics on failure."
  (let ((script (make-temp-file "emacs-startup-default-" nil ".el"))
        (output (generate-new-buffer " *startup defaults*")))
    (unwind-protect
        (progn
          (with-temp-file script
            (insert ";;; -*- lexical-binding: t; -*-\n")
            (prin1 `(progn ,@forms) (current-buffer)))
          (unless (zerop (call-process (expand-file-name invocation-name invocation-directory)
                                       nil output nil "-Q" "--batch" "-l" script))
            (ert-fail (with-current-buffer output (buffer-string)))))
      (delete-file script)
      (kill-buffer output))))

(ert-deftest be/global-lexical-default-preserves-file-overrides ()
  (let ((directory (make-temp-file "emacs-lexical-fixtures-" t)))
    (unwind-protect
        (progn
          (dolist (dialect '(lexical dynamic))
            (with-temp-file (expand-file-name (format "%s.el" dialect) directory)
              (insert (format ";;; -*- lexical-binding: %S; -*-\n" (eq dialect 'lexical)))
              (insert "(setq be/lexical-test-closure (let ((captured 42)) (lambda () captured)))\n")))
          (be/check-startup-forms
           `((load ,(expand-file-name "early-init.el" be/defaults-test-root) nil t)
             (setq inhibit-message nil inhibit-redisplay nil)
             (unless (default-value 'lexical-binding) (error "Lexical default is disabled"))
             (with-temp-buffer
               (insert "(setq be/lexical-test-closure (let ((captured 42)) (lambda () captured)))")
               (eval-buffer))
             (unless (= (funcall be/lexical-test-closure) 42)
               (error "Buffer evaluation did not use the lexical default"))
             (load ,(expand-file-name "lexical.el" directory) nil t)
             (unless (= (funcall be/lexical-test-closure) 42)
               (error "Explicit lexical source did not capture scope"))
             (load ,(expand-file-name "dynamic.el" directory) nil t)
             (condition-case nil
                 (progn (funcall be/lexical-test-closure) (error "Explicit dynamic scope was overridden"))
               (void-variable t)))))
      (delete-directory directory t))))

(ert-deftest be/tool-bootstrap-does-not-load-network-libraries ()
  (be/check-startup-forms
   `((setq load-prefer-newer t)
     (add-to-list 'load-path ,(expand-file-name "local-packages" be/defaults-test-root))
     (let ((before (featurep 'map)))
       (require 'be-system)
       (unless (and (eq before (featurep 'map))
                    (not (featurep 'url)) (not (featurep 'json)))
         (error "Installer bootstrap loaded network libraries prematurely"))))))

;;; startup-default-tests.el ends here
