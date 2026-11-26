;;; bootstrap-smoke.el --- Fresh-home integration check -*- lexical-binding: t; -*-
;;; Commentary:
;; EMACS_TEST_DIRECTORY must name an isolated copy of the tracked configuration.
;;; Code:
(require 'cl-lib)
(setq user-emacs-directory (file-name-as-directory (or (getenv "EMACS_TEST_DIRECTORY")
                                                       (error "Set EMACS_TEST_DIRECTORY"))))
(setq use-package-expand-minimally t)
(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (startup-redirect-eln-cache (expand-file-name "eln-cache/" user-emacs-directory)))
(defvar be/smoke-config-errors nil)
(advice-add 'display-warning :before
            (lambda (_type text &rest _)
              (when (string-match-p "Config Error" (format "%s" text))
                (push text be/smoke-config-errors))))
(add-hook 'elpaca-error-functions
          (lambda (_package err)
            (write-region (format "%S\n" err) nil
                          (expand-file-name "bootstrap-errors.log" user-emacs-directory)
                          t 'silent)
            nil))
(load (expand-file-name "early-init.el" user-emacs-directory) nil t)
(unless (and (<= gc-cons-threshold (* 64 1024 1024))
             (<= gc-cons-percentage 0.1))
  (error "Startup disables or excessively delays garbage collection"))
(setq inhibit-message nil inhibit-redisplay nil)
(defvar be/smoke-progress-timer
  (run-with-timer
   5 15
   (lambda ()
     (when (fboundp 'elpaca--queued)
       (with-temp-file (expand-file-name "bootstrap-status.el" user-emacs-directory)
         (prin1 (cl-loop for (id . e) in (elpaca--queued)
                         unless (eq (elpaca<-status e) 'finished)
                         collect (cons id (elpaca<-status e))) (current-buffer))
         (terpri (current-buffer))
         (dolist (entry (elpaca--queued))
           (when (eq (elpaca<-status (cdr entry)) 'failed)
             (prin1 (cons (car entry)
                          (mapcar #'elpaca-event<-payload (elpaca-event-log (car entry))))
                    (current-buffer))
             (terpri (current-buffer))))
         (let ((standard-output (current-buffer))) (backtrace)))))))
(let ((after-init-time nil))
  (load (expand-file-name "init.el" user-emacs-directory) nil t))
(elpaca-wait)
(cancel-timer be/smoke-progress-timer)
(setq inhibit-message nil inhibit-redisplay nil)
(when be/smoke-config-errors
  (error "Package configuration failed: %S" be/smoke-config-errors))
(let ((failed (cl-loop for (id . e) in (elpaca--queued)
                       when (eq (elpaca<-status e) 'failed) collect id)))
  (when failed (error "Package installation failed: %S" failed)))
(dolist (package '(sly sbcl racket-mode haskell-mode jupyter ein rust-mode jtsx consult-gh))
  (when (elpaca-get package) (error "Unused language installed at startup: %s" package)))
(unless (be/encryption-key) (error "No encryption key available after bootstrap"))
(require 'which-key)
(require 'eglot)
(when (featurep 'lsp-mode) (error "lsp-mode was loaded"))
(unless be/batch-bootstrap
  (unless (bound-and-true-p compile-angel-on-load-mode)
    (error "Ordinary startup did not enable compile-angel")))
;; Native compilation has its own resource limit; the disposable startup
;; check need not keep its process alive until every background job finishes.
(setq native-comp-async-query-on-exit nil confirm-kill-processes nil)
(princ (format "Bootstrap passed on Emacs %s; no unused languages installed\n" emacs-version))
;;; bootstrap-smoke.el ends here
