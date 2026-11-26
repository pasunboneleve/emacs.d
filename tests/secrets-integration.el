;;; secrets-integration.el --- Real disposable GnuPG integration -*- lexical-binding: t; -*-
;;; Commentary:
;; Creates and removes its own keyrings; never uses the account's real keys.
;;; Code:
(require 'ert)
(require 'cl-lib)
(setq load-prefer-newer t)
(add-to-list 'load-path
             (expand-file-name "../local-packages" (file-name-directory load-file-name)))
(require 'be-system)
(require 'be-secrets)

(ert-deftest be/local-key-and-foreign-file ()
  (let* ((directory (make-temp-file "emacs-gpg-test-" t))
         (home (expand-file-name "local" directory))
         (foreign (expand-file-name "foreign" directory))
         (encrypted (expand-file-name "sample.el.gpg" directory))
         (source (expand-file-name "sample.el" directory))
         (process-environment (copy-sequence process-environment))
         (epa-file-encrypt-to nil))
    (make-directory home)
    (make-directory foreign)
    (set-file-modes home #o700)
    (set-file-modes foreign #o700)
    (setenv "GNUPGHOME" home)
    (unwind-protect
        (progn
          (should (be/setup-encryption))
          (let ((recipient (car epa-file-encrypt-to)))
            (should recipient)
            (should (be/setup-encryption))
            (should (equal recipient (car epa-file-encrypt-to)))
            (with-temp-file source (insert "(setq be/test-secret-loaded t)\n"))
            (should (zerop (call-process "gpg" nil nil nil "--batch" "--yes"
                                         "--trust-model" "always" "--recipient" recipient
                                         "--output" encrypted "--encrypt" source)))
            (should (be/can-decrypt-p encrypted))
            (setenv "GNUPGHOME" foreign)
            (should-not (be/can-decrypt-p encrypted))
            (should (be/setup-encryption))
            (should-not (equal recipient (car epa-file-encrypt-to)))
            (should-not (be/can-decrypt-p encrypted))))
      (dolist (keyring (list home foreign))
        (call-process "gpgconf" nil nil nil "--homedir" keyring "--kill" "gpg-agent"))
      (delete-directory directory t))))

;;; secrets-integration.el ends here
