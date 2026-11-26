;;; be-secrets.el --- Machine-local encryption and optional private files -*- lexical-binding: t; -*-
;;; Commentary:
;; A local key enables new secrets.  It cannot unlock another machine's files.
;;; Code:
(require 'cl-lib)
(require 'epg)
(require 'epa-file)

(defun be/encryption-key ()
  "Return a usable local secret encryption key, if available."
  (cl-find-if
   (lambda (key)
     (cl-some (lambda (subkey)
                (and (memq 'encrypt (epg-sub-key-capability subkey))
                     (not (memq (epg-sub-key-validity subkey) '(revoked expired disabled invalid)))
                     (or (not (epg-sub-key-expiration-time subkey))
                         (> (epg-sub-key-expiration-time subkey) (float-time)))))
              (epg-key-sub-key-list key)))
   (epg-list-keys (epg-make-context 'OpenPGP) nil t)))

(defun be/setup-encryption ()
  "Use a local encryption key, creating one when none is available.
Generated keys have no passphrase and depend on OS account protection;
they are local storage keys, never exported or used as an identity."
  (interactive)
  (condition-case err
      (when (be/ensure-tool
             'gpg nil
             (lambda ()
               (when (be/setup-encryption)
                 (dolist (file '("custom.el.gpg" "custom-local.el.gpg"
                                 "mixins/communication.el.gpg" "mixins/safe.el.gpg"))
                   (be/load-optional-user-file (expand-file-name file user-emacs-directory))))))
        (let ((key (be/encryption-key)))
          (unless key
            (be/run-installer
             (list "gpg" "--batch" "--pinentry-mode" "loopback" "--passphrase" ""
                   "--quick-generate-key"
                   (format "Emacs local storage (%s@%s)" (user-login-name) (system-name))
                   "default" "default" "0"))
            (setq key (or (be/encryption-key) (error "GnuPG created no usable encryption key"))))
          (setq epa-file-encrypt-to
                (list (epg-sub-key-fingerprint (car (epg-key-sub-key-list key)))))
          (epa-file-enable)
          t))
    (error (display-warning 'be-secrets (error-message-string err) :warning) nil)))

(defun be/can-decrypt-p (file)
  "Return non-nil when FILE names a recipient with an available secret key.
Packet inspection never invokes pinentry or attempts to decrypt the file."
  (when (executable-find "gpg")
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8))
        (call-process "gpg" nil (list (current-buffer) nil) nil
                      "--batch" "--pinentry-mode" "error" "--list-only"
                      "--list-packets" (expand-file-name file)))
      (let ((packets (buffer-string))
            (keys (epg-list-keys (epg-make-context 'OpenPGP) nil t)))
        (cl-some
         (lambda (key)
           (cl-some (lambda (subkey)
                      (string-match-p (concat "keyid " (regexp-quote (epg-sub-key-id subkey)))
                                      packets))
                    (epg-key-sub-key-list key)))
         keys)))))

(defun be/load-optional-user-file (file)
  "Load existing FILE only when its encryption key is available."
  (when (file-exists-p file)
    (condition-case err
        (if (and (string-suffix-p ".gpg" file) (not (be/can-decrypt-p file)))
            (display-warning 'be-secrets (format "Skipping %s: no matching secret key" file) :warning)
          (load file nil 'nomessage nil t))
      (error (display-warning 'be-secrets
                              (format "Cannot load %s: %s" file (error-message-string err))
                              :warning)))))

(provide 'be-secrets)
;;; be-secrets.el ends here
