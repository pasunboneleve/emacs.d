;;; be-formatters.el --- First-use formatter executables -*- lexical-binding: t; -*-
;;; Commentary:
;; Let Apheleia resolve npm/PnP projects before requesting a global fallback.
;;; Code:
(defun be/apheleia-tools (function command buffer remote callback &rest args)
  "Provision COMMAND's executable before FUNCTION formats BUFFER.
Complete CALLBACK with an explicit pending error while installation is running."
  (with-current-buffer buffer
    (let ((program (car command)))
      (if (or remote (file-remote-p default-directory) (not (stringp program))
              (if (equal program "apheleia-npx")
                  (be/ensure-tool "node")
                (or (string-prefix-p "apheleia-" program) (be/ensure-tool program))))
          (apply function command buffer remote callback args)
        (funcall callback '(error "Formatter installation pending or failed; see dependency log") nil)))))

(defun be/apheleia-npx-missing (function context stderr)
  "Request a fallback only after FUNCTION confirms npm/PnP found no formatter."
  (let ((missing (funcall function context stderr)))
    (when (and missing (not (apheleia-formatter--remote context))
               (equal (apheleia-formatter--arg1 context) "apheleia-npx")
               (buffer-live-p (apheleia-formatter--stdin context)))
      (with-current-buffer (apheleia-formatter--stdin context)
        (be/ensure-tool (car (apheleia-formatter--argv context)))))
    missing))

(provide 'be-formatters)
;;; be-formatters.el ends here
