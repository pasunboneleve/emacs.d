;;; be-install.el --- Serial background tool installation -*- lexical-binding: t; -*-
;;; Commentary:
;; The editor owns progress and sudo prompts.  Private installers run in a child
;; Emacs; native packages use TRAMP's asynchronous process API for authentication.
;;; Code:
(require 'cl-lib)

(defvar be/tool-queue nil)
(defvar be/tool-process nil)
(defvar be/tool-current nil)
(defvar be/tool-status nil)
(defvar be/tool-installers (make-hash-table :test 'equal))
(defvar be/tool-callbacks (make-hash-table :test 'equal))
(defconst be/tool-worker-file
  (expand-file-name "be-tool-worker.el" (file-name-directory load-file-name)))
(add-to-list 'global-mode-string '(:eval be/tool-status) t)

(defun be/tool-label (name)
  "Return the human-facing progress label for installation NAME."
  (cond ((string-prefix-p "@" name) (concat (substring name 1) " libraries"))
        ((string-prefix-p "grammar:" name) (concat (substring name 8) " grammar"))
        (t name)))

(defun be/queue-tool (name installer callback)
  "Enqueue NAME and its prerequisites once, retaining each ready CALLBACK."
  (when callback
    (puthash name (cons callback (gethash name be/tool-callbacks)) be/tool-callbacks))
  (cl-labels ((enqueue (tool)
                (unless (or (be/tool-present-p tool) (equal tool be/tool-current)
                            (member tool be/tool-queue))
                  (mapc #'enqueue (be/tool-requirements tool))
                  (setq be/tool-queue (append be/tool-queue (list tool))))))
    (enqueue name))
  (when installer (puthash name installer be/tool-installers))
  ;; Starting a native job may ask for sudo.  Do that after the calling mode
  ;; finishes activation, rather than interrupting its setup halfway through.
  (unless be/tool-current (run-at-time 0 nil #'be/start-next-tool)))

(defun be/tool-worker-command (name installer)
  "Return the background command for private installer NAME and INSTALLER."
  (let ((command (list (expand-file-name invocation-name invocation-directory)
                       "-Q" "--batch" "-l" be/tool-worker-file
                       name (prin1-to-string installer))))
    (when (executable-find "nice")
      (setq command (append '("nice" "-n" "10") command)))
    ;; A user scope contains the whole process tree, including compiler children.
    ;; CI/root/bus-less environments use the normal child process and build limits.
    (when (and (eq system-type 'gnu/linux) (not (zerop (user-uid)))
               (getenv "DBUS_SESSION_BUS_ADDRESS") (executable-find "systemd-run"))
      (setq command (append '("systemd-run" "--user" "--scope" "--quiet"
                              "-p" "MemoryMax=2G" "-p" "MemorySwapMax=0"
                              "-p" "CPUQuota=100%") command)))
    command))

(defun be/native-tool-command (name installer)
  "Return a native package command for NAME, or nil for a private installer."
  (cond ((string-prefix-p "@" name) (be/tool-library-command name))
        ((string-prefix-p "grammar:" name) nil)
        ((stringp installer) nil)
        (installer (be/system-install-command name (symbol-name installer)))
        ((member name be/managed-tools) nil)
        ((assq (intern name) be/system-packages) (be/system-install-command name))
        ((or (assoc name be/tool-dependencies)
             (member name '("uv" "cargo" "rustup"))) nil)
        (t (be/system-install-command name))))

(defun be/start-next-tool ()
  "Start the next serial installation without waiting for its subprocess."
  (unless be/tool-current
    (when-let ((name (pop be/tool-queue)))
      (setq be/tool-current name
            be/tool-status (format " Installing %s%s" (be/tool-label name)
                                   (if be/tool-queue (format " (+%d)" (length be/tool-queue)) "")))
      (force-mode-line-update t)
      (message "Installing %s in the background; editing remains available" (be/tool-label name))
      (condition-case err
          (if (be/tool-present-p name)
              (be/finish-tool t "already available")
            (progn
            (dolist (dependency (be/tool-requirements name))
              (unless (be/tool-present-p dependency)
                (error "%s prerequisite %s did not install" name dependency)))
            (let* ((installer (gethash name be/tool-installers))
                   (command (be/native-tool-command name installer))
                   (buffer (get-buffer-create "*Emacs dependency installation*"))
                   (log-start (with-current-buffer buffer (point-max)))
                   (default-directory (expand-file-name "~"))
                   (process-environment (copy-sequence process-environment)))
              (with-current-buffer buffer
                (goto-char (point-max)) (insert (format "\nInstalling %s\n" name)))
              (setenv "BE_TOOL_EMACS_DIRECTORY" user-emacs-directory)
              (setenv "BE_TOOL_WORKER" "1")
              (when-let ((library (locate-library "vterm")))
                (setenv "BE_TOOL_VTERM_DIRECTORY" (file-name-directory library)))
              (if command
                  (progn
                    (when (equal (car command) "sudo")
                      (setq default-directory "/sudo::/" command (cdr command)))
                    (setq be/tool-process
                          (apply #'start-file-process (concat "install-" name) buffer
                                 (car command) (cdr command))))
                (setq be/tool-process
                      (make-process :name (concat "install-" name) :buffer buffer
                                    :command (be/tool-worker-command name installer)
                                    :connection-type 'pipe :noquery t)))
              (let ((process be/tool-process))
                (process-put process 'be/log-start log-start)
                (set-process-sentinel process #'be/tool-finished)
                ;; Very short native jobs can finish before the sentinel is attached.
                (when (and (eq process be/tool-process)
                           (memq (process-status process) '(exit signal)))
                  (be/tool-finished process "finished"))))))
        (error (be/finish-tool nil (error-message-string err)))))))

(defun be/tool-finished (process _event)
  "Handle the final status of installation PROCESS."
  (when (and (eq process be/tool-process) (memq (process-status process) '(exit signal)))
    (if (and (string-prefix-p "grammar:" be/tool-current)
             (with-current-buffer (process-buffer process)
               (save-excursion
                 (goto-char (or (process-get process 'be/log-start) (point-min)))
                 (search-forward "BE_TOOL_PREREQUISITE:c++" nil t))))
        (let ((name be/tool-current))
          (puthash name '("c++") be/tool-extra-requirements)
          (setq be/tool-current nil be/tool-process nil)
          (be/queue-tool name (gethash name be/tool-installers) nil))
      (be/finish-tool (and (eq (process-status process) 'exit)
                         (zerop (process-exit-status process)))
                    (format "process exited %s" (process-exit-status process))))))

(defun be/finish-tool (success detail)
  "Publish installation SUCCESS or DETAIL, then continue the queue."
  (let ((name be/tool-current))
    (when (and success (string-prefix-p "@" name))
      (make-directory (file-name-directory (be/tool-marker name)) t)
      (write-region "installed\n" nil (be/tool-marker name) nil 'silent))
    (setq success (and success (be/tool-present-p name)))
    (setq be/tool-current nil be/tool-process nil be/tool-status nil)
    (force-mode-line-update t)
    (if success
        (progn
          (message "%s is ready" (be/tool-label name))
          (dolist (callback (gethash name be/tool-callbacks))
            (condition-case err (funcall callback)
              (error (display-warning 'be-system (error-message-string err) :warning)))))
      (display-warning 'be-system
                       (format "Could not install %s (%s). See *Emacs dependency installation*. Retry with M-x be/install-tool."
                               name detail) :warning))
    (remhash name be/tool-callbacks)
    (when success (remhash name be/tool-installers))
    (when be/tool-queue (run-at-time 0 nil #'be/start-next-tool))))

(defun be/install-tool (name)
  "Install or retry executable NAME, reporting progress without blocking editing."
  (interactive (list (completing-read "Install tool: "
                                    (append be/managed-tools (mapcar #'car be/tool-dependencies)))))
  (if (be/ensure-tool name) (message "%s is already available" name)
    (message "%s installation requested; see the mode line and dependency log" name)))

(provide 'be-install)
;;; be-install.el ends here
