;;; be-git.el --- SSH when authenticated, HTTPS otherwise -*- lexical-binding: t; -*-
;;; Commentary:
;; Only package and grammar downloads use this policy.  User remotes stay intact.
;;; Code:
(require 'cl-lib)
(require 'subr-x)

(defvar be/ssh-checks (make-hash-table :test #'equal))
(defconst be/git-hosts '("github.com" "gitlab.com" "codeberg.org" "git.sr.ht"))

(defun be/git-ssh-command ()
  "Return the SSH command configured for Git, or the standard client."
  (or (getenv "GIT_SSH_COMMAND")
      (with-temp-buffer
        (when (zerop (call-process "git" nil t nil "config" "--get" "core.sshCommand"))
          (string-trim (buffer-string))))
      "ssh"))

(defun be/with-git-urls (urls function &rest args)
  "Call FUNCTION with ARGS using the selected transports in URLS.
An exact URL rewrite prevents a broad global insteadOf rule from turning
the HTTPS fallback back into SSH.  All settings are subprocess-local."
  (let ((process-environment (copy-sequence process-environment))
        (count (string-to-number (or (getenv "GIT_CONFIG_COUNT") "0"))))
    (dolist (url urls)
      (when (and (string-match "\\`\\(?:https://\\|ssh://git@\\|git@\\)\\([^/:]+\\)[:/]" url)
                 (member (match-string 1 url) be/git-hosts))
        (setenv (format "GIT_CONFIG_KEY_%d" count) (concat "url." url ".insteadOf"))
        (setenv (format "GIT_CONFIG_VALUE_%d" count) url)
        (setq count (1+ count))))
    (dolist (key '("pack.threads" "index.threads"))
      (setenv (format "GIT_CONFIG_KEY_%d" count) key)
      (setenv (format "GIT_CONFIG_VALUE_%d" count) "1")
      (setq count (1+ count)))
    (setenv "GIT_CONFIG_COUNT" (number-to-string count))
    (setenv "GIT_TERMINAL_PROMPT" "0")
    (setenv "GIT_SSH_COMMAND"
            (concat (be/git-ssh-command)
                    " -o BatchMode=yes -o StrictHostKeyChecking=yes -o ConnectTimeout=5"))
    (apply function args)))

(defun be/elpaca-git-process (function e &rest spec)
  "Apply transport settings when FUNCTION starts E's Git subprocess SPEC."
  (let ((command (plist-get spec :command)))
    (if (not (equal (car command) "git"))
        (apply function e spec)
      (let ((urls (cl-remove-if-not
                   (lambda (arg) (string-match-p "\\`\\(?:https://\\|git@\\|ssh://git@\\)" arg))
                   (cdr command)))
            (repository (elpaca<-source-dir e)))
        (when (file-directory-p repository)
          (let ((default-directory repository))
            (with-temp-buffer
              (when (zerop (call-process "git" nil t nil "config" "--get-regexp" "^remote\\..*\\.url$"))
		(dolist (line (split-string (buffer-string) "\n" t))
                  (push (substring line (1+ (string-match " " line))) urls))))))
        (apply #'be/with-git-urls urls function e spec)))))

(defun be/ssh-authenticated-p (host)
  "Probe HOST with a six-second deadline and no interactive authentication."
  (with-temp-buffer
    (let* ((process (make-process
                     :name "emacs-ssh-check" :buffer (current-buffer)
                     :command (list shell-file-name shell-command-switch
                                    (concat (be/git-ssh-command)
                                            " -vT -o BatchMode=yes -o StrictHostKeyChecking=yes"
                                            " -o ConnectTimeout=3 "
                                            (shell-quote-argument (concat "git@" host))))
                     :connection-type 'pipe :noquery t))
           (deadline (+ (float-time) 6)))
      (unwind-protect
          (progn
            (while (and (process-live-p process) (< (float-time) deadline))
              (accept-process-output process 0.1))
            (goto-char (point-min))
            (and (not (process-live-p process))
                 (or (zerop (process-exit-status process))
                     (re-search-forward
                      "Authenticated to \\|successfully authenticated\\|Welcome to GitLab\\|successfully logged in"
                      nil t))
                 t))
        (when (process-live-p process) (delete-process process))))))

(defun be/ssh-ready-p (host)
  "Check HOST authentication without prompting, caching for one minute.
Use ssh's configured identities, agent, aliases, and known host keys."
  (let ((cached (gethash host be/ssh-checks)))
    (if (and cached (< (- (float-time) (car cached)) 60))
        (cdr cached)
      ;; Process sentinels can request another clone while this probe waits.
      (puthash host (cons (float-time) nil) be/ssh-checks)
      (let ((ready (and (executable-find "ssh") (be/ssh-authenticated-p host))))
        (puthash host (cons (float-time) ready) be/ssh-checks)
        ready))))

(defun be/git-url (url)
  "Choose an authenticated transport for a supported public hosting URL."
  (if (string-match
       "\\`\\(?:https://\\|ssh://git@\\|git@\\)\\([^/:]+\\)[:/]\\(.+\\)\\'" url)
      (let ((host (match-string 1 url)) (path (match-string 2 url)))
        (if (member host be/git-hosts)
            (let ((ssh (be/ssh-ready-p host)))
              (concat (if ssh "git@" "https://") host (if ssh ":" "/") path))
          url))
    url))

(defun be/elpaca-repo-url (original recipe)
  "Apply download transport policy to ORIGINAL's RECIPE URL."
  (be/git-url (funcall original recipe)))

(defun be/elpaca-refresh-remotes (e &rest _)
  "Update supported package checkout remotes before fetching E."
  (let ((default-directory (elpaca<-source-dir e)))
    (with-temp-buffer
      (unless (zerop (call-process "git" nil t nil "remote"))
        (error "Cannot list package remotes in %s" default-directory))
      (dolist (remote (split-string (buffer-string) "\n" t))
        (with-temp-buffer
          (when (zerop (call-process "git" nil t nil "config" "--get"
                                     (concat "remote." remote ".url")))
            (let* ((old (string-trim (buffer-string)))
                   (new (be/git-url old)))
              (unless (equal old new)
                (unless (zerop (call-process "git" nil nil nil "remote" "set-url" remote new))
                  (error "Cannot update package remote %s" remote))))))))))

(defun be/git-setup ()
  "Enable transport selection for new and existing Elpaca checkouts."
  (require 'elpaca-git)
  (advice-add 'elpaca-git--repo-uri :around #'be/elpaca-repo-url)
  (advice-add 'elpaca--make-process :around #'be/elpaca-git-process)
  (advice-add 'elpaca-git--fetch :before #'be/elpaca-refresh-remotes))

(provide 'be-git)
;;; be-git.el ends here
