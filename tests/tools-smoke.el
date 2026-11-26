;;; tools-smoke.el --- Actual first-use CLI installation -*- lexical-binding: t; -*-
;;; Commentary:
;; Run only in the disposable CI home; installs SDKs and checks real LSP startup.
;;; Code:
(load (expand-file-name "bootstrap-smoke.el" (file-name-directory load-file-name)) nil t)
(require 'json)

(defun be/smoke-tool (name &rest arguments)
  "Provision NAME, then execute its version/help ARGUMENTS."
  (message "Installing and checking %s" name)
  (unless (be/ensure-tool name)
    (when-let ((buffer (get-buffer "*Emacs dependency installation*")))
      (princ (with-current-buffer buffer (buffer-string))))
    (error "Could not provision %s" name))
  (when arguments
    (let ((output (generate-new-buffer (concat " *CLI check " name "*"))) process)
      (unwind-protect
          (progn
            (setq process (make-process :name (concat "check-" name) :buffer output
                                        :command (cons name arguments) :connection-type 'pipe :noquery t))
            (let ((deadline (+ (float-time) 180)))
              (while (and (process-live-p process) (< (float-time) deadline))
                (accept-process-output process 0.1)))
            (unless (and (eq (process-status process) 'exit) (zerop (process-exit-status process)))
              (error "%s validation failed or timed out: %s" name (with-current-buffer output (buffer-string)))))
        (when (process-live-p process) (delete-process process))
        (kill-buffer output))))
  (message "%s CLI check passed" name))

(defun be/smoke-lsp (name command)
  "Require an actual initialize response from language server NAME and COMMAND."
  (let* ((directory (make-temp-file "emacs-lsp-smoke-" t))
         (default-directory directory)
         (output (generate-new-buffer (concat " *" name " stdout*")))
         (errors (generate-new-buffer (concat " *" name " stderr*")))
         process)
    (unwind-protect
        (progn
          (setq process (make-process :name name :buffer output :stderr errors
                                      :command command :coding 'utf-8-unix :connection-type 'pipe :noquery t))
          (let ((message (json-serialize
                          `(:jsonrpc "2.0" :id 1 :method "initialize"
                            :params (:processId :null :rootUri ,(concat "file://" directory)
                                     :capabilities ,(make-hash-table)))
                          :null-object :null)))
            (process-send-string process (format "Content-Length: %s\r\n\r\n%s"
                                                  (string-bytes message) message)))
          (let ((deadline (+ (float-time) 90)))
            (while (and (process-live-p process) (< (float-time) deadline)
                        (not (with-current-buffer output
                               (string-match-p "\"capabilities\"" (buffer-string)))))
              (accept-process-output process 0.1)))
          (unless (with-current-buffer output (string-match-p "\"capabilities\"" (buffer-string)))
            (error "%s did not initialize. stdout=%s stderr=%s" name
                   (with-current-buffer output (buffer-string))
                   (with-current-buffer errors (buffer-string))))
          (princ (format "%s LSP initialize passed\n" name)))
      (when (process-live-p process) (delete-process process))
      (kill-buffer output) (kill-buffer errors)
      (delete-directory directory t))))

(dolist (entry '(("tinymist" "--version") ("typst" "--version") ("typstyle" "--version")
                  ("helm_ls" "--help") ("fourmolu" "--version")
                  ("jdtls" "--help") ("google-java-format" "--version")
                  ("omnisharp") ("csharpier" "--version")
                  ("metals" "--version") ("sbt") ("scalafmt" "--version")
                  ("haskell-language-server-wrapper" "--version") ("stack" "--version")
                  ("cabal" "--version") ("elm-language-server" "--version")
                  ("prettier" "--version") ("mmdc" "--version") ("sqlfluff" "--version")
                  ("racket" "--version") ("dot" "-V") ("dhall" "--version")
                  ("aider" "--version")))
  (apply #'be/smoke-tool entry))
(be/smoke-lsp "tinymist" '("tinymist"))
(be/smoke-lsp "jdtls" '("jdtls"))
(be/smoke-lsp "omnisharp" '("omnisharp" "-lsp"))
(let (opened)
  (cl-letf (((symbol-function 'be/aider-open) (lambda () (setq opened t))))
    (be/aider)
    (elpaca-wait))
  (unless (and opened (be/tool-present-p "vterm-module"))
    (error "Aider entry point did not prepare the native backend before opening"))
  (princ "Aider entry point and Vterm native backend passed\n"))
(let* ((directory (make-temp-file "emacs-mermaid-" t))
       (source (expand-file-name "diagram.mmd" directory))
       (output (expand-file-name "diagram.svg" directory))
       (config (expand-file-name "puppeteer.json" directory)))
  (unwind-protect
      (progn
        (with-temp-file source (insert "graph TD; A-->B;\n"))
        ;; Root containers have no browser sandbox.  This fixture flag is never
        ;; written to the user's editor configuration or used on a workstation.
        (when (zerop (user-uid))
          (with-temp-file config (insert "{\"args\":[\"--no-sandbox\"]}")))
        (be/run-installer (append (list "mmdc" "-i" source "-o" output)
                                  (when (zerop (user-uid)) (list "-p" config))))
        (unless (and (file-exists-p output) (> (file-attribute-size (file-attributes output)) 0))
          (error "Mermaid produced no SVG"))
        (princ "Mermaid CLI and browser rendered SVG successfully\n"))
    (delete-directory directory t)))
(princ (format "CLI provisioning and language-server startup passed on Emacs %s\n" emacs-version))
;;; tools-smoke.el ends here
