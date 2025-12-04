;;; markdown-mermaid.el --- Helper to preview Mermaid inside Markdown using /tmp
;;; Commentary:
;;;   Extracts mermaid code blocks from markdown, compiles them with mmdc,
;;;   and opens the resulting PNG from /tmp.
;;;
;;; npm install -g @mermaid-js/mermaid-cli
;;;
;;; and then adjust the `markdown-mermaid-mmdc-path'.
;;;
;;; Code:

(require 'markdown-mode)

;; 1. Configuration Variables
(defvar markdown-mermaid-mmdc-path
  (expand-file-name "~/.nvm/versions/node/v23.11.0/bin/mmdc")
  "Path to the mermaid-cli executable (mmdc).")

(defvar markdown-mermaid-temp-files nil
  "List of temporary files created during the session.")

;; 2. Cleanup Function
(defun markdown-mermaid-cleanup ()
  "Delete all temporary files created by markdown-mermaid."
  (interactive) ;; <--- NOW IT IS INTERACTIVE
  (let ((count 0))
    (if (not markdown-mermaid-temp-files)
        (message "No temporary Mermaid files to clean up.")
      (dolist (file markdown-mermaid-temp-files)
        (when (file-exists-p file)
          (delete-file file)
          (setq count (1+ count))))
      (setq markdown-mermaid-temp-files nil)
      (message "Cleaned up %d temporary Mermaid files." count))))

;; Register the cleanup function to run when Emacs exits
(add-hook 'kill-emacs-hook #'markdown-mermaid-cleanup)

;; 3. The Main Function
;;;###autoload
(defun markdown-mermaid-preview ()
  "Find the current mermaid block, save to /tmp, compile, and preview."
  (interactive)
  (let ((temp-input (make-temp-file "mermaid-block-" nil ".mmd"))
        (temp-output (make-temp-file "mermaid-block-" nil ".png"))
        start end mermaid-code)

    ;; Add files to the tracking list so they get deleted later
    (add-to-list 'markdown-mermaid-temp-files temp-input)
    (add-to-list 'markdown-mermaid-temp-files temp-output)

    ;; A. Find the bounds of the code block
    (save-excursion
      (let ((case-fold-search t))
        (if (re-search-backward "^[ \t]*```[ \t]*mermaid" nil t)
            (progn
              (forward-line 1)
              (setq start (point))
              (if (re-search-forward "^[ \t]*```" nil t)
                  (setq end (match-beginning 0))
                (error "Found start of block, but not the end")))
          (error "Cursor is not inside a ```mermaid block"))))

    ;; B. Extract code
    (setq mermaid-code (buffer-substring-no-properties start end))
    (with-temp-file temp-input
      (insert mermaid-code))

    ;; C. Compile
    (message "Compiling Mermaid block...")
    (call-process markdown-mermaid-mmdc-path
                  nil
                  "*mermaid-error*"
                  nil
                  "-i" temp-input
                  "-o" temp-output
                  "-b" "transparent")

    ;; D. Display
    (if (file-exists-p temp-output)
        (progn
          (message "Preview generated: %s" temp-output)
          (find-file-other-window temp-output))
      (switch-to-buffer-other-window "*mermaid-error*")
      (message "Compilation failed."))))

(provide 'markdown-mermaid)
;;; markdown-mermaid.el ends here.
