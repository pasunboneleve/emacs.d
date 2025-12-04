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

;; 1. Define the path variable (Easy to change later)
(defvar markdown-mermaid-mmdc-path
  (expand-file-name "~/.nvm/versions/node/v23.11.0/bin/mmdc")
  "Path to the mermaid-cli executable (mmdc).")

;; 2. The Main Function
;;;###autoload
(defun markdown-mermaid-preview ()
  "Find the current mermaid block, save to /tmp, compile, and preview."
  (interactive)
  (let ((temp-input (make-temp-file "mermaid-block-" nil ".mmd"))
        (temp-output (make-temp-file "mermaid-block-" nil ".png"))
        start end mermaid-code)

    ;; A. Find the bounds of the code block
    (save-excursion
      (let ((case-fold-search t))
        ;; Search backward for the opening fence ```mermaid
        (if (re-search-backward "^[ \t]*```[ \t]*mermaid" nil t)
            (progn
              (forward-line 1) ;; Move inside the block
              (setq start (point))
              ;; Search forward for the closing fence ```
              (if (re-search-forward "^[ \t]*```" nil t)
                  (setq end (match-beginning 0))
                (error "Found start of block, but not the end")))
          (error "Cursor is not inside a ```mermaid block"))))

    ;; B. Extract the code and write to temp file
    (setq mermaid-code (buffer-substring-no-properties start end))
    (with-temp-file temp-input
      (insert mermaid-code))

    ;; C. Compile using mmdc
    (message "Compiling Mermaid block...")
    (call-process markdown-mermaid-mmdc-path
                  nil
                  "*mermaid-error*"
                  nil
                  "-i" temp-input
                  "-o" temp-output
                  "-b" "transparent")

    ;; D. Display the result
    (if (file-exists-p temp-output)
        (progn
          (message "Preview generated: %s" temp-output)
          (find-file-other-window temp-output))
      (switch-to-buffer-other-window "*mermaid-error*")
      (message "Compilation failed."))))

;; 3. Provide the feature so Emacs can require it
(provide 'markdown-mermaid)
;;; markdown-mermaid.el ends here.
