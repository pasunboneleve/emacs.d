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

(require 'markdown-mode)
(require 'json)

;; 1. Configuration Variables
(defvar markdown-mermaid-mmdc-path
  (expand-file-name "~/.nvm/versions/node/v23.11.0/bin/mmdc")
  "Path to the mermaid-cli executable (mmdc).")

(defvar markdown-mermaid-temp-files nil
  "List of temporary files created during the session.")

;; 2. Helper: Extract Colors and Create Config
(defun markdown-mermaid-generate-theme-config (file-path)
  "Create a Mermaid JSON config file at FILE-PATH based on current Emacs theme."
  (let* ((fg (face-attribute 'default :foreground nil 'default))
         (bg (face-attribute 'default :background nil 'default))
         ;; Use the 'Type' face color for node borders (usually distinct)
         (secondary (face-attribute 'font-lock-type-face :foreground nil 'default))
         ;; Use the 'Constant' face for lines/arrows
         (lines (face-attribute 'font-lock-constant-face :foreground nil 'default)))

    ;; Fallbacks in case Emacs returns 'unspecified
    (when (eq fg 'unspecified) (setq fg "#ffffff"))
    (when (eq bg 'unspecified) (setq bg "#1e1e1e"))
    (when (eq secondary 'unspecified) (setq secondary "#61dafb"))
    (when (eq lines 'unspecified) (setq lines fg))

    ;; Construct JSON string
    ;; We use the "base" theme which allows full variable overrides
    (let ((json-str (format
                     "{
  \"theme\": \"base\",
  \"themeVariables\": {
    \"primaryColor\": \"%s\",
    \"primaryTextColor\": \"%s\",
    \"primaryBorderColor\": \"%s\",
    \"lineColor\": \"%s\",
    \"secondaryColor\": \"%s\",
    \"tertiaryColor\": \"%s\"
  }
}"
                     bg        ;; primaryColor (Node Background) - matches editor bg
                     fg        ;; primaryTextColor (Text)
                     secondary ;; primaryBorderColor (Node Borders)
                     lines     ;; lineColor (Arrows)
                     bg        ;; secondaryColor
                     bg)))        ;; tertiaryColor

      (with-temp-file file-path
        (insert json-str)))))

;; 3. Cleanup Function
(defun markdown-mermaid-cleanup ()
  "Delete all temporary files created by markdown-mermaid."
  (interactive)
  (let ((count 0))
    (if (not markdown-mermaid-temp-files)
        (message "No temporary Mermaid files to clean up.")
      (dolist (file markdown-mermaid-temp-files)
        (when (file-exists-p file)
          (delete-file file)
          (setq count (1+ count))))
      (setq markdown-mermaid-temp-files nil)
      (message "Cleaned up %d temporary Mermaid files." count))))

(add-hook 'kill-emacs-hook #'markdown-mermaid-cleanup)

;; 4. The Main Function
;;;###autoload
(defun markdown-mermaid-preview ()
  "Find current mermaid block, generate theme config, compile, and preview."
  (interactive)
  (let ((temp-input (make-temp-file "mermaid-block-" nil ".mmd"))
        (temp-output (make-temp-file "mermaid-block-" nil ".png"))
        (temp-config (make-temp-file "mermaid-config-" nil ".json"))
        start end mermaid-code)

    ;; Track files for cleanup
    (add-to-list 'markdown-mermaid-temp-files temp-input)
    (add-to-list 'markdown-mermaid-temp-files temp-output)
    (add-to-list 'markdown-mermaid-temp-files temp-config)

    ;; A. Find bounds
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

    ;; B. Generate Dynamic Theme Config
    (markdown-mermaid-generate-theme-config temp-config)

    ;; C. Extract Code
    (setq mermaid-code (buffer-substring-no-properties start end))
    (with-temp-file temp-input
      (insert mermaid-code))

    ;; D. Compile with Config (-c) and Transparent Background (-b)
    (message "Compiling Mermaid block with current theme...")
    (call-process markdown-mermaid-mmdc-path
                  nil
                  "*mermaid-error*"
                  nil
                  "-i" temp-input
                  "-o" temp-output
                  "-c" temp-config    ;; <--- LOAD THE JSON CONFIG
                  "-b" "transparent") ;; <--- Keep transparent so it floats on Emacs bg

    ;; E. Display
    (if (file-exists-p temp-output)
        (progn
          (message "Preview generated: %s" temp-output)
          (find-file-other-window temp-output))
      (switch-to-buffer-other-window "*mermaid-error*")
      (message "Compilation failed."))))

(provide 'markdown-mermaid)
;;; markdown-mermaid.el ends here.
