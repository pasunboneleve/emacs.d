;;; ai.el --- Summary  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Artificial Intelligence support.
;;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Artificial Intelligence modes
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'url)
(require 'secrets)

(use-package vterm ;; best Aidermacs backend
  :ensure t
  :config
  (defun my/sync-vterm-colors-with-theme ()
    "Set vterm-color-settings based on the current theme's ansi-color-names-vector."
    (interactive)
    (when (boundp 'ansi-color-names-vector)
      (let ((colors ansi-color-names-vector))
        (setq vterm-color-settings
              (list
               ;; Normal Colors (0-7)
               (list :num 0 :val (aref colors 0)) ;; Black
               (list :num 1 :val (aref colors 1)) ;; Red
               (list :num 2 :val (aref colors 2)) ;; Green
               (list :num 3 :val (aref colors 3)) ;; Yellow
               (list :num 4 :val (aref colors 4)) ;; Blue
               (list :num 5 :val (aref colors 5)) ;; Magenta
               (list :num 6 :val (aref colors 6)) ;; Cyan
               (list :num 7 :val (aref colors 7)) ;; White

               ;; Bright Colors (8-15)
               ;; We map these to the same vector because Solarized
               ;; typically handles 'bright' via bolding rather than different hues.
               (list :num 8  :val (aref colors 0))
               (list :num 9  :val (aref colors 1))
               (list :num 10 :val (aref colors 2))
               (list :num 11 :val (aref colors 3))
               (list :num 12 :val (aref colors 4))
               (list :num 13 :val (aref colors 5))
               (list :num 14 :val (aref colors 6))
               (list :num 15 :val (aref colors 7)))))))

  ;; 1. Run immediately on startup
  (my/sync-vterm-colors-with-theme)

  ;; 2. Run whenever a theme is loaded (Dynamic Support)
  ;; Note: This ensures if you M-x load-theme, vterm settings update.
  (add-hook 'enable-theme-functions #'my/sync-vterm-colors-with-theme))

(use-package aidermacs
  :after vterm
  :custom
  (aidermacs-backend 'vterm)
  (aidermacs-auto-commits nil)
  (aidermacs-watch-files t)
  (aidermacs-extra-args '("--dark-mode"))
  ;; Workflow: Start in "Architect" mode (Plan -> Edit)
  (aidermacs-default-chat-mode 'architect)

  ;; 1. General Model (Code Mode) - Your daily driver
  (aidermacs-default-model "gemini")

  ;; 2. Architect Model (The "Brain") - High-level reasoning
  ;; (aidermacs-architect-model "anthropic/claude-sonnet-4-5-20250929")
  (aidermacs-architect-model "gemini")

   ;; 3. Editor Model (The "Hands") - Applies the Architect's plan
   ;; Gemini 2.0 Flash is currently the best at diff application speed/cost
  (aidermacs-editor-model "flash")

   ;; 4. Weak Model (The "Scribe") - Commits & Summaries
   ;; Keep this as Gemini Flash to keep these interactions free and instant
  (aidermacs-weak-model "flash-lite")

  :bind (
         ;; Global binding to open the Transient Menu (The entry point)
         ("C-c a" . aidermacs-transient-menu)
         ;; Local bindings: Only active when aidermacs-minor-mode is on
         :map aidermacs-minor-mode-map
         ("C-c A c" . aidermacs-change-model)       ; Quick swap (Architect <-> Code)
         ("C-c A a" . aidermacs-add-current-buffer) ; Add file to context
         ("C-c A s" . aidermacs-send-region-or-buffer)) ; Send selection

  :config
  ;; Force the base model environment variable
  ;; This function reads the AI provider API keys from Gnome Keyring. If you
  ;; are not on Linux it will fail silently and you will need to use the normal
  ;; aider files or environment variables.
  (defun my/load-aider-keys-from-secrets ()
    "Load all *-api-key items from 'AI' collection into environment variables."
    (interactive)
    ;; check if "AI" collection exists
    (if (member "AI" (secrets-list-collections))
        (dolist (item (secrets-list-items "AI"))
          ;; Match items named like "gemini-api-key" or "anthropic-api-key"
          (when (string-match "^\\(.+\\)-api-key$" item)
            (let* ((provider (match-string 1 item))
                   (env-var-name (format "%s_API_KEY" (upcase provider)))
                   (secret-val (secrets-get-secret "AI" item)))

              ;; Set the environment variable for this emacs process
              ;; Aider will inherit this automatically.
              (when secret-val
                (setenv env-var-name secret-val)
                (message "Aider: Loaded %s" env-var-name)))))
      (message "Aider: 'AI' collection not found in keyring.")))

   ;; Run it immediately
  (my/load-aider-keys-from-secrets)

  :ensure-system-package
  ((cmake)
   (libtool)
   (uv . python-uv)
   (aider . "uv tool install --force --with-pip aider-chat@latest")))

(use-package gptel
  :custom
  ;; 1. Static Settings (Safe in :custom)
  (gptel-model 'deepseek-chat)       ; The default model name
  (gptel-stream t)                   ; Stream responses by default
  (gptel-max-tokens 8192)            ; Max token limit
  (gptel-default-mode 'org-mode)     ; Format responses as Org
  (gptel-use-header-line t)          ; shows status bar at the top while streaming
  :config
  ;; 2. Setup Logic (Must be in :config because we are calling functions)

  ;;;; 1. Setup Anthropic (The "Smart" Architect)
  (gptel-make-anthropic "Claude"
    :key (ignore-errors (secrets-get-secret "AI" "anthropic-api-key"))
    :stream t
    :models '("claude-sonnet-4-5-20250929"  ; <--- Your New "Architect" (Fast/Smart)
              "claude-opus-4-5-20251101"    ; <--- The "Heavyweight" (Slow/Genius)
              "claude-sonnet-4-20250514"))  ; <--- The Backup

  ;;;; 2. Setup Google Gemini (The "Free" Context Heavyweight)
  (gptel-make-gemini "Gemini"
    :key (ignore-errors (secrets-get-secret "AI" "gemini-api-key"))
    :stream t
    :models '("gemini-2.0-flash"     ; <--- Stable 2.0
              "gemini-1.5-flash"     ; <--- The Ultimate Backup (Always Free/Fast)
              "gemini-2.0-pro"))     ; <--- If you have paid credits

  ;;;; 3. Setup DeepSeek V3 (The "Cheap" reasoner, fast and great for general coding)
  (setq gptel-backend
   (gptel-make-openai "DeepSeek"
    :host "api.deepseek.com"
    :key (ignore-errors (secrets-get-secret "AI" "deepseek-api-key"))
    :stream t
    :models '("deepseek-chat" "deepseek-reasoner")))
  (add-to-list 'gptel-directives
               '(analyst . "You are an expert Product Analyst and Marketing Strategist.
1. Analyze the project idea for market viability, target audience, and competitive landscape.
2. Define a Monetization Strategy (Freemium, Subscriptions, One-time).
3. Create a Go-to-Market strategy (Launch plan, Marketing channels).
4. Identify User Personas (Who are we building this for?).
5. Output in Org-mode. Be critical and realistic."))
  (add-to-list 'gptel-directives
               '(architect . "You are an expert Software Architect.
1. Read the Context and Requirements provided.
2. Fill in \"Section 3: Architecture\" with the tech stack, file structure, and data models.
3. Fill in \"Section 4: Implementation Plan\" with a step-by-step checklist for the developer.
4. Use Org-mode formatting. Do not repeat the Context/Requirements."))

  (add-to-list 'gptel-directives
               '(developer . "You are an expert Full Stack Developer.
1. Read the Architecture and Implementation Plan.
2. Write the code for the requested feature.
3. Ensure code is clean, commented, and follows best practices.
4. Wrap code in Org-mode source blocks.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Utilities
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/gptel-load-bmad-persona (persona-file)
  "Load a BMAD PERSONA-FILE as the system prompt for gptel."
  (interactive "fSelect BMAD Persona File: ")
  (let ((persona-text (with-temp-buffer
                        (insert-file-contents persona-file)
                        (buffer-string))))
    (setq gptel-system-message persona-text)
    (message "Gptel acts as: %s" (file-name-base persona-file))))

(defvar my/bmad-prompts-dir (expand-file-name "~/.config/emacs/bmad-prompts/")
  "Directory to store BMAD agent prompts.")

;; A list of (Agent-Name . Raw-GitHub-URL)
;; You can add/remove agents here as the repo evolves.
(defvar my/bmad-agent-sources
  '(("Architect" . "https://raw.githubusercontent.com/bmad-code-org/BMAD-METHOD/main/src/modules/bmm/agents/architect.md")
    ("ProductManager" . "https://raw.githubusercontent.com/bmad-code-org/BMAD-METHOD/main/src/modules/bmm/agents/product-manager.md")
    ("Developer" . "https://raw.githubusercontent.com/bmad-code-org/BMAD-METHOD/main/src/modules/bmm/agents/developer.md")
    ("Analyst" . "https://raw.githubusercontent.com/bmad-code-org/BMAD-METHOD/main/src/modules/bmm/agents/analyst.md")))

(defun my/bmad-fetch-prompts ()
  "Download BMAD agent prompts from GitHub to my/bmad-prompts-dir."
  (interactive)
  (unless (file-exists-p my/bmad-prompts-dir)
    (make-directory my/bmad-prompts-dir t))

  (dolist (agent my/bmad-agent-sources)
    (let* ((name (car agent))
           (url (cdr agent))
           (target-file (expand-file-name (concat name ".md") my/bmad-prompts-dir)))
      (message "Fetching BMAD Agent: %s..." name)
      (with-current-buffer (url-retrieve-synchronously url)
        (goto-char (point-min))
        ;; Skip HTTP headers
        (re-search-forward "\n\n")
        (let ((content (buffer-substring (point) (point-max))))
          (with-temp-file target-file
            (insert content)))
        (kill-buffer))))
  (message "BMAD Prompts saved to %s" my/bmad-prompts-dir))

(defun my/gptel-set-bmad-persona ()
  "Read a BMAD prompt file and set it as the system message for the current buffer."
  (interactive)
  ;; 1. Ensure prompts exist; fetch if missing
  (unless (file-exists-p my/bmad-prompts-dir)
    (if (y-or-n-p "BMAD prompts not found.  Fetch them now?")
        (my/bmad-fetch-prompts)
      (error "Prompts missing")))

  ;; 2. Select Agent
  (let* ((files (directory-files my/bmad-prompts-dir nil "\\.md$"))
         (selected-agent (completing-read "Select BMAD Agent: " files nil t))
         (full-path (expand-file-name selected-agent my/bmad-prompts-dir))
         (prompt-content (with-temp-buffer
                           (insert-file-contents full-path)
                           (buffer-string))))

    ;; 3. Set GPTel System Prompt
    (setq-local gptel-system-message prompt-content)
    (message "Gptel is now running as: %s" (file-name-base selected-agent))))

;; Bind this to a convenient key
(global-set-key (kbd "C-c B p") 'my/gptel-set-bmad-persona)

(provide 'ai)
;;; ai.el ends here
