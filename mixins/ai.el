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

(use-package vterm) ;; best Aidermacs backend

(use-package aidermacs
  :after vterm
  :custom
  (aidermacs-backend 'vterm)
  (aidermacs-auto-commits nil)
  (aidermacs-watch-files t)

  ;; Workflow: Start in "Architect" mode (Plan -> Edit)
  (aidermacs-default-chat-mode 'architect)

  ;; 1. General Model (Code Mode) - Your daily driver
  (aidermacs-default-model "deepseek/deepseek-chat")

  ;; 2. Architect Model (The "Brain") - High-level reasoning
  (aidermacs-architect-model "anthropic/claude-3-5-sonnet-20241022")

  ;; 3. Editor Model (The "Hands") - Applies the Architect's plan
  ;; Gemini 2.0 Flash is currently the best at diff application speed/cost
  (aidermacs-editor-model "gemini/gemini-2.0-flash-exp")

  ;; 4. Weak Model (The "Scribe") - Commits & Summaries
  ;; Keep this as Gemini Flash to keep these interactions free and instant
  (aidermacs-weak-model "gemini/gemini-2.0-flash-exp")

  ;; Extra args: Only needed for flags not covered by variables
  (aidermacs-extra-args '("--no-show-model-warnings"))

  :bind (
         ;; Global binding to open the Transient Menu (The entry point)
         ("C-c a" . aidermacs-transient-menu)
         ;; Local bindings: Only active when aidermacs-minor-mode is on
         :map aidermacs-minor-mode-map
         ("C-c A c" . aidermacs-change-model)       ; Quick swap (Architect <-> Code)
         ("C-c A a" . aidermacs-add-current-buffer) ; Add file to context
         ("C-c A s" . aidermacs-send-region-or-buffer)) ; Send selection

  :config
  (global-aidermacs-mode t)
  (require 'secrets)

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
  (gptel-default-mode 'deepseek)
  (gptel-stream t)
  (gptel-max-tokens 4096)
  (gptel-use-curl nil)
  (gptel-model 'deepseek-chat)
  :config
  ;; 1. Setup Anthropic (The "Smart" Architect)
  (setq gptel-api-key "") ; Default key
  (gptel-make-anthropic "Claude"
    :key (ignore-errors (secrets-get-secret "AI" "anthropic-api-key"))
    :stream t
    :models '("claude-3-5-sonnet-20241022"))

  ;; 2. Setup Google Gemini (The "Free" Context Heavyweight)
  (gptel-make-gemini "Gemini"
    :key (ignore-errors (secrets-get-secret "AI" "gemini-api-key"))
    :stream t
    :models '("gemini-1.5-pro-latest"
              "gemini-1.5-flash-latest"))

  ;; 3. Setup DeepSeek V3 (The "Cheap" reasoner, fast and great for general coding)
  (gptel-make-openai "DeepSeek"
   :host "api.deepseek.com"
   :key (ignore-errors (secrets-get-secret "AI" "deepseek-api-key"))
   :stream t
   :models '("deepseek-chat" "deepseek-reasoner")))

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

(provide 'ai)
;;; ai.el ends here
