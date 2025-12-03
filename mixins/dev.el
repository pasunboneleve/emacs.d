;;; dev.el --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Cross language functionality.
;;; Code:

;; narrowing is life
(put 'narrow-to-region 'disabled nil)

;; rainbow-delimiters -- excellent for any language,
;; critical for lisps
(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

;; helps reading yaml
(use-package highlight-indent-guides
  :custom (highlight-indent-guides-method 'character))

;; we need to upgrade the inbuilt flymake version so
;; that packages that require it don't fail
(use-package flymake
  :ensure nil
  :bind (:map flymake-mode-map (("M-n" . flymake-goto-next-error)
                                ("M-p" . flymake-goto-prev-error))))

;; Syntax checking and everything else related to it
(use-package flycheck
  ;; :init (global-flycheck-mode)
  :config
  (setq-default flycheck-temp-prefix ".flycheck")
  :custom
  (flycheck-disabled-checkers '(haskell-stack-ghc haskell-ghc haskell-hlint)))

(use-package aggressive-indent)

;;; format code helper
(use-package format-all
  :config
  (custom-set-variables '(format-all-formatters '(("Haskell" fourmolu))))
  :hook (format-all-mode . format-all-ensure-formatter))

(use-package yasnippet)

;; code folding, very useful with big JSON files
(use-package origami
  :bind (:map origami-mode-map ("TAB" . origami-toggle-node)))

;; respect pairs when killing, and add paredit-like navigation to
;; all prog modes
(use-package puni
  :delight
  :config (defun puni-delete-one
              (from to &optional strict-sexp kill fail-action return-region)
            (puni-soft-delete
             from to strict-sexp 'delete-one kill fail-action return-region))
  :hook (prog-mode . puni-mode)
  :bind (:map puni-mode-map (("C-)" . puni-slurp-forward)
                             ("C-(" . puni-slurp-backward)
                             ("C-}" . puni-barf-forward)
                             ("C-{" . puni-barf-backward)
                             ("C-k" . nil))))

;; fallback from puni
(use-package smartparens
  :bind (:map smartparens-mode-map
              ("C-)" . sp-forward-slurp-sexp)
              ("C-(" . sp-backward-slurp-sexp)
              ("C-}" . sp-forward-barf-sexp)
              ("C-{" . sp-backward-barf-sexp)))

;; source editing and movement using treesitter when available
(use-package combobulate
  :hook ((prog-mode . combobulate-mode))
  :ensure (combobulate :host github :repo "dmvianna/combobulate"))

;; easily find other similar symbols in buffer
(use-package smartscan)

;;; save lots of undo history
;;; manual: https://www.dr-qubit.org/undo-tree/undo-tree.txt
;;; memory management: https://www.dr-qubit.org/Lost_undo-tree_history.html
;; (use-package undo-tree
;;   :custom
;;   (undo-tree-auto-save-history nil)
;;   (undo-tree-limit 8000000) ;; 10 times smaller than default
;;   (undo-tree-strong-limit 12000000) ;; 10 times smaller than default
;;   (undo-tree-outer-limit 360000000) ;; 10 times smaller than default
;;   :init
;;   (setq undo-tree-enable-undo-in-region nil)
;;   (defadvice undo-tree-make-history-save-file-name
;;       (after undo-tree activate)
;;     (setq ad-return-value (concat ad-return-value ".gz"))))

(use-package undo-hl
  :delight
  :ensure (undo-hl :host github :repo "casouri/undo-hl")
  :hook ((text-mode . undo-hl-mode)
         (prog-mode . undo-hl-mode))
  :custom-face
  (undo-hl-insert ((t (:background "#FFFF00"))))
  (undo-hl-delete ((t (:background "#FFFF00")))))

(use-package vundo
  :after undo-hl
  :ensure (vundo :host github :repo "casouri/vundo")
  :config
  ;; this is all so that undo-hl works with vundo. It mostly does.
  (setq undo-hl-undo-commands
        (-union
         undo-hl-undo-commands
         '(vundo-backward vundo-forward vundo-stem-root vundo-stem-end)))
  (defvar my-real-vundo-buf nil)
  (defun my-vundo-forward-pre-command-hook ()
    (let ((buf (current-buffer)))
      (when (bound-and-true-p my-real-vundo-buf)
        (unless (eq buf my-real-vundo-buf)
          (with-current-buffer my-real-vundo-buf (run-hooks 'pre-command-hook))))))
  ;; also undo-hl stuff
  (add-hook 'vundo-pre-enter-hook
            (lambda ()
              (setq my-real-vundo-buf (current-buffer))
              (add-hook 'pre-command-hook 'my-vundo-forward-pre-command-hook)
              (undo-hl-mode 1)))
  (add-hook 'vundo-post-exit-hook
            (lambda ()
              (remove-hook 'pre-command-hook 'my-vundo-forward-pre-command-hook)
              (undo-hl-mode -1)
              (makunbound 'my-real-vundo-buf)))
  ;; Better contrasting highlight.
  :custom-face
  (vundo-node ((t (:foreground "#808080"))))
  (vundo-stem ((t (:foreground "#808080"))))
  (vundo-highlight ((t (:foreground "#FFFF00")))))

;;; exclude the following directories from grep searches
(eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories "tmp")
     (add-to-list 'grep-find-ignored-directories "straight")
     (add-to-list 'grep-find-ignored-directories "elpaca")
     (add-to-list 'grep-find-ignored-directories "node_modules")
     (add-to-list 'grep-find-ignored-directories "amazonka")
     (add-to-list 'grep-find-ignored-directories "amazonka-1.6.1")
     (add-to-list 'grep-find-ignored-directories "amazonka-2.0.0")
     (add-to-list 'grep-find-ignored-directories ".stack-work")))
(setq wgrep-enable-key (kbd "C-c C-c"))
(add-hook 'grep-mode-hook (lambda () (toggle-truncate-lines 1)))

;; file search
(use-package rg
  :ensure-system-package (rg . ripgrep))


;; fallback cross file definition lookup
(use-package dumb-jump
  :hook (xref-backend-functions . dumb-jump-xref-activate)
  :ensure-system-package (rg . ripgrep))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; shells
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; because in Big Sur this is set to /
(setq default-directory (getenv "HOME"))

;; exec path from shell
(use-package exec-path-from-shell
  :config
  (dolist (var '("PATH" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"
                 "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH" "XAUTHORITY"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   CLI
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; term
(use-package multi-term)

;; elisp term
(use-package aweshell
  :ensure (aweshell
           :host github
           :repo "manateelazycat/aweshell"
           :files ("aweshell.el"
                   "eshell-did-you-mean.el"
                   "eshell-prompt-extras.el"
                   "eshell-up.el"
                   "exec-path-from-shell.el")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package transient
  :pin gnu)

(use-package magit
  :requires (transient git-commit)
  :pin gnu
  :after transient
  ;; :hook
  ;; (git-commit-setup . (lambda () (electric-indent-local-mode -1)))
  :config
  ;; status is expensive in big repos, only refresh if
  ;; it is the current buffer
  ;; (setq magit-refresh-status-buffer nil)
  ;; it is always git, so no need to display it
  ;; https://emacs.stackexchange.com/a/10957/3895
  (defadvice vc-mode-line (after strip-backend () activate)
    (when (stringp vc-mode)
      (let ((noback (replace-regexp-in-string
                     (format "^ %s" (vc-backend buffer-file-name))
                     " " vc-mode)))
        (setq vc-mode noback))))
  (setq vc-display-status nil) ;; don't display branch name in mode line
  (if (not (boundp 'project-switch-commands))
      (setq project-switch-commands nil))
  :bind (:map
         magit-mode-map
         ("C-x g" . magit-status)))

(use-package abridge-diff
  :after magit)

;;; Install github cli tool from https://cli.github.com/manual/installation
(use-package consult-gh
  :after consult
  :ensure (consult-gh :host github :repo "armindarvish/consult-gh")
  :config
  (setq consult-gh-default-clone-directory "~/src/vendor/"))

(use-package ediff
  :ensure nil
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-prefer-iconified-control-frame t))

;; major mode for viewing logs
(use-package logview)

(define-minor-mode ansi-color-mode
  "Apply colour escape codes to buffer."
  nil nil nil
  (ansi-color-apply-on-region 1 (buffer-size)))

;; smerge is always on anyway
(use-package smerge-mode
  :ensure nil
  :delight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   web browser
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; open file in browser
(global-set-key "\C-c\C-zv" 'browse-url-of-file)

;; set browser
(setq browse-url-browser-function 'browse-url-firefox)

;; google stuff
(use-package google-this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   system and devops
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package sudo-edit)

(use-package kubernetes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   emacs introspection
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Build and install sources, so emacs could use its introspection features:
;;;
;;; dnf download --source emacs
;;; sudo dnf install Xaw3d-devel gnutls-devel gtk3-devel ncurses-devel
;;; sudo dnf builddep emacs
;;; rpm -ivh emacs-*.src.rpm
;;; rpmbuild -bp rpmbuild/SPECS/emacs.spec
;;;
;;; The directory structure might change between versions, be sure to
;;; look at the sources in ~/rpmbuild/BUILD/emacs-* before being
;;; content with the path below.
;;;

(setq find-function-C-source-directory
      (concat "~/rpmbuild/BUILD/emacs-"
              emacs-version "-build/emacs-"
              emacs-version "/build-pgtk"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Artificial Intelligence
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vterm) ;; best Aidermacs backend

(use-package aidermacs
  :ensure t
  :after vterm
  :bind (("C-c a" . aidermacs-transient-menu))
  :custom
  (aidermacs-backend 'vterm)
  (aidermacs-default-chat-mode 'architect)
  (aidermacs-auto-commits nil)
  (aidermacs-watch-files t)
  (aidermacs-default-model "anthropic/claude-3-5-sonnet") ; General interactions
  (aidermacs-architect-model "anthropic/claude-3-5-sonnet") ; High-level planning
  (aidermacs-editor-model "gemini-2.5-flash") ; Code execution and diff generation
  (aidermacs-weak-model "gemini-2.5-flash") ; Commit messages, summaries

  :config
  (require 'secrets)

   ;; Define the helper function inside :config so it uses the loaded library
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

(provide 'dev)
;;; dev.el ends here
