;; init.el --- Summary -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;;
;;; Reload this config with M-: (load user-init-file)
;;;
;;; init file
;;; Code:

;;;
;;;
;;; File structure inspired by emacs-bedrock
;;; https://git.sr.ht/~ashton314/emacs-bedrock
;;;
;;;

;;;
;;; To reload the config, run
;;; M-: (load user-init-file)
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Package management
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; bootstrap elpaca
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;;; Populate elpaca queue with the packages that will process the config.
;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :ensure t unless otherwise specified.
  (setq use-package-always-ensure t))

;; Install system-packages
(elpaca system-packages
  (setq system-packages-use-sudo t ;; change to t if installing, else avoid it.
        system-packages-package-manager 'dnf
        async-shell-command-buffer 'new-buffer))

;; Block until current queue processed.
(elpaca-wait)

;; add modules within this directory to the scope
(add-to-list 'load-path
             (expand-file-name "local-packages" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Byte-compile and native-compile everything
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ensure Emacs loads the most recent byte-compiled files.
(setq load-prefer-newer t)

;; Ensure JIT compilation is enabled for improved performance by
;; native-compiling loaded .elc files asynchronously
(setq native-comp-jit-compilation t)
(setq native-comp-deferred-compilation t) ; Deprecated in Emacs > 29.1

(use-package compile-angel
  :delight compile-angel-on-load-mode
  :delight compile-angel-on-save-mode
  :delight compile-angel-on-save-local-mode
  :config
  ;; Set `compile-angel-verbose' to nil to silence compile-angel.
  (setq compile-angel-verbose t)

  (compile-angel-on-load-mode)
  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))

;; Ensure that quitting only occurs once Emacs finishes native compiling,
;; preventing incomplete or leftover compilation files in `/tmp`.
(setq native-comp-async-query-on-exit t)
(setq confirm-kill-processes t)

;; ;; Show buffer when there is a warning.
;; ;; (NOT RECOMMENDED, except during development).
;; (setq compile-angel-verbose t)
;; (setq warning-minimum-level :warning)
;; (setq byte-compile-verbose t)
;; (setq byte-compile-warnings t)
;; (setq native-comp-async-report-warnings-errors t)
;; (setq native-comp-warning-on-missing-source t)

;; Non-nil means to native compile packages as part of their installation.
(setq package-native-compile t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Redefining things in Lisp shouldn't be a surprise
(setq ad-redefinition-action 'accept)

;; Startup speed, annoyance suppression
;;
;; We have started, bring gc threshold back down
(setq doom-gc-cons-threshold (* 16 1024 1024)) ;; 16 MB
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold doom-gc-cons-threshold
                  gc-cons-percentage 0.1)))

;; It may also be wise to raise gc-cons-threshold while the minibuffer is active, so the GC
;; doesn’t slow down expensive commands (or completion frameworks, like helm and ivy). Here
;; is how Doom does it:
(defun doom-defer-garbage-collection-h ()
  "Don't do garbage collection during startup."
  (setq gc-cons-threshold most-positive-fixnum))

(defun doom-restore-garbage-collection-h ()
  "Defer it so that commands launched immediately after will enjoy the benefits."
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold doom-gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)

;; garbage collection is hard, let a library do it
(use-package gcmh
  :delight (gcmh-mode)
  :commands (gcmh-mode)
  :functions (gcmh-idle-garbage-collect)
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold (* 16 1024 1024))
  (gcmh-verbose nil)
  :hook
  (emacs-startup-hook . gcmh-mode))

(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))

;;; auto-byte-compile
(setq load-prefer-newer t)
(package-initialize)
(use-package auto-compile
  :config
  '((auto-compile-on-load-mode)
    (auto-compile-on-save-mode)))

;; file encoding
(prefer-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)

;; Disable backgrounding, of course. The first thing
;; a newbie in a terminal should do to remain sane.
(global-unset-key (kbd "C-z"))

;; Basic layout
;; t is True, nil is False. If you're copy-pasting 1 or -1,
;; these are also truthy and falsy values, respectively.
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; My favourite font. Do as you please.
(add-to-list 'default-frame-alist '(font . "Inconsolata-16"))

;; No electric indent
(electric-indent-mode -1)

;; this information is useless for most
(setq display-time-default-load-average nil)

;; Automatically reread from disk if the underlying file changes
(setq auto-revert-interval 3)
(setq auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; line wrap by default
(global-visual-line-mode t)
;; but when I want to truncate, I mean it
(setq truncate-partial-width-windows nil)

;; Save history of minibuffer
(savehist-mode)

;;; Save all tempfiles in $TMPDIR/emacs-$UID/
(defconst emacs-tmp-dir
  (format "%s/%s%s/" temporary-file-directory "emacs-" (user-uid)))
(unless (file-directory-p emacs-tmp-dir)
  (make-directory emacs-tmp-dir))
(setq backup-directory-alist
      `(("." . ,emacs-tmp-dir)))
(setq undo-tree-history-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))

;;; I want to work on the file, not the backup
(setq backup-by-copying t)

;; Keep customisations out of my config
(setq custom-file (concat user-emacs-directory "custom.el.gpg"))
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer/completion settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion

(setq enable-recursive-minibuffers t)                             ; Use the minibuffer whilst in the minibuffer
(setq completion-cycle-threshold 3)                               ; TAB cycles candidates
(setq completions-detailed t)                                     ; Show annotations
(setq tab-always-indent 'complete)                                ; When I hit TAB, try to complete, otherwise, indent
(setq tab-first-completion 'word-or-paren-or-punct)

(fido-vertical-mode)                                              ; Show completion candidates in a vertical, interactive list
(setq completion-styles '(basic initials substring))              ; Different styles to match input to candidates
(define-key minibuffer-mode-map (kbd "TAB") 'minibuffer-complete) ; TAB acts more like how it does in the shell

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Mouse
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mouse-wheel-mode t)
(xterm-mouse-mode t)

;;; Mouse color must be the same in emacs-daemon
(setq mouse-color "#002b36") ;;; solarized-dark light gray
(set-mouse-color mouse-color) ;;; that's emacs

;;; Keep mouse color in GUI too
(require 'frame)
(defun set-default-hook (frame)
  "Set all default frame logic.  FRAME is the frame being created."
  (modify-frame-parameters
   frame (list (cons 'mouse-color mouse-color))))

;; Enable horizontal scrolling
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)

;; mouse wheel in iterm2 terminal
(require 'mwheel)
(require 'mouse)
(xterm-mouse-mode t)
(mouse-wheel-mode t)
(global-set-key [mouse-4] 'next-line)
(global-set-key [mouse-5] 'previous-line)

;; really fast scrolling with PGTK

(defun up-single () "Up single." (interactive) (scroll-up 1))
(defun up-double () "Up double." (interactive) (scroll-up 2))
(defun up-triple () "Up triple." (interactive) (scroll-up 3))
(defun down-single () "Down single." (interactive) (scroll-down 1))
(defun down-double () "Down double." (interactive) (scroll-down 2))
(defun down-triple () "Down triple." (interactive) (scroll-down 3))

(global-set-key [wheel-down] 'up-single)
(global-set-key [double-wheel-down] 'up-double)
(global-set-key [triple-wheel-down] 'up-triple)
(global-set-key [wheel-up] 'down-single)
(global-set-key [double-wheel-up] 'down-double)
(global-set-key [triple-wheel-up] 'down-triple)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   My preferred default interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 '(use-file-dialog nil nil nil "'use-file-dialog' hangs emacs 29 in pgtk."))

;; go to windows by number
(use-package winum
  :config (winum-mode))

;; smooth scrolling (emacs 29)
(if (version< emacs-version "29") (pixel-scroll-mode 1) (pixel-scroll-precision-mode 1))
(customize-set-variable 'frame-resize-pixelwise t)
(customize-set-variable 'window-resize-pixelwise t)

;; directory tree view

(defun ignore-packages (path patterns)
     "Return non-nil if PATH contain any of the PATTERNS."
     (let* ((path-parts
             (split-string
              (file-name-directory (expand-file-name path)) "/" t))
            (matches
             (cl-loop for elt in patterns
                      collect
                      (cl-remove-if-not
                       (lambda (x) (string-equal elt x)) path-parts))))
       (remove nil matches)))

(use-package treemacs
  :custom
  (treemacs-show-hidden-files t)
  (treemacs-file-follow-ignore-functions
   '((lambda (path)
       (ignore-packages
        path
        '(".venv" "node_modules" ".metals" "elpaca")))))
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window)))

;; make resize seamless in Wayland
(setq frame-resize-pixelwise t
      x-frame-normalize-before-maximize t
      frame-inhibit-implied-resize t)
(push '(fullscreen . maximized) initial-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)

;; we can redisplay now, boot is over
(add-hook 'window-setup-hook
          (lambda ()
            ;; leave this inhibit-redisplay alone! We're setting it
            ;; back to its original value.
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (default-frame-layout-hook (selected-frame))))

;;; focus on emacs frame when it is started
(add-hook 'server-switch-hook #'raise-frame)

;;; make a frame hook function
(defun default-frame-layout-hook (frame)
  "Select the FRAME and apply the default layout."
  (set-default-hook frame)
  (select-frame-set-input-focus frame)
  (raise-frame)
  (when (display-graphic-p frame)
    (my-maximized-frame-layout)))

;;; make a maximized function
(defun my-maximized-frame-layout ()
  "Make Emacs frame use all the screen area."
  (progn
    (scroll-bar-mode -1)
    (split-window-horizontally)
    (if (and (featurep 'treemacs)
             (not (eq (treemacs-current-visibility)
                      'visible)))
        (treemacs))))

;;; that's what emacs-daemon uses
(add-hook
 'after-make-frame-functions
 #'default-frame-layout-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Interface enhancements/defaults
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Switch windows easily
(bind-key* "M-o" 'other-window)

;; Mode line information
(setq line-number-mode t)                         ; Show current line in modeline
(setq column-number-mode t)                       ; Show column as well

(setq x-underline-at-descent-line nil)            ; Prettier underlines
(setq switch-to-buffer-obey-display-actions t)    ; Make switching buffers more consistent

(setq-default show-trailing-whitespace nil)       ; By default, don't underline trailing spaces
(setq-default indicate-buffer-boundaries 'left)   ; Show icons showing the size of the buffer in the margin

;;; Set tab width
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

;; Misc. UI tweaks
(blink-cursor-mode nil)           ; Steady cursor
;; (pixel-scroll-precision-mode)     ; Smooth scrolling
(global-hl-line-mode)             ; Highlight the current line

;; bind home and end keys to beginning and end of file
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

(use-package prog-mode
  :ensure nil
  :init
  (add-to-list 'auto-mode-alist '("\\.list$" . prog-mode))
  ;; Display line numbers
  :hook
  (prog-mode . display-line-numbers-mode) ;; linum-mode, but efficient
  ;; open pairs with extra newline in between, and autoindent
  (prog-mode . electric-pair-local-mode))

;; Do not use shift-arrow to move between windows
;; as it conflicts with org-mode keybindings.
(require 'windmove)
(windmove-default-keybindings 'control)

;;; change capitalisation
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; dired enhancements
(with-eval-after-load 'dired
  (setq dired-x-hands-off-my-keys nil)
  (require 'dired-x))

(setopt undo-limit 160000000)  ;; 1000 x more than default
(setopt undo-strong-limit 2400000000)  ;; 1000 x more than default
;; ;; Display sane file names
;; (require 'uniquify)
;; (setq uniquify-buffer-name-style 'forward)

;; Disable emoji picker / rebind it in Gnome
;; I can't do it from within emacs but I need somewhere to put my documentation,
;; so here it is:
;;
;; List relevant info:
;;
;; $ gsettings list-recursively org.freedesktop.ibus.panel.emoji
;;
;; It will list the default:
;;
;; > org.freedesktop.ibus.panel.emoji hotkey ['<Super>period']
;;
;; Replace with something safe:
;;
;; $ gsettings set org.freedesktop.ibus.panel.emoji hotkey '["<Super>e"]'
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tab-bar configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show the tab-bar as soon as tab-bar functions are invoked
(setq tab-bar-show 0)

;; Add the time to the tab-bar, if visible
;; (add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
;; (add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
;; (setq display-time-format "%a %F %T")
;; (setq display-time-interval 1)
;; (display-time-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package solarized-theme
  :config
  (load-theme 'solarized-dark t))

(use-package all-the-icons)
(use-package all-the-icons-dired
  :requires all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Optional mixins
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Uncomment these lines or copy from the mixin/ files as you see fit

;; UI/UX enhancements mostly focused on minibuffer and autocompletion interfaces
(load-file (concat user-emacs-directory "mixins/ui.el"))

;; Packages for software development
(load-file (concat user-emacs-directory "mixins/dev.el"))

;; Language support
(load-file (concat user-emacs-directory "mixins/languages.el"))

;; File viewers
(load-file (concat user-emacs-directory "mixins/viewers.el"))

;; Minibuffer & popups
(load-file (concat user-emacs-directory "mixins/minibuffers.el"))

;; Artificial Intelligence
(load-file (concat user-emacs-directory "mixins/ai.el"))

;; Entertainment
(load-file (concat user-emacs-directory "mixins/entertainment.el"))

;; Email and internet messaging
(load (concat user-emacs-directory "mixins/communication.el.gpg") t nil nil t)

;; Orgmode
(load (concat user-emacs-directory "mixins/org-config.el"))

;; Tramp
(load (concat user-emacs-directory "mixins/tramp-config.el"))

;; Safe
(load (concat user-emacs-directory "mixins/safe.el.gpg"))

;; Modes that must be loaded early
(load-file (concat user-emacs-directory "mixins/early-modes.el"))

(provide 'init)
;;; init.el ends here
