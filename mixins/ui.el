;;; ui.el --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; UI goodies.
;;; Code:

;; Stop annoying warnings I don't intent to investigate
(setq warning-minimum-level :error)

;; Don't duplicate eldoc output in minibuffer if
;; I'm using the eldoc-doc-buffer.

(setq eldoc-echo-area-prefer-doc-buffer t)

;;
;; Re-spawn scratch buffer when killed
;;

(use-package immortal-scratch
  :init
  (setq initial-scratch-message "")
  (setq initial-major-mode 'emacs-lisp-mode)
  :hook
  (after-init . immortal-scratch-mode))

(use-package info-colors
  :hook
  (Info-selection . info-colors-fontify-node)
  (Info-selection . rainbow-delimiters-mode))

(use-package which-key
  :config
  (which-key-mode))

(use-package delight
  :ensure (:wait t)
  :defer t
  :init
  (delight '((which-key-mode nil t)
             (visual-line-mode nil t)
             (eldoc-mode nil t))))

(use-package string-inflection)

(use-package visual-regexp)

(use-package visual-regexp-steroids
  :ensure (visual-regexp-steroids
           :repo "benma/visual-regexp-steroids.el"
           :files ("visual-regexp-steroids.el" "regexp.py"))
  :requires visual-regexp
  :bind (:map global-map
              ("C-s" . isearch-forward)
              ("C-r" . isearch-backward)
              ("C-q" . query-replace)
              ("C-c r" . vr/replace)
              ("C-c q" . vr/query-replace)
              ("C-c r" . vr/isearch-backward)
              ("C-c s" . vr/isearch-forward)))


(use-package multiple-cursors
  :bind (:map global-map
              ("C-c m" . vr/mc-mark)
              ("C->" . mc/mark-next-like-this)
              ("C-<" . mc/mark-previous-like-this)
              ("C-c C-<" . mc/mark-all-like-this)))

;; ibuffer
(use-package ibuffer
  :ensure nil
  :config (global-set-key [remap list-buffers] 'ibuffer))

(use-package ibuffer-vc
  :hook (ibuffer-mode . ibuffer-vc-generate-filter-groups-by-vc-root)
  :bind (:map ibuffer-mode-map ("g" . ibuffer-list-buffers))
  :init
  ;; Include version control status info in the ibuffer list.
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process)))
  :config (add-hook 'ibuffer-hook (lambda()
                                    (ibuffer-vc-set-filter-groups-by-vc-root)
                                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                                      (ibuffer-do-sort-by-alphabetic)))))

;; drag selections around
(use-package drag-stuff
  :delight
  :init (setq drag-stuff-modifier 'ctrl)
  :config (drag-stuff-global-mode t)
  (bind-keys :map drag-stuff-mode-map
             ("M-<up>" . drag-stuff-up)
             ("M-<down>" . drag-stuff-down)))


(use-package smart-mode-line
  :init
  (progn
    (setq sml/theme 'respectful
          sml/shorten-directory t
          sml/name-width 32
          sml/shorten-modes t
          sml/extra-filler 0)
    ;; sml/use-projectile-p 'before-prefixes
    ;; sml/projectile-replacement-format "%s/"

    (sml/setup)))

;; dired but nice
(use-package dirvish
  :config (dirvish-override-dired-mode))

(use-package eldoc-box
  :hook (eglot-managed-mode . eldoc-box-hover-mode))

(use-package kbd-mode
  :ensure (:host github :repo "kmonad/kbd-mode")
  :custom
  (kbd-mode-kill-kmonad "pkill -9 kmonad")
  (kbd-mode-start-kmonad
   "kmonad ~/.config/dotfiles/config.kbd")
  :hook
  (kbd-mode . (lambda () (flymake-mode -1))))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function)))

;; Nicer icons everywhere. Make sure you install them with
;; M-x nerd-icons-install-fonts
(use-package nerd-icons
  :custom
 (nerd-icons-font-family "Inconsolata Nerd Font Regular"))

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(provide 'ui)
;;; ui.el ends here.
