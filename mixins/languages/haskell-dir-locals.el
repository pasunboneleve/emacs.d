;;; Package --- dir-locals.el -*- lexical-binding: t; -*-
;;; Summary: This project's configurations for emacs.
;;;
;;; Commentary:
;;;
;;; If you don't use the `emacs' editor, you can safely ignore this file.
;;; If you do use it, you may want to copy it to `.dir-locals.el'.  It will
;;; set up some configuration variables that will only work when Emacs is
;;; visiting this project.
;;;
;;; To silence `emacs' requesting confirmation on whether you want to
;;; apply the `risky' compilation-related variables below, just add
;;; the following code to your `emacs' configuration:
;;;
;;; (put 'eglot-workspace-configuration 'safe-local-variable #'symbolp)
;;; (put 'haskell-process-type 'safe-local-variable #'symbolp)
;;; (put 'haskell-compile-cabal-build-command 'safe-local-variable #'stringp)
;;; (put 'haskell-compile-cabal-build-alt-command 'safe-local-variable #'stringp)
;;;
;;; Notice that we chose to bind `haskell-compile-cabal-build-command'
;;; and `haskell-compile-cabal-build-alt-command' to Cabal's `build'
;;; command. Of course you're free to bind these to whatever is
;;; convenient to you. Don't forget to bind `haskell-compile' to a
;;; convenient keystroke, such as "C-c C-c". This will trigger a
;;; compile, while "C-u C-c C-c" will let you edit the command before
;;; executing it.
;;;
;;; Shockingly, `haskell-mode' doesn't have a default command for
;;; testing, but we included a keybinding for that here: just use
;;; `C-c' `t' or rebind to your heart's pleasure.
;;;
;;; Code:

((nil . ((eglot-workspace-configuration
          . (:haskell
             (:formattingProvider
              "fourmolu" :plugin
              (:importQualifiedPost (:globalOn t))
              :checkProject nil)
             :nil (:formatting (:command ["nixpkgs-fmt"]))))
         (haskell-process-type . 'cabal-repl)
         (cabal-project . "./api/cabal-dev.project")
         (haskell-compile-cabal-build-alt-command . "cabal build -j --ghc-option=-ferror-spans --ghc-option=-fdefer-type-errors --builddir=dist-custom-build/dev --project-file=cabal-dev.project")
         (haskell-compile-cabal-build-command . "cabal build -j --ghc-option=-ferror-spans --ghc-option=-fdefer-type-errors --builddir=dist-custom-build/dev --project-file=cabal-dev.project")))
      (haskell-mode
       . ((haskell-mode-map
           . (let ((map (make-sparse-keymap)))
               (define-key map (kbd "C-c C-,") 'haskell-mode-format-imports)
               (define-key map [remap delete-indentation]
                           'haskell-delete-indentation)
               (define-key map (kbd "C-c C-l")
                           'haskell-mode-enable-process-minor-mode)
               (define-key map (kbd "C-c C-b")
                           'haskell-mode-enable-process-minor-mode)
               (define-key map (kbd "C-c C-v")
                           'haskell-mode-enable-process-minor-mode)
               (define-key map (kbd "C-c C-t")
                           '(lambda nil (interactive) (save-some-buffers t)
                             (setq-local haskell-compile-cabal-build-command
                                         "cabal test -j --ghc-option=-ferror-spans --ghc-option=-fdefer-type-errors --builddir=dist-custom-build/dev --project-file=cabal-dev.project")
                             (haskell-compile)))
               (define-key map (kbd "C-c C-i")
                           'haskell-mode-enable-process-minor-mode)
               (define-key map (kbd "C-c C-s") 'haskell-mode-toggle-scc-at-point)
               map)))))

(provide 'dir-locals.el)
;;; .dir-locals.el ends here.
