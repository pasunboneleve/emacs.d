;;; Package --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Scala configuration
;;; Code:

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :interpreter ("scala" . scala-mode)
  :mode "\\.sc\\'\\|\\.scala\\'"
  ;; We need to tell project.el in which directory to run in order for
  ;; eglot to be able to trigger metals and be able to find
  ;; definitions and all the good stuff.
  :init (add-to-list 'project-vc-extra-root-markers "build.sbt")
  :hook
  (scala-mode . envrc-mode)
  (scala-mode . eglot-ensure))
;; :init
;; (setq-local eglot-workspace-configuration
;;             '((metals (scalafmt-config-path (".scalafmt.conf"))))
;;             )


;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  (defun fix-sbt-movement ()
    (local-set-key (kbd "C-p") 'comint-previous-input)
    (local-set-key (kbd "C-n") 'comint-next-input))
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  (setq sbt:program-options
        '("-Dsbt.supershell=false"
          "-J-XX:+UseParallelGC" ; improve GC performance by leveraging multiple processors
          "-J-Xmx2g" ; maximum size of the memory allocation pool
          "-J-Xms100m" ; minimum and initial size of the heap
          "-J-Xss4m" ; thread stack size
          "-Dmetals.client=emacs")) ; configure metals for emacs
  :hook (sbt-mode . fix-sbt-movement))

(provide 'scala-config)
;;; scala-config.el ends here
