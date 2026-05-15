;;; Package --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Java configuration
;;; Code:

(require 'cl-generic)
(require 'eglot)
(require 'subr-x)

(defvar +eglot/initialization-options-map (make-hash-table :size 5))

(cl-defmethod eglot-initialization-options ((server eglot-lsp-server))
  (if-let ((init-options (gethash (eglot--major-mode server) +eglot/initialization-options-map)))
      init-options
    eglot--{}))

(add-to-list 'eglot-server-programs
             `(java-mode "jdtls"
                         "-configuration" ,(expand-file-name "cache/language-server/java/jdtls/config_linux" user-emacs-directory)
                         "-data" ,(expand-file-name "cache/java-workspace" user-emacs-directory)
                         ,(concat "--jvm-arg=-javaagent:" (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.20/lombok-1.18.20.jar"))))

(puthash 'java-mode
         `(:settings
           (:java
            (:configuration
             (:runtime [(:name "JavaSE-1.8" :path "/usr/local/jdk-8")
                        (:name "JavaSE-11" :path "/usr/local/graalvm-ce-java11-22.0.0.2")
                        (:name "JavaSE-17" :path "/usr/local/graalvm-ce-java17-22.0.0.2" :default t)])
             :format (:settings (:url ,(expand-file-name (locate-user-emacs-file "cache/eclipse-java-google-style.xml"))
                                      :profile "GoogleStyle"))
             ;; NOTE: https://github.com/redhat-developer/vscode-java/issues/406#issuecomment-356303715
             ;; > We enabled it by default so that workspace-wide errors can be reported (eg. removing a public method in one class would cause compilation errors in other files consuming that method).
             ;; for large workspaces, it may make sense to be able to disable autobuild if it negatively impacts performance.
             :autobuild (:enabled t)
             ;; https://github.com/dgileadi/vscode-java-decompiler
             :contentProvider (:preferred "fernflower")))
           ;; WIP: support non standard LSP `java/classFileContents', `Location' items that have a `jdt://...' uri
           ;; https://github.com/eclipse/eclipse.jdt.ls/issues/1384
           ;; nvim impl demo: https://github.com/mfussenegger/dotfiles/commit/3cddf73cd43120da2655e2df6d79bdfd06697f0e
           ;; lsp-java impl demo: https://github.com/emacs-lsp/lsp-java/blob/master/lsp-java.el
           :extendedClientCapabilities (:classFileContentsSupport t)
           ;; bundles: decompilers, etc.
           ;; https://github.com/dgileadi/dg.jdt.ls.decompiler
           :bundles ,(let ((bundles-dir (expand-file-name (locate-user-emacs-file "cache/language-server/java/bundles" user-emacs-directory))))
                       (apply #'vector
                              (when (file-directory-p bundles-dir)
                                (directory-files bundles-dir t "\\.jar$")))))
         +eglot/initialization-options-map)

(cl-defmethod eglot-execute-command
  (_server (_cmd (eql java.apply.workspaceEdit)) arguments)
  "Eclipse JDT breaks spec and replies with edits as arguments."
  (mapc #'eglot--apply-workspace-edit arguments))

(provide 'java-config)
;;; java-config.el ends here
