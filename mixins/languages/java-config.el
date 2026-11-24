;;; java-config.el --- Java support -*- lexical-binding: t; -*-
;;; Commentary:
;; Use Eglot's server discovery and the project's JDK environment.
;;; Code:
(require 'eglot)
(add-hook 'java-mode-hook #'be/eglot-ensure)
(add-hook 'java-ts-mode-hook #'be/eglot-ensure)

(cl-defmethod eglot-execute-command
  (_server (_command (eql java.apply.workspaceEdit)) arguments)
  "Apply Eclipse JDT's nonstandard workspace edit response."
  (mapc #'eglot--apply-workspace-edit arguments))

(provide 'java-config)
;;; java-config.el ends here
