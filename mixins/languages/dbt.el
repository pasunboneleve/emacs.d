;;; dbt.el --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Stuff for dbt
;;; Code:

(use-package poly-mode)

(use-package poly-sql
  :mode ("\\.sql\\'" . poly-sql-mode))

;; Or simpler hybrid with mmm-mode (more lightweight)
(use-package mmm-mode
  :config
  (mmm-add-mode-ext-class 'sql-mode nil 'jinja2)
  :mode (("\\.sql\\'" . sql-mode)))


(add-to-list 'eglot-server-programs
             '((sql-mode) . ("sqls" "-config" "~/.config/sqls/config.yml")))

(add-hook 'sql-mode-hook 'eglot-ensure)
;; go install github.com/sqls-server/sqls@latest

;; clone https://github.com/ericpar/emacs-dbt to ~/.config/emacs/snippets/emacs-dbt
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/emacs-dbt")


(defun dbt-run-current-model (extra-args)
  "Run dbt run `--select' CURRENT_MODEL [EXTRA-ARGS]."
  (interactive "sExtra dbt args (e.g. --full-refresh --threads 4): ")
  (if (buffer-file-name)
      (let* ((full-file (buffer-file-name))
             (model-name (file-name-sans-extension
                          (file-name-nondirectory full-file))))
        (compile (format "dbt run --select %s %s" model-name extra-args)))
    (message "No file associated with this buffer.")))

(global-set-key (kbd "C-c d r") 'dbt-run)

(provide 'dbt)
;;; dbt.el ends here
