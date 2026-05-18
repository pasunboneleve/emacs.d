;;; sql-config.el --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; SQL-related functionality
;;; Code:

(use-package sql
  :ensure nil
  :custom (sql-product 'postgres))

(use-package dbt-mode
  :init (require 'sql)
  :ensure (:type git :host github :repo "CyberShadow/dbt-mode")
  ;; Customize `sql-product' to set the flavor of the SQL syntax.
  :custom (sql-product 'postgres))

(use-package sql-indent
  :ensure (sql-indent
           :host github
           :repo "alex-hhh/emacs-sql-indent"
           :branch "master"))

;; (use-package bigquery-mode
;;   :ensure (bigquery-mode
;;            :host github
;;            :repo "dmvianna/bigquery-mode"
;;            :branch "quote"
;;            :files ("bigquery-mode.el" "bqm-names.el"))

;;   :ensure-system-package (gcloud . google-cloud-cli))

(use-package sqlformat
  :ensure (sqlformat
           :host github
           :repo "purcell/sqlformat")
  :custom
  (sqlformat-command 'sqlfluff)
  (sqlformat-args '("--dialect" "bigquery"))
  :hook (sql-mode . sqlformat-on-save-mode))

(provide 'sql-config)
;;; sql-config.el ends here.
