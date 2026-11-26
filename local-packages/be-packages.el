;;; be-packages.el --- Package compatibility exceptions -*- lexical-binding: t; -*-
;;; Commentary:
;; Keep the short list of upstream releases that dropped supported Emacs here.
;;; Code:
(defun be/package-compatibility (recipe)
  "Return Emacs-version-specific overrides for Elpaca RECIPE."
  (let* ((package (format "%s" (plist-get recipe :package)))
         (repo (plist-get recipe :repo)))
    (cond
     ((equal package "delight")
      '(:repo "https://github.com/emacsmirror/delight" :depth 1))
     ((and (< emacs-major-version 30) (equal package "casual"))
      ;; Casual 2.17 requires Emacs 30.1; 2.14 satisfies Anju on Emacs 29.
      '(:tag "2.14.0"))
     ((and (version< emacs-version "29.4") (equal package "consult-gh"))
      ;; Consult-gh 2.5+ needs 29.4; Ubuntu 24.04 supplies 29.3.
      '(:tag "v2.0"))
     ((member (if (consp repo) (car repo) repo)
              '("https://github.com/emacs-mirror/emacs" "emacs-mirror/emacs"))
      ;; GNU ELPA core recipes otherwise clone the whole Emacs tree per package.
      ;; These mirrors contain the separately released ELPA package sources.
      (list :repo (concat "https://github.com/emacs-straight/" package)
            :branch "master" :depth 1 :files '(:defaults)
            :main (concat package ".el"))))))

(provide 'be-packages)
;;; be-packages.el ends here
