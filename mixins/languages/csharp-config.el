;;; csharp-config.el --- Csharp support -*- lexical-binding: t; -*-
;;; Commentary:
;; Loaded on first use.
;;; Code:

(use-package csharp-mode
 :ensure nil
 :hook ((csharp-mode . be/eglot-ensure)
        (csharp-ts-mode . be/eglot-ensure))
 :config

 (with-eval-after-load 'eglot
   (add-to-list 'eglot-server-programs
                `((csharp-mode csharp-ts-mode)
                  . ,(lambda (_interactive)
                       ;; OmniSharp may start from the wrapper's cwd.
                       ;; Force the actual project root so it loads the .csproj/.sln.
                      (list "omnisharp" "-lsp" "-s"
                            (if-let ((project (project-current)))
                                (project-root project) default-directory)))))))

(use-package csproj-mode
  :mode ("\\.csproj\\'" . csproj-mode))

(provide 'csharp-config)
;;; csharp-config.el ends here
