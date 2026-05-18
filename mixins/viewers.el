;;; viewers.el --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;; File viewers.
;;; Code:

(use-package doc-view
  :ensure nil
  :bind (:map
         doc-view-mode-map
         ("<mouse-4>" . doc-view-scroll-down-or-previous-page)
         ("<mouse-5>" . doc-view-scroll-up-or-next-page)
         ("<mouse-6>" . image-scroll-right)
         ("<mouse-7>" . image-scroll-left)
         ("<mouse-8>" . image-decrease-size)
         ("<mouse-9>" . image-increase-size)))

(use-package pdf-tools
  :custom (pdf-view-display-size 'fit-height)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook
  (pdf-view-mode . (lambda () (display-line-numbers-mode -1))))

(use-package image-mode
  :ensure nil
  :bind (:map
         image-mode-map
         ("<mouse-4>" . image-scroll-down)
         ("<mouse-5>" . image-scroll-up)
         ("<mouse-6>" . image-scroll-right)
         ("<mouse-7>" . image-scroll-left)
         ("<mouse-8>" . image-decrease-size)
         ("<mouse-9>" . image-increase-size)))

(use-package nov
  :mode
  ("\\.epub$" . nov-mode))

(provide 'viewers)
;;; viewers.el ends here.
