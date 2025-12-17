;;; technique-mode.el -- Emacs major mode for the Technique Procedure Language -*- coding: utf-8; lexical-binding: t; -*-

;;; Copyright © by Daniel Vianna
;;; Version: 0.1.0
;;; Created: 17 December 2025

;;; Keywords: languages

;;; This file is not part of GNU Emacs.

;;; License:

;;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:
;;;
;;; Code:

(require 'generic-x)

(define-generic-mode
    'technique-mode
  nil ;; comments
  '("repeat") ;; reserved words
  '(
    ("^% technique .*" . font-lock-preprocessor-face) ;; version
    ("^\\(!\\|&\\)\s.*" . font-lock-preprocessor-face) ;; copyright
    ("^# [[:alnum:] _]+" . font-lock-doc-markup-face) ;; procedure
    ("^[[:alnum:] _\\(\\)]+\\\s:\s.*" . font-lock-type-face) ;; functions
    ("^\s*\\([[:alnum:]]+\\)\\.\s\\{1,2\\}" . font-lock-comment-face) ;; lists
    ("{\\|}\\|;\\|<\\|>\\|\\[\\|\\]\\||" . font-lock-comment-face) ;; separators
    ("=\\|~" . font-lock-keyword-face) ;; operators
    ("```[[:alnum:]]*\\|'[[:alnum:]]+'" . font-lock-preprocessor-face) ;;; code
    ("\\(\\^\\|\\@\\)[[:alnum:]_]+" . font-lock-constant-face)) ;; named actor
  '("\\.tq$") ;; file extension
  nil
  "Major mode for editing Technique Procedure Language files.")


(provide 'technique-mode)

;;; technique-mode.el ends here
