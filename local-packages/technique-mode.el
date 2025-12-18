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
  '("repeat" "exec") ;; reserved words
  '(
    ;; The following regex matches standard double-quoted strings.
    ;; The (0 'default t) applies the default face to the entire match,
    ;; overriding any prior string highlighting rules.
    ("\"[^\"]*\"" 0 'default t)

    ;; header metadata
    ("^\s*%.*$" . font-lock-warning-face)              ;; header
    ("^\s*!.*$" . font-lock-warning-face)              ;; SPDX
    ("^\s*&.*$" . font-lock-warning-face)              ;; template

    ;; titles
    ("^\s*#" . font-lock-comment-face)                 ;; title marker
    ("^\s*#\\(.*\\)$" . (1 font-lock-warning-face))    ;; title

    ;; sections
    ("^\s*[IVX]+." . font-lock-comment-face)           ;; uppercase roman numerals

    ;; steps
    ("^\s*\d+.\s" . font-lock-comment-face)            ;; number
    ("^\s*[a-hj-uw-z].\s" . font-lock-comment-face)    ;; substep letter
    ("^\s*[ivx]+.\s" . font-lock-comment-face)         ;; subsubstep roman
    ("^\s*-\s" . font-lock-warning-face)               ;; parallel step
    ("^[[:alnum:] _\\(\\)]+\\\s:\s.*" . font-lock-type-face) ;; procedure
    ("^\s*\\([[:alnum:]]+\\)\\.\s\\{1,2\\}" . font-lock-comment-face) ;; lists
    ("{\\|}\\|;\\|<\\|>\\|\\[\\|\\]\\||" . font-lock-comment-face) ;; separators
    ("=\\|~" . font-lock-keyword-face) ;; operators
    ("```[[:alnum:]]*\\|'[[:alnum:]]+'" . font-lock-preprocessor-face) ;;; code
    ("\\(\\^\\|\\@\\)[[:alnum:]_\\*]+" . font-lock-constant-face)) ;; named actor
  '("\\.tq$") ;; file extensions
  nil
  "Major mode for editing Technique Procedure Language files.")


(provide 'technique-mode)

;;; technique-mode.el ends here
