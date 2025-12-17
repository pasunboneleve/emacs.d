;;; avro-mode.el -- major mode for editing Avro IDL files. -*- coding: utf-8; lexical-binding: t; -*-

;;; Copyright © by Daniel Vianna
;;; Version: 0.0.2
;;; Created: 19 Aug 2021

;;; Keywords: languages

;;; This file is not part of GNU Emacs.

;;; License:

;;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Package --- summary
;;; Commentary:
;;;
;;; Avro interface description language mode (.avdl)
;;; Code:

(require 'generic-x)

(define-generic-mode
    'avro-mode
  '("//") ;; comments
  '("protocol" "namespace") ;; reserved words
  '(("\\<[[:alnum:]_]+\\>" (0 font-lock-builtin-face) ("\\<[[:alnum:]_]+\\>;?\\( {\\)?" nil nil (0 nil))) ;; type
    ("\\<\\(?:namespace\\|protocol\\)\\>" . font-lock-keyword-face)

    ("{\\|}\\|;\\|<\\|>" . font-lock-constant-face)) ;; separators
  '("\\.avdl$") ;; file extension
  nil
  "Major mode for editing Avro IDL files.")


(provide 'avro-mode)

;;; avro-mode.el ends here
