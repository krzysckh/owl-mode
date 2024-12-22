;;; owl.el --- Owl Lisp editing mode -*- lexical-binding: t; -*-

;; Author: Krzysztof Michałczyk <kpm@krzysckh.org>

;; Copyright (C) 2024  Krzysztof Michałczyk <kpm@krzysckh.org>
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;;     * Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following disclaimer
;; in the documentation and/or other materials provided with the
;; distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; this is a tiny major mode for editing Owl Lisp.
;; it was written mainly to add some keyword highlighting and indent-function thing
;; also sorry for overwriting scheme-indent-function with lets and tuple-case etc
;; but whateVer lol

;;; Code:

(require 'scheme)

(defvar owl-mode-map
  "mode map for owl mode"
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-shared-map)
    map))

(easy-menu-define owl-mode-menu owl-mode-map
  "Menu for Owl Lisp mode."
  '("Owl Lisp"
    ["Run scheme" run-scheme]
    ))

(define-derived-mode owl-mode scheme-mode "Owl Lisp"
  "Mode for editing Owl Lisp code."
  (setq font-lock-defaults
        '((owl-font-lock-keywords)
          nil t (("+-*/.<>=!?$%_&~^:" . "w") (?#. "w 14"))
          beginning-of-defun
          (font-lock-mark-block-function . mark-defun))))

(defgroup owl nil
  "Editing Owl Lisp code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'lisp)

(defcustom owl-mode-hook nil
  "Normal hook run when entering `owl-mode'"
  :type 'hook)

(defconst owl-font-lock-keywords
  (append
   scheme-font-lock-keywords-1
   scheme-font-lock-keywords-2
   (list
    (cons
     (concat
      "("
      (regexp-opt
       '("lets" "if-lets" "tuple-case"
         ) t)
      "\\>")
     1))))

(put 'lets 'scheme-indent-function 1)
(put 'if-lets 'scheme-indent-function 1)
(put 'tuple-case 'scheme-indent-function 2)

(provide 'owl)

;;; owl.el ends here
