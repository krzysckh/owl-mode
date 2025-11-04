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
;; it was written mainly to add some keyword highlighting (the ones i like :3)

;;; Code:

(require 'scheme)

(defface owl-warning-face
  `((((class color) (background light))
     (:foreground "Orange"))
    (((class color) (background dark))
     (:foreground "Orange"))
    (t :weight bold))
  "Face used to highlight keywords you probably shouldn't maybe touch? idk??.")

(defvar owl-warning-face 'owl-warning-face)

(defvar owl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-shared-map)
    map)
  "mode map for owl mode")

;; this mostly duplicates scheme-indent-function
;; which mostly duplicates lisp-indent-function
(defun owl-indent-function (indent-point state)
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (get (intern-soft function) 'scheme-indent-function)
                         (get (intern-soft function) 'owl-indent-function)
                         (get (intern-soft function) 'scheme-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function))
                   (string-match "\\`with" function))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
                (funcall method state indent-point normal-indent)))))))

(easy-menu-define owl-mode-menu owl-mode-map
  "Menu for Owl Lisp mode."
  '("Owl Lisp"
    ["Run scheme" run-scheme]
    ))

(define-derived-mode owl-mode scheme-mode "Owl Lisp"
  "Mode for editing Owl Lisp code."
  (progn
    (setq-local lisp-indent-function 'owl-indent-function)
    (setq font-lock-defaults
          '((owl-font-lock-keywords)
            nil t (("+-*/.<>=!?$%_&~^:" . "w") (?#. "w 14"))
            beginning-of-defun
            (font-lock-mark-block-function . mark-defun)))))

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
       '(;; let-like
         "lets" "if-lets" "tuple-case" "lets/cc" "pipe" "piper"
         ;; fasl stuff
         "fasl-encode" "fasl-decode" "fasl-save" "fasl-load"
         ;; etc
         "call/cc2" "call/cc3" "force-ll"
         ) t)
      "\\>")
     1)
    (cons
     (concat
      "("
      (regexp-opt
       '("thread" "mail" "error" "interact" "accept-mail"
         "wait-mail" "next-mail" "check-mail" "kill" "link" "thunk->thread"
         "exit-owl" "exit-thread" "fork-named"
         ) t)
      "\\>")
     '(1 font-lock-builtin-face))
    (cons
     (concat
      "("
      (regexp-opt
       '(;; should you really be doing that?
         "run" "set-ticker" "bind" "mkt" "halt" "set-memory-limit"
         "get-word-size" "get-memory-limit" "sys-prim"
         ) t)
      "\\>")
     '(1 owl-warning-face))
    (cons "(\\(with\\(?:-[^ ]+\\)?\\)\\>" 1)
    (cons "(\\(define[^ ]+\\)\\>" 1)
    ;; (cons "(\\([^ ]+!\\)\\>" '(1 owl-warning-face)) ; <- TODO: is this nice or too much?
    )))

(put 'lets 'owl-indent-function 1)
(put 'if-lets 'owl-indent-function 1)
(put 'lets/cc 'owl-indent-function 1)
(put 'pipe 'owl-indent-function 1)
(put 'piper 'owl-indent-function 1)
(put 'tuple-case 'owl-indent-function 1)

(defalias 'owl-lisp-mode 'owl-mode)

(provide 'owl)

;;; owl.el ends here
