;;; ===============
;;; Structural mode
;;; ===============


;; Author: Edwin H. Jonkvorst <hetlevenkronen@gmail.com>.
;; Keywords: internal, emacs config.
;; This file is NOT part of GNU Emacs.
;;
;; The MIT Licence (MIT)
;;
;; Copyright (C) 2023 Edwin H. Jonkvorst.
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; The Software Is Provided "As Is", Without Warranty Of ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.



;; --------------
;; Custom package
;; --------------
(use-package my/structural-mode
  :ensure nil
  :hook ((lisp-mode
	  emacs-lisp-mode
	  scheme-mode
	  lisp-interaction-mode) . my/structural-mode)
  :bind (:map my/structural-mode-map
              ("C-M-[" . my/structural-slurp-sexp)
              ("C-M-]" . my/structural-barf-sexp))
  :preface
  (defcustom my/structural-pair-alist
    '((?\" . ?\")
      (?\( . ?\))
      (?\[ . ?\])
      (?\{ . ?\}))
    "Alist of pair characters."
    :group 'structural-mode
    :type '(alist :key-type character :value-type character))
  (defun my/structural--sexp-bounds ()
    "Return a cons cell with positions at the start and end of expression."
    (save-excursion
      (up-list -1 'escape-strings 'no-syntax-crossing)
      (let ((start (point)))
        (forward-sexp)
        (cons start (point)))))
  (defun my/structural--matching-delim (char)
    "Return matching delimiter for CHAR."
    (if-let ((pair (assoc char my/structural-pair-alist)))
        (cdr pair)
      (if-let ((pair (rassoc char my/structural-pair-alist)))
          (car pair)
        (user-error "no matching character for %s"
                    (char-to-string char)))))
  (defun my/structural-slurp-sexp (&optional n)
    "Move closing delimiter N expressions forward.

In:  (foo █bar) baz
Out: (foo █bar baz)"
    (interactive "p")
    (condition-case e
        (save-excursion
          (let* ((bounds (my/structural--sexp-bounds))
                 (start (car bounds))
                 (end (cdr bounds))
                 (delim (char-after start)))
            (goto-char end)
            (forward-sexp n)
            (insert (my/structural--matching-delim delim))
            (goto-char end)
            (delete-char -1))
          (provide 'my/structural-mode))
      (error (message "%s" (cadr e)))))
  (defun my/structural-barf-sexp (&optional n)
    "Move closing delimiter N expressions forward.

In:  (foo █bar baz)
Out: (foo █bar) baz"
    (interactive "p")
    (condition-case e
        (save-excursion
          (let* ((bounds (my/structural--sexp-bounds))
                 (start (car bounds))
                 (end (cdr bounds))
                 (delim (char-after start)))
            (goto-char end)
            (delete-char -1)
            (condition-case e
                (progn
                  (forward-sexp -1)
                  (forward-sexp (- n))
                  (forward-sexp 1))
              (error (message "%s" (cadr e))))
            (insert (my/structural--matching-delim delim))))
      (error (message "%s" (cadr e)))))
  (define-minor-mode my/structural-mode
    "Simple structural editing mode.

\\<structural-mode-map>"
    :lighter " (λ)"
    :group 'editing
    :keymap (make-sparse-keymap))
  (provide 'my/structural-mode))
