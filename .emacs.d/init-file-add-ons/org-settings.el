;;; ============
;;; Org settings
;;; ============


;; Author: Edwin H. Jonkvorst <hetlevenkronen@gmail.com>.
;; Keywords: internal, emacs config.
;; This file is NOT part of GNU Emacs.
;;
;; The MIT Licence (MIT)
;;
;; Copyright (C) 2023 Edwin H. Jonkvorst.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to
;; deal in the Software without restriction, including without limitation the
;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; The Software Is Provided "As Is", Without Warranty Of ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;; IN THE SOFTWARE.



;; ---------------------------
;; Extent built-in org support
;; ---------------------------
(use-package org
  :defer t
  :config
  (setq org-support-shift-select t 	   ; use shift selection in org-mode buffers also
        org-startup-indented nil           ; align subheadings and text below its respective heading and remove leading stars
        org-src-preserve-indentation nil   ; indent source code
        org-src-tab-acts-natively t        ; make TAB act as if it were issued in a buffer of the language’s major mode
        org-edit-src-content-indentation 0 ; by default source-code blocks are indented with two spaces; set it to 0
        org-src-fontify-natively t         ; enable syntax highlighting in source code blocks
        org-hide-emphasis-markers t        ; show bold/italics etc. without markup symbols
        org-startup-with-inline-images t   ; show images
        org-image-actual-width '(300)      ; set default width of a preview in pixels
        org-cycle-separator-lines -1       ; or: 0?
        org-ellipsis " ⤵"                  ; changing the org-mode ellipsis. Alternatives: {▾, ⋱, …, ⤵, ⬎, ⤷, ↴}
        org-return-follows-link t          ; use RET to follow links
        org-pretty-entities t)             ; show special symbols, such as superscript and subscript and other LATEX related special characters like \alpha
  ;; :custom-face
  ;; (org-level-1 ((t (:height 1.17))))      ; adjust the size of the headings
  ;; (org-level-2 ((t (:height 1.15))))
  ;; (org-level-3 ((t (:height 1.13))))
  ;; (org-level-4 ((t (:height 1.11))))
  ;; (org-level-5 ((t (:height 1.09))))
  ;; (org-level-6 ((t (:height 1.06))))
  ;; (org-level-7 ((t (:height 1.03))))
  ;; (org-level-8 ((t (:height 1.00))))
  ;; (org-level-1 ((t (:height 1.00))))      ; reset customizations of the headings
  ;; (org-level-2 ((t (:height 1.00))))
  ;; (org-level-3 ((t (:height 1.00))))
  ;; (org-level-4 ((t (:height 1.00))))
  ;; (org-level-5 ((t (:height 1.00))))
  ;; (org-level-6 ((t (:height 1.00))))
  ;; (org-level-7 ((t (:height 1.00))))
  ;; (org-level-8 ((t (:height 1.00))))
  )
