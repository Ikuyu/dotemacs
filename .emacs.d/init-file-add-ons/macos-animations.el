;;; ================
;;; macOS Animations
;;; ================



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



;; Start-up animation (crazy).
;;(mac-start-animation nil :type 'page-curl-with-shadow
;;                     :duration 1.0 :direction 'right :angle 45)

;; Start-up animation (less crazy).
;;(mac-start-animation (selected-window) :type 'move-out
;;                     :duration 1.0 :direction 'right)

;; Fade-outs.
(defcustom mac-animation-duration 0.3
  "Duration of transition animations")

(defvar mac-animation-locked-p nil)

(defun mac-animation-toggle-lock ()
  (setq mac-animation-locked-p (not mac-animation-locked-p)))

(defun animate-frame-fade-out (&rest args)
  (unless mac-animation-locked-p
    (mac-animation-toggle-lock)
    (mac-start-animation nil :type 'fade-out :duration mac-animation-duration)
    (run-with-timer mac-animation-duration nil 'mac-animation-toggle-lock)))

;; Fade outs everywhere.
(advice-add 'set-window-buffer :before 'animate-frame-fade-out)
(advice-add 'split-window :before 'animate-frame-fade-out)
(advice-add 'delete-window :before 'animate-frame-fade-out)
(advice-add 'delete-other-windows :before 'animate-frame-fade-out)
(advice-add 'window-toggle-side-windows :before 'animate-frame-fade-out)
