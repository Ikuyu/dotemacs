;; ==========
;; Early-init
;; ==========



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

;; To start Emacs in fullscreen mode, create an Automator script with the argument -fs.

;; The early init file uses strictly built-in Emacs features to do the following:
;; - Improve startup time
;; - Set up initial frame behavior



(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq gc-cons-threshold most-positive-fixnum          ; don't collect garbage at early init while initialising
      gc-cons-percentage 0.6                          ; 'gcmh' will clean things up later and reset these settings
      default-file-name-handler-alist file-name-handler-alist ; tip from
      file-name-handler-alist nil                     ; Doom Emacs
      inhibit-startup-screen t                        ; disable the splash screen and start with an empty *scratch* buffer
      inhibit-startup-echo-area-message t             ; disable the startup message in the echo area
      initial-scratch-message nil                     ; disable the message in the scratch buffer
      package-quickstart nil                          ; do not allow loading from the package cache
      comp-deferred-compilation nil                   ; prevent unwanted runtime builds
      frame-inhibit-implied-resize t                  ; don't resize the frame in order to preserve the number of columns or lines it displays
      frame-resize-pixelwise t                        ; resize pixelwise, not character-wise
      package-enable-at-startup nil)                  ; we want to use use-package, not the default emacs behavior
