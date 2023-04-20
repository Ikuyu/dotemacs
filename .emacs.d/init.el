;;; ==========
;;; Emacs init
;;; ==========



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

;; This is where the meat of all configuration goes. This file:
;; - Add minor UI niceties (e.g. clock in the tab-bar, full-screen by default, etc.)
;; - Set the default theme (modus-vivendi)
;; - Turn on discovery aids (e.g. help-quick, which-key, etc.)

;; The order of the code in this file is important.


;; ---------------
;; Global settings
;; ---------------
(load (concat user-emacs-directory "init-file-add-ons/global-settings.el"))



;; --------
;; Packages
;; --------
(load (concat user-emacs-directory "init-file-add-ons/packages.el"))



;; --------------
;; Email settings
;; --------------
(load (concat user-emacs-directory "init-file-add-ons/email-settings.el"))



;; ------------
;; Erc settings
;; ------------
(load (concat user-emacs-directory "init-file-add-ons/erc-settings.el"))



;; ------------
;; Org settings
;; ------------
(load (concat user-emacs-directory "init-file-add-ons/org-settings.el"))



;; -----------------
;; macOS keybindings
;; -----------------
;; Remap macOS ‚å•- and ‚åò- and fn-key and add macos specific keybindings.
(when (my/macos-p)
  (setq mac-command-modifier 'super ; use ‚åò-key as super-key
        mac-option-modifier 'meta ; use ‚å•-key as meta-key
        mac-function-modifier 'hyper ; use fn-key as hyper-key
        mac-right-option-modifier 'none) ; use right ‚å•-key for special characters (e.g. √° √ü √© etc.)
  (when (not my/modus-tollens)
    (load (concat user-emacs-directory "init-file-add-ons/macos-keybindings.el"))))



;;--------------------------------
;; Buttery smooth emacs animations
;; -------------------------------
(when (my/macos-p)
  (load (concat user-emacs-directory "init-file-add-ons/macos-animations.el")))



;; ---------------
;; Structural mode
;; ---------------
(load (concat user-emacs-directory "init-file-add-ons/structural-mode.el"))



;; -----------
;; User keymap
;; -----------
(when (and (my/macos-p) (not my/modus-tollens))
  (load (concat user-emacs-directory "init-file-add-ons/user-keymap.el")))
(put 'dired-find-alternate-file 'disabled nil)
