;;; ===============
;;; Global Settings
;;; ===============



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



;; Transparent background, excluding images and text (feature of Emacs 29):
;; (when my/macos-p)
;;   (setq default-frame-alist '((ns-transparent-titlebar . t)
;;                               (ns-appearance . dark)
;;                               (alpha . (80 . 75))))) ; or ( alpha-background . 0.9)
;; Emacs 29 has support for pixel scrolling:
;; (pixel-scroll-precision-mode)

(global-set-key [remap dabrev-expand] 'hippie-expand) ; Use M-/ to cycle through completions. Emacs 29: (keymap-global-set "M-/" 'hippie-expand)

;; Flag to disable the:
;; - mouse
;; - macos-keybindings
;; - user-keymap
;; - arrow, end, home and delete keys as well as their control and met
;;   prefixes
;; - macOS specific undo/redo behavior.
(defvar my/modus-tollens nil)

;; Function: check whether the system is Darwin/macOS or not.
(defun my/macos-p ()
  "Return non-nil if system is Darwin-based (macOS)"
  (string-equal system-type "darwin"))

;; Override historic undo-redo behaviour.
(when (and (my/macos-p) (not my/modus-tollens))
  (setq undo-no-redo t))

;; Adjust globally applied local settings.
(setq-default case-fold-search nil               ; enable case-sensitive search & replace
	      indent-tabs-mode nil   		 ; never mix tabs and spaces, never use tabs
              cursor-type 'bar
              word-wrap t
              fill-column 79)                    ; used in auto-fill-mode, ignored in visual-line-mode

;; Adjust global settings (variables).
(setq electric-pair-pairs '((?\{ . ?\}) (?\< . ?\>)) ; extend electrc-pair-mode with curly braces etc.
      fast-but-imprecise-scrolling t             ; accelerate scrolling operations
      inhibit-compacting-font-caches t           ; don’t compact font caches during garbage collection
      save-interprogram-paste-before-kill t      ; ensure that Emacs kill operations do not irrevocably overwrite existing clipboard text
      apropos-do-all t                           ; apropos commands will search more extensively
      mouse-yank-at-point t                      ; middle-clicking pastes at the current location instead of moving it
      window-resize-pixelwise nil                ; if set to 't' this can cause crashes in some cases where we resize windows too quickly
      scroll-conservatively 101                  ; scroll up to this many lines, to bring point back on screen
      mouse-wheel-follow-mouse t                 ; if the frame contains multiple windows, scroll the one under the pointer instead of the one that has keyboard focus
      mouse-wheel-progressive-speed nil          ; if moving the wheel faster don't make the scrolling progressively faster
      mouse-wheel-scroll-amount '(2 ((shift) . 4) ((control) . 6)) ; scroll two lines at once - not 5
      mac-redisplay-dont-reset-vscroll t         ; update doesn’t reset vscroll
      ;; mac-mouse-wheel-smooth-scroll nil       ; enable character scrolling (2 lines at once)
      blink-cursor-blinks -1                     ; by default the cusor stops blinking after 10 blinks. 0 or a negative value means blink forever
      package-native-compile t                   ; natively compile packages for more speed (might give some warnings but can be ignored)
      byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local) ; less warnings when compiling elisp
      column-number-mode t		         ; display culumn number next to the line number
      auto-save-default nil	                 ; disable #autosave# files
      make-backup-files nil	                 ; disable creating backup~ files
      create-lockfiles nil	 	         ; disable creating .#lock files
      load-prefer-newer t		         ; prefer newest elisp files
      custom-file (make-temp-file "")            ; use a temporary file as a placeholder
      custom-safe-themes t                       ; treat all themes as safe
      idle-update-delay 1.0                      ; slow down the ui refresh rate a little bit
      enable-local-variables :all                ; fix 'defvar' warnings
      savehist-mode t			         ; track minibuffer history
      ;;x-select-enable-clipboard t                ; cutting and pasting uses the clipboard
      x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING) ; treat clipboard input as UTF-8 string first, compound text next, etc.
      save-interprogram-paste-before-kill t      ; save existing clipboard text into kill ring before replacing it
      echo-keystrokes 0.1                        ; turn down time to echo keystrokes
      use-dialog-box nil                         ; use the echo area for everything
      ;; mouse-drag-and-drop-region-cross-program t ; added in Emacs 29
      sentence-end-double-space nil              ; sentences end in one space, not two
      use-short-answers t                        ; disable yes/no answers
      ring-bell-function 'ignore                 ; ignore the beep
      compilation-scroll-output t                ; scroll the *compilation* buffer window as output appears
      next-error-message-highlight t             ; highlight the current error message in the 'next-error' buffer
      confirm-kill-processes nil                 ; kill running processes on exit without warning
      truncate-string-ellipsis "…"               ; use the unicode ellipsis for truncations
      ;; Info about auto completion in Emacs 29:
      ;; https://robbmann.io/posts/emacs-29-completions.
a      ;; completion-auto-help t                     ; added in Emacs 29 {nil, t, 'always, 'visible}
      ;; completion-auto-select 'second-tab         ; added in Emacs 29 {nil, t, 'second-tab}
      ;; (define-key minibuffer-local-map (kbd "C-p") #'minibuffer-previous-completion)          ; up when completing in the minibuffer
      ;; (define-key minibuffer-local-map (kbd "C-n") #'minibuffer-next-completion)              ; down when completing in the minibuffer`'
      ;; (define-key completion-in-region-mode-map (kbd "C-p") #'minibuffer-previous-completion) ; up when competing in a normal buffer
      ;; (define-key completion-in-region-mode-map (kbd "C-n") #'minibuffer-next-completion)     ; down when competing in a normal buffer
      gnus-inhibit-startup-message t
      auth-sources '("~/.authinfo" "~/.authinfo.gpg" macos-keychain-internet macos-keychain-generic)) ; tell auth-source where credentials can be found

;; Adjust global settings (functions).
(setenv "LANG" "en_US.UTF-8")                    ; make sure pbcopy/pbpaste (see below) uses utf8
(electric-pair-mode t)                           ; autopair parens, brackets and quotes
(delete-selection-mode t)                        ; overwrite selected text
(global-auto-revert-mode t)                      ; update buffers when underlying files are changed externally
(save-place-mode)		                 ; when visiting a file, point goessavehist-mode to the last place where it was before
(tooltip-mode -1)                                ; show tooltips in the minibuffer

(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them."
  (mapc #'disable-theme custom-enabled-themes))

;; By default Emacs uses a monospaced (fixed-pitch) font designed for writing
;; code. In a fixed-pitch font all the characters have the same with, whether
;; an i or an m, just like an old mechanical typewriter. This helps to align
;; the lines of code/text. On macOS use 'SF Mono' when available:
;; https://osxdaily.com/2018/01/07/use-sf-mono-font-mac/
(when (and (my/macos-p) (find-font (font-spec :name "SF Mono")))
  (set-face-attribute 'default nil :font "SF Mono" :height 160 :weight 'light)
  (setq-default line-spacing 0.3))	         ; hide new messages in the bufferlist instead of popping up

;; Hooks.
(add-hook 'before-save-hook 'delete-trailing-whitespace) ; delete trailing whitespace on save
;;(add-hook 'prog-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;; Faces.
(set-face-foreground 'fill-column-indicator "#9d0006")

;; --------------------------------------
;; Adjust default copy/yank/kill behavior
;; --------------------------------------
;; On Linux, add the following lines to your .zshrc/.bashrc for a macOS
;; pbcopy/pbpaste like experience:
;; alias pbcopy='xsel --clipboard --input'
;; alias pbpaste='xsel --clipboard --output'
(defun my/keep-mark-active-after-copy-region-as-kill (&rest args)
  "Keep mark active after copy-region-as-kill. To be used as advice AFTER any
function that sets `deactivate-mark' to t."
  (setq deactivate-mark nil))

(advice-add 'copy-region-as-kill :after #'my/keep-mark-active-after-copy-region-as-kill)

(defun my/pasteboard-copy ()
  "Save the region in the kill ring and the system pasteboard as if killed, but don't kill it."
  (interactive)
  (copy-region-as-kill (point) (mark)) ; unconditionally deactivates the mark
  (call-process-region (point) (mark) "pbcopy")
  (message "copy"))

(defun my/pasteboard-yank ()
  "Reinsert (\"paste\") the last stretch of killed text from the system pasteboard."
  (interactive)
  (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t)
  (message "yank"))

(defun my/pasteboard-kill ()
  "Kill (\"cut\") text between point and mark, save it in the kill ring and in the system pasteboard."
  (interactive)
  (my/pasteboard-copy)
  (delete-region (region-beginning) (region-end))
  (message "kill"))

;; Overwrite default copy/yank/kill keybindings.
(global-set-key (kbd "M-w") 'my/pasteboard-copy)
(global-set-key (kbd "C-y") 'my/pasteboard-yank)
(global-set-key (kbd "C-w") 'my/pasteboard-kill)
