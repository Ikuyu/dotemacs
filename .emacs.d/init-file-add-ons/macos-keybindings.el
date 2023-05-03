;;; =================
;;; macOS keybindings
;;; =================



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



;; ---------
;; Shortcuts
;; ---------
;; ⌘C, ⌘V, ⌘X             — Copy/paste/cut to and from the system clipboard)
;; ⌘A                     — Select All
;; ⌘Z                     — Undo
;; ⌘⇧Z                    — Redo
;; ⌘F                     — Search for a string
;; ⌘G                     — Search forward for a string
;; ⌘⇧G                    — Search backward for a string
;; ⌘O                     — Open an existing file into an Emacs buffer
;; ⌘S                     — Save current buffer into its file
;; ⌘⇧S                    — Write current buffer into another file
;; ⌘P                     — Print current buffer
;; ⌘M                     — Minimize the window
;; ⌘Q                     — Quit
;; ⌘.                     — Interrupt operation
;; ⌘,                     — Open settings/preferences
;; ⌘L                     — Go to Line
;; ⌘K                     — Kill current buffer
;; ⌘↑                     — Move point to the beginning of the buffer
;; ⌘↓                     — Move point to the end of the buffer
;; ⌘←                     — Move point to beginning of current line
;; ⌘→                     — Move point to end of current line
;; ⌥↑                     — Move point to previous paragraph
;; ⌥↓                     — Move point to next paragraph
;; ⌘N                     — Create a new file
;; ⌘⇧N                    — Make a new window (frame)
;; ⌘=                     — Scale up text
;; ⌘-                     — Scale down text
;; ⌘0                     — Reset scale
;; ⌘⇧I                    — Display current file/directory in a new Finder window
;; ⌘]                     — Go to the next buffer
;; ⌘[                     — Go to the previous buffer
;; ⌘⌫                     — Delete text from the current position to the beginning of the line.
;; Fn⌫                    — Delete Text to the right of the cursor.
;; M⌫D                    — Delete entire word to the left
;; Fn f or ^⌘F            — Toggle fullscreen-mode
;; ⌥⌘F                    — Advanced search (replace text)
;; ⇧ + left mouse button  — Select/extend region
;; ⌥ + left mouse button  — Rectangular selection
;; ⌘⇧ + left mouse button — Is bound to mc/add-cursor-on-click in package multiple-cursors
;; Right mouse button     — Show context menu
;; ⌘T                     — Open tab
;; ⌘}                     — Go to the next tab
;; ⌘{                     — Go to the prevous tab
;; ⌘W                     — Close the active tab
;; ⌘⇧W                    — Close all tabs except for one
;; ⌘⇧T                    — Reopen last closed tab
;; ⌘/                     — (un)Comment line/selection
;; ⌘⇧;                    — spelling/grammar (only works if a spelling checker program is installed)
;; ⌘1                     — Delete other windows
;; ⌘2                     — Split window below
;; ⌘3                     — Split window right
;; ⌘4                     — Delete window
;; fn↑                    — Page up
;; fn↓                    — Page down
;; fn←                    — Home
;; fn→                    — End



;; ----------------------------------------
;; Keybindings based on the shortcuts above
;; ----------------------------------------
(global-set-key (kbd "s-c") (kbd "M-w"))
(global-set-key (kbd "s-v") (kbd "C-y"))
(define-key isearch-mode-map (kbd "s-v") 'isearch-yank-kill)
(global-set-key (kbd "s-x") (kbd "C-w"))
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-Z") 'undo-redo)
(global-set-key (kbd "s-f") 'isearch-forward)
(global-set-key (kbd "s-g") 'isearch-repeat-forward)
(global-set-key (kbd "s-G") 'isearch-repeat-backward)
(global-set-key (kbd "s-o") 'my/mac-open-file)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-S") 'my/mac-save-file-as) ; or use: 'write-file
(global-set-key (kbd "s-p") 'mac-print-frames-dialog)
(global-set-key (kbd "s-m") 'iconify-frame)
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "s-.") 'keyboard-escape-quit) ; 'keyboard-quit
(global-set-key (kbd "s-,") (lambda ()
                              (interactive)
                              (customize-group 'mac)))
(global-set-key (kbd "s-l") 'goto-line)
(global-set-key (kbd "s-k") (lambda ()
                              (interactive) (kill-buffer (current-buffer))))
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)
(global-set-key (kbd "s-<left>") 'beginning-of-visual-line)
(global-set-key (kbd "s-<right>") 'end-of-visual-line)
(global-set-key [(meta down)] 'forward-paragraph)
(global-set-key [(meta up)] 'backward-paragraph)
(global-set-key (kbd "s-n") 'find-file)
(global-set-key (kbd "s-N") 'make-frame)
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-0") (kbd "C-x C-0"))
(global-set-key (kbd "s-I") (lambda ()
			      (interactive)
			      (shell-command "open .")))
(global-set-key (kbd "s-]") 'next-buffer)
(global-set-key (kbd "s-[") 'previous-buffer)
(global-set-key (kbd "s-<backspace>") (lambda ()
					(interactive)
                                        (set-mark (point))
                                        (beginning-of-line)
                                        (backward-delete-char-untabify 1)))
(global-set-key (kbd "H-<backspace>") 'delete-char)
(global-set-key (kbd "M-<backspace>") 'backward-kill-word)
(global-set-key (kbd "C-s-f") 'my/toggle-fullscreen)
(global-set-key (kbd "H-f") 'my/toggle-fullscreen)
(global-set-key (kbd "M-s-f") 'query-replace)
(global-set-key (kbd "S-<mouse-1>") #'mouse-set-mark)
(global-set-key (kbd "S-<down-mouse-1>") #'mouse-set-mark)
(global-set-key (kbd "M-<down-mouse-1>") #'mouse-drag-region-rectangle)
(global-set-key (kbd "M-<drag-mouse-1>") #'ignore)
(global-set-key (kbd "S-s-<mouse-1>") (lambda ()
				        (interactive)
				        (when (not (package-installed-p 'multiple-cursors))
					  (message "Error enabeling Multiple Cursors:\n(Searching for program No such file or directory multiple-cursors)"))))
(global-set-key (kbd "<mouse-3>") `(menu-item ,(purecopy "Menu Bar") ignore
                                              :filter (lambda (_)
                                                        (if (zerop (or (frame-parameter nil 'menu-bar-lines) 0))
                                                            (mouse-menu-bar-map)
                                                          (mouse-menu-major-mode-map)))))
(global-set-key (kbd "s-t") 'tab-bar-new-tab)
(global-set-key (kbd "s-}") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "s-{") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "s-w") 'tab-bar-close-tab)
(global-set-key (kbd "s-W") 'tab-bar-close-other-tabs)
(global-set-key (kbd "s-T") 'tab-bar-undo-close-tab)
(global-set-key (kbd "s-/") 'comment-line)
(global-set-key (kbd "s-:") 'flyspell-mode) ; only works if a spelling checker program is installed
(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-2") 'split-window-below)
(global-set-key (kbd "s-3") 'split-window-right)
(global-set-key (kbd "s-4") 'delete-window)
(global-set-key (kbd "H-<down>") 'scroll-up-command)
(global-set-key (kbd "H-<up>") 'scroll-down-command)
(global-set-key (kbd "H-<left>") 'beginning-of-buffer)
(global-set-key (kbd "H-<right>") 'end-of-buffer)



;; ---------------------------------
;; macOS open & save file interfaces
;; ---------------------------------
(defun my/mac-open-file ()
  "Open a panel that prompts the user to select a file to open."
  (interactive)
  (let ((file (do-applescript "POSIX path of (choose file)")))
    (if (> (length file) 3)
        (setq file (substring file 1 (- (length file) 1))))
    (if (and (not (equal file "")) (file-readable-p file))
        (find-file file))))

(defun my/mac-save-file-as ()
  "Open a panel that prompts the user for information about where to save a file."
  (interactive)
  (let ((file (do-applescript "POSIX path of (choose file name with prompt \"Save As...\")")))
    (if (> (length file) 3)
        (setq file (substring file 1 (- (length file) 1))))
    (if (not (equal file ""))
        (write-file file))))



;; ---------------------
;; macOS fullscreen mode
;; ---------------------
(defun my/toggle-fullscreen ()
  "Same as clicking the corresponding titlebar icon in the right hand corner of macOS app windows"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (pcase (frame-parameter nil 'fullscreen)
     (`fullboth nil)
     (`fullscreen nil)
     (_ 'fullscreen))))
