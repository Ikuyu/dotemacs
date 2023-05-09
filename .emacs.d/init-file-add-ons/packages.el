;;; ========
;;; Packages
;;; ========


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



;; -----------------------------
;; Package system initialization
;; -----------------------------
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/") ; add package repositories
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))



;; -------------------------------------
;; Install package manager "use-package"
;; -------------------------------------
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t        ; saves the trouble of having to specify :ensure t everywhere
      load-prefer-newer t                ; always load newest byte code
      ;;warning-minimum-level :emergency
      use-package-expand-minimally t)    ; make the byte-compiled file as minimal as possible



;; ------------------------------------------
;; Use Quelpa to install packages from source
;; ------------------------------------------
(use-package quelpa		      ; obsolete in: emacs 29. Use "package-vc-install" instead
  ;;:custom (quelpa-upgrade-p t "Always try to update packages") ; see: 'auto-package-update'
  )

(use-package quelpa-use-package       ; example: (package-vc-install '(combobulate :url "https://github.com/mickeynp/combobulate"))
  :init
  (setq quelpa-self-upgrade-p nil
        quelpa-update-melpa-p nil))   ; "auto-package-update" is used for updating (see elsewhere in this file)



;; ---------
;; Set theme
;; ---------
;; monokai-pro-theme:
;;   {'monokai-pro,'monokai-pro-classic,'monokai-pro-machine, 'monokai-pro-octagon,'monokai-pro-ristrettto,'emonokai-pro-spectrum}.
;; alect-themes:
;;   {'alect-black,'alect-black-alt,'alect-dark,'alect-dark-alt,'alect-light,'alect-light-alt}
;; gruvbox-theme:
;;   {'gruvbox-dark-medium,'gruvbox-dark-soft,'gruvbox-dark-hard, 'gruvbox-light-medium,'gruvbox-light-soft,'gruvbox-light-hard}
;; nofrils-acme-theme:
;;   {'nofrils-acme, 'nofrils-dark, 'nofrils-light, 'nofrils-sepia}.
;; autumn-light-theme:
;;   {'autumn-light}.
;; solarized-theme:
;;   {'solarized-light,'solarized-dark}.
;; standard-themes:
;;   {'standard-light,'standard-dark}.
;; modus-themes (updated):
;;   {'modus-operandi,'modus-vivendi,'modus-operandi-tinted, 'modus-vivendi-tinted,'modus-operandi-deuteranopia,'modus-vivendi-deuteranopia}.

;; The standard-, ef- and the modus-themes are the work of Protesilaos Stavrou.
;; Prot is one of the offical Emacs built-in themes creators/maintainers).

;; The standard-themes are a pair of light and dark themes for GNU Emacs. They
;; emulate the out-of-the-box looks of Emacs (which technically do NOT
;; constitute a theme) while bringing to them thematic consistency,
;; customizability, and extensibility. In practice, the Standard themes take
;; the default style of the font-lock and Org faces, complement it with a wider
;; colour palette, address some inconsistencies, and apply established semantic
;; patterns across all interfaces.

;; The modus-themes are (updated) variations of the modus-operandi and
;; modus-vivendi themes (included in Emacs since August 2020) that will be
;; merged with Emacs 30.

;; Some great build-in themes: {'wheatgrass,'light-blue}.

;; (use-package modus-themes
;;   :init
;;   (setq modus-themes-bold-constructs t
;; 	   modus-themes-mixed-fonts nil
;;  	   modus-themes-variable-pitch-ui nil
;;         modus-themes-completions '((matches . (extrabold intense background))
;;                                    (selection . (semibold accented intense))
;;                                    (popup . (accented)))
;;         modus-themes-diffs 'desaturated
;; 	   modus-themes-italic-constructs nil
;;         modus-themes-hl-line '(nil)
;;         modus-themes-links '(nil)
;;         modus-themes-mixed-fonts nil
;;         modus-themes-tabs-accented t
;;         modus-themes-prompts '(background)
;;         modus-themes-region '(accented bg-only)
;;         modus-themes-syntax '(faint)
;;         modus-themes-tabs-accented nil
;; 	   modus-themes-org-blocks 'gray-background) ; {nil,'gray-background,'tinted-background}
;;         modus-themes-org-agenda '((header-date . (grayscale workaholic bold-today))
;;                                   (header-block . (1.5 semibold))
;;                                   (scheduled . uniform)
;;                                   (event . (italic))
;;                                   (habit . traffic-light)))
;;   (modus-themes-load-themes)
;;   :config
;;   (modus-themes-load-operandi)
;;   :bind ("<f5>" . modus-themes-toggle))

;; (use-package standard-themes
;;   :init
;;   (setq standard-themes-bold-constructs nil
;;         standard-themes-italic-constructs nil
;;         standard-themes-fringes 'subtle        ; {nil,'subtle,'intense}
;;         ;;standard-themes-links '(neutral-underline faint) ; {nil,neutral-underline,no-underline,faint,bold,italic}
;;         ;;standard-themes-prompts '(bold italic)           ; {nil,background,bold,italic}
;;         )
;;   (mapc #'disable-theme custom-enabled-themes) ; disable all other themes to avoid awkward blending
;;   (load-theme 'standard-dark t))

;; (use-package solarized-emacs
;;   :quelpa (solarized-emacs
;;            :fetcher github
;;            :repo "bbatsov/solarized-emacs")
;;   :init
;;   (setq solarized-distinct-fringe-background t ; make the fringe stand out from the background
;;         solarized-use-variable-pitch nil       ; don't change the font for some headings and titles
;;         solarized-high-contrast-mode-line t    ; make the modeline high contrast
;;         solarized-use-less-bold t              ; use less bolding
;;         solarized-use-more-italic t            ; use more italics
;;         solarized-emphasize-indicators nil     ; use less colors for indicators such as git:gutter, flycheck and similar
;;         solarized-scale-org-headlines nil      ; don't change size of org-mode headlines (but keep other size-changes)
;;         solarized-height-minus-1 1.0           ; all solarized-height variables: avoid all font-size changes
;;         solarized-height-plus-1 1.0
;;         solarized-height-plus-2 1.0
;;         solarized-height-plus-3 1.0
;;         solarized-height-plus-4 1.0)
;;   (load-theme 'solarized-dark t))

(use-package gruvbox-theme
  :init
  (setq gruvbox-bold-constructs t)
  (load-theme 'gruvbox-light-medium t)
  :custom-face
  (avy-lead-face ((nil (:foreground "#ebdbb2"))))
  (avy-lead-face-0 ((nil (:foreground "#ebdbb2"))))
  (avy-lead-face-1 ((nil (:foreground "#ebdbb2"))))
  (avy-lead-face-2 ((nil (:foreground "#ebdbb2"))))
  (isearch ((nil (:foreground "#ebdbb2"))))
  (isearch-fail ((nil (:foreground "#ebdbb2"))))
  ;;(fill-column-indicator "#9d0006")
  (powerline-inactive2 ((nil (:background "#f2e5bc")))))



;; -----------------
;; Toggle appearance
;; -----------------
(use-package heaven-and-hell
  :after gruvbox-theme
  :config
  (setq heaven-and-hell-theme-type 'light         ; default appearance
        heaven-and-hell-themes '((light . gruvbox-light-medium)
                                 (dark . gruvbox-dark-medium))
        heaven-and-hell-load-theme-no-confirm t)
  :hook (after-init . heaven-and-hell-init-hook))



;; -----------------
;; Prettify modeline
;; -----------------
(use-package powerline
  :config
  (setq powerline-default-separator 'wave ; {'alternate,'arrow,'arrow-fade,'bar,'box,'brace,'butt,'chamfer,'contour,'curve,'rounded,'roundstub,'slant,'wave,'zigzag,nil}
        powerline-display-buffer-size nil))



;; ------------------------------------------------------------------------
;; Enforce sneaky garbage collection to minimize interference with the user
;; activity
;; ------------------------------------------------------------------------
(use-package gcmh
  :diminish gcmh-mode
  :config
  (setq gcmh-idle-delay 5
	gcmh-high-cons-threshold (* 16 1024 1024)) ; 16mb
  (gcmh-mode 1)
  :hook (emacs-startup . (lambda ()
                           (setq gc-cons-percentage 0.1))))



;; -------------------------------------------------------------------------
;; Get environment variables inside Emacs (probaly not nescessary when using
;; the Emacs Railwaycat's port)
;; -------------------------------------------------------------------------
(use-package exec-path-from-shell
  :if (my/macos-p)
  :hook (emacs-startup . (lambda ()
                           (setq exec-path-from-shell-arguments '("-l")) ; removed the -i for faster startup
                           (exec-path-from-shell-initialize))))



;; ----------------------------------
;; Keep installed packages up-to-date
;; ----------------------------------
(use-package auto-package-update               ; Emacs 29: use the command "package-update-all"
  :custom
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)  ; delete residual older versions
  (auto-package-update-hide-results t))        ; do not bother me when updates have taken place



;; --------------------------------------------------------------------------
;; Disable the arrow, end, home and delete keys, as well as their control and
;; meta prefixes
;; --------------------------------------------------------------------------
(use-package guru-mode	                       ; force using the proper Emacs movement keys
  :if my/modus-tollens
  :config
  ;;(setq guru-warn-only t)                    ; warnings only when using the arrow keys
  (guru-global-mode t))



;; -----------------
;; Disable the mouse
;; -----------------
(use-package disable-mouse      	       ; force using the proper Emacs movement keys
  :if my/modus-tollens
  :quelpa (disable-mouse
	   :fetcher github
	   :repo "purcell/disable-mouse")
  :config (global-disable-mouse-mode t))



;; ------------------------------------
;; Center document and increase margins
;; ------------------------------------
(use-package olivetti
  :custom
  (olivetti-body-width 70)                     ; ideal for reading text
  :config
  (setq ;;olivetti-body-width 0.65
        ;;olivetti-recall-visual-line-mode-entry-state t
        olivetti-style nil))



;; ---------------------------
;; Try new packages in advance
;; ---------------------------
(use-package try)



;; ---------------------------------------------
;; Highlight delimiters according to their depth
;; ---------------------------------------------
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))



;; ------------------------------------------------------------------------
;; Highlight keywords in comments: TODO FIXME NEXT THEM DONE FAIL NOTE OKAY
;; PROG DONT TEMP KLUDGE THEM HACK XXXX
;; ------------------------------------------------------------------------
(use-package hl-todo
  :init (global-hl-todo-mode t))



;; --------------------------------
;; Read NOS Teletext pages in Emacs
;; --------------------------------
(use-package teletext-nos                      ; start NOS Teletekst automatically with "s-u t" (see: user-keymap.el)
  :quelpa (teletext-nos
           :fetcher github
           :repo "Ikuyu/emacs-teletext-nos")
  :init (require 'teletext-nos)
  :bind (:map teletext-mode-map
              ("<wheel-up>" . nil)
              ("<wheel-down>" . nil)))


;; ----------------------------
;; Fast jumping in visible text
;; ----------------------------
(use-package avy ; an alternative is included in package "meow" (see below)
  :config (avy-setup-default)
  :bind (("s-j" . avy-goto-char-timer)))       ; jump back in sequence with C-x C-SPC



;; ----------------------------
;; Fast jumping between buffers
;; ----------------------------
(use-package frog-jump-buffer
  :config
  (dolist (regexp '("TAGS" "^\\*Compile-log" "-debug\\*$" "^\\:" "errors\\*$" "^\\*Backtrace" "-ls\\*$"
                    "stderr\\*$" "^\\*Flymake" "^\\*vc" "^\\*Warnings" "checkout\\*$" "\\^*Shell Command"))
    (push regexp frog-jump-buffer-ignore-buffers))
  :bind (("s-b" . frog-jump-buffer )))

(use-package frog-menu
  :custom-face (frog-menu-posframe-background-face ((nil (:background "#f4e8ba")))))



;; ------------------------------------------
;; Increase selected region by semantic units
;; ------------------------------------------
(use-package expand-region                     ; package "meow" (see below) als it's own way of expanding a region
  :bind ("C-=" . er/expand-region))            ; press "C-=" once to enable this mode



;; ----------------------------------------------
;; Add/change/delete pairs based on expand-region
;; ----------------------------------------------
(use-package embrace
  :config
  (setq embrace-show-help-p t)      ; show/hide the help message
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  :bind ("C-," . embrace-commander))



;; ------------------------------------------------------------------------
;; Open a terminal in the same folder as where the working document resides
;; ------------------------------------------------------------------------
(use-package terminal-here)



;; ----------------------------------------
;; Extend build-in directory editor support
;; ----------------------------------------
(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-lha --group-directories-first") ; human-readable listing
  (dired-recursive-copies 'always)                 ; "always" means no asking
  (dired-recursive-deletes 'top)                   ; "top" means ask once for top level directory
  :bind
  (:map dired-mode-map
        ("C-c C-a" . gnus-dired-attach))
  :config
  (when (my/macos-p)
    (setq dired-use-ls-dired t
          insert-directory-program "gls"           ; make sure "gls" is installed with 'brew install coreutils'
          trash-directory "~/.Trash"))
  (setq dired-dwim-target t                        ; if another Dired buffer is visible in another window, use that directory as target for rename/copy
        dired-kill-when-opening-new-dired-buffer t ; use "a" to replace the current Dired buffer with the file/directory the cursor is on
        ;; dired-make-directory-clickable t        ; Emacs 29.1
        ;; dired-mouse-drag-files t                ; Emacs 29.1
        delete-by-moving-to-trash t)               ; move deleted files to trash
  :hook
  (dired-mode . dired-hide-details-mode)
  (dired-mode-hook . auto-revert-mode))            ; automatically refresh dired buffer on changes



;; --------------------------------------------------------
;; Extend build-in directory editor support with Dired-plus
;; --------------------------------------------------------
(use-package dired+
  :quelpa (dired+
           :fetcher github
           :repo "emacsmirror/dired-plus")
  :custom (diredp-make-find-file-keys-reuse-dirs)
  :bind
  (:map dired-mode-map
        ("<mouse-2>" . dired-find-file))
  (:map image-map
        ("<return>" . image-kill-buffer)
        ("<mouse-1>" . image-kill-buffer)))



;; -------------------------------------------
;; Disallow *scratch* buffer from being killed
;; -------------------------------------------
(use-package unkillable-scratch     ; Emacs 29: use the command "scratch-buffer" to switch to this buffer or to create one if it has been deleted
  :config (unkillable-scratch t))



;;-----------------------------------------------------------
;; Move lines/selection up/down, move words/region left/right
;; ----------------------------------------------------------
(use-package drag-stuff			; press M-s-<key> on a word, sentence or region
  :init (when (not my/modus-tollens)
	  (bind-key "M-s-<up>" 'drag-stuff-up)
	  (bind-key "M-s-<down>" 'drag-stuff-down)
	  (bind-key "M-s-<left>" 'drag-stuff-left)
	  (bind-key "M-s-<right>" 'drag-stuff-right))
  (drag-stuff-global-mode 1)
  :bind
  (("M-s-p" . drag-stuff-up)
   ("M-s-n" . drag-stuff-down)
   ("M-s-b" . drag-stuff-left)
   ("M-s-f" . drag-stuff-right)))



;; -------------------------------------------------
;; Markdown support (requires "brew install pandoc")
;; -------------------------------------------------
(use-package markdown-mode
  :init (setq markdown-command '("pandoc" "--from=markdown" "--to=html5"))
  :config (setq markdown-fontify-code-blocks-natively t)
  :mode
  (("README\\.md\\'" . markdown-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :hook ((markdown-mode-hook)))

(use-package markdown-preview-mode
  :after markdown-mode
  :init (setq markdown-preview-stylesheets nil)
  :bind (:map markdown-mode-map ("C-c C-c p" . markdown-preview-mode)))



;; -----------
;; PDF support
;; -----------
(use-package pdf-tools
  :config
  (pdf-tools-install :no-query)
  (custom-set-variables
    '(pdf-tools-handle-upgrades t)))



;; ------------
;; EPUB support
;; ------------
(use-package esxml)

(use-package nov		    ; currently the only viable option
  :after esxml
  :mode ("\\.epub\\'" . nov-mode)
  :preface
  (defun init-nov-delayed-render ()
    (run-with-idle-timer 0.2 nil 'nov-render-document))
  :custom (nov-text-width t)
  :config (progn
            (defun my-nov-font-setup ()
              (face-remap-add-relative 'variable-pitch
                                       :family "times new roman"
                                       :height 1n.3))
            (add-hook 'nov-mode-hook 'my-nov-font-setup))
  :hook
  (nov-mode-hook . olivetti-mode)
  ;;(nov-mode-hook . no-fringes)
  (nov-mode-hook . init-nov-delayed-render))



;; ------------------
;; Support for Scheme
  ;; ------------------
(use-package geiser
  :disabled
  :init
  (setenv "LANG" "en_US.UTF-8")     ; fix "warning failed to install locale" (en_NL is not a locale)
  (setq geiser-default-implementation 'guile
	geiser-mode-start-repl-p t
	geiser-active-implementations '(guile))
  :commands (geiser run-geiser))

(use-package geiser-guile
  :disabled
  :after geiser)



;; -------------------------
;; Autocompletion for Geiser
;; -------------------------
(use-package ac-geiser
  :disabled
  :config (eval-after-load 'auto-complete
	    '(add-to-list 'ac-modes 'geiser-repl-mode))
  :hook
  (geiser-mode  . ac-geiser-setup)
  (geiser-repl-mode . ac-geiser-setup))



;; ------------------
;; Support for Racket
;; ------------------
(use-package racket-mode
  :disabled
  :hook ((racket-mode . racket-xp-mode)
         (racket-mode . racket-unicode-input-method-enable)
         (racket-repl-mode . racket-unicode-input-method-enable)))



;; ---------------------------------------------------------------
;; Support for Common Lisp (requires "touch ~/.sly-mrepl-history")
;; ---------------------------------------------------------------
(use-package sly
  :config
  (setq sly-net-coding-system 'utf-8-unix
        ;;sly-complete-symbol-function 'sly-simple-completions    ; or: 'sly-flex-completions
        ;;sly-symbol-completion-mode nil
        ;;sly-kill-without-query-p t
        sly-mrepl-pop-sylvester nil
        sly-mrepl-history-file-name (expand-file-name "~/.sly-mrepl-history")
        sly-contribs '(sly-fancy)
        inferior-lisp-program "sbcl --noinform"
        ;;sly-common-lisp-style-default 'sbcl
        ;;inferior-lisp-program "clisp -q -ansi -modern -I -on-error abort" ; not working
        ;; sly-lisp-implementations '((sbcl ("sbcl" "--noinform" "--disable-debugger"))
        ;;                            (clisp ("clisp" "-q" "-ansi" "-modern" "-I" "-on-error" "abort"))
        ;;                            (clozure-cl ("/Applications/CCL.app/Contents/Resources/darwinx86/dx86cl64")))
))



;; ----------------------------------------
;; Add ANSI colors support to the sly mrepl
;; ----------------------------------------
(use-package sly-repl-ansi-color
  :disabled
  :after sly
  :init (add-to-list 'sly-contribs 'sly-repl-ansi-color nil #'eq))



;; ---------------------
;; Fancy macro-expansion
;; ---------------------
(use-package sly-macrostep
  :disabled
  :after sly
  :config (with-eval-after-load 'sly
            (sly-enable-contrib 'sly-macrostep)))



;; ----------------------
;; Autocompletion for Sly
;; ----------------------
(use-package ac-sly
  :config (eval-after-load 'auto-complete
	    '(add-to-list 'ac-modes 'sly-mrepl-mode))
  :hook (sly-mode . set-up-sly-ac))



;; -----------------------------
;; Extend build-in eLisp support
;; -----------------------------
(use-package emacs-lisp-mode
  :ensure nil
  :bind (:map emacs-lisp-mode-map
              ("C-c C-b" . eval-buffer)
              ("C-c C-c" . emacs-lisp-byte-compile)
              ("C-c C-d" . eval-defun)
              ("C-c C-l" . emacs-lisp-byte-compile-and-load)
              ("C-c C-r" . eval-region)
              ("C-c C-t" . ert)))   ; run test



;; ----------------------------------
;; Behavior-driven Emacs lisp testing
;; ----------------------------------
(use-package buttercup)



;; --------------------------------
;; Test support functions for Emacs
;; --------------------------------
(use-package assess)



;; ----------------------------
;; Extend build-in IELM support
;; ----------------------------
(use-package ielm
  :ensure nil
  :config
  (defun ielm-read-history ()
    "Restore `ielm' history."
    (let ((path (expand-file-name "ielm/history" user-emacs-directory)))
      (make-directory (file-name-directory path) t)
      (setq-local comint-input-ring-file-name path)
      (setq-local comint-input-ring-size 10000)
      (setq-local comint-input-ignoredups t)
      (when (file-exists-p path)
        (comint-read-input-ring))))
  (defun ielm-write-history (&rest _args)
    "Save `ielm' history."
    (with-file-modes #o600
      (comint-write-input-ring)))
  (advice-add 'ielm-send-input :after 'ielm-write-history)
  (add-hook 'ielm-mode-hook 'ielm-read-history)
  :bind (:map comint-mode-map
              ("C-c C-l" . comint-clear-buffer))
  :hook (ielm-mode-hook . eldoc-mode)) ; enable Eldoc in IELM



;; -------------------------
;; Extend build-in C support
;; -------------------------
(use-package c-mode
  :ensure nil
  :config
  (setq c-block-comment-prefix "* "
        c-default-style "gnu"
        c-basic-offset 4)
  :bind (:map c-mode-map
              ("C-c c"   . compile)
              ("C-c C-c" . recompile)
              ("C-c g"   . gdb)
              ("C-c C-r" . gdb-run)))



; -----------------------------------------
;; Edit multiple occurrences simultaneously
;; ----------------------------------------
;; https://www.youtube.com/watch?v=3O-bDYqhFos
(use-package multiple-cursors       ; alternatives: "iedit", "macrursors" or "meow" (see below)
  :disabled
  :if (not my/modus-tollens)
  :init (setq mc/always-run-for-all t)
  :bind ("S-s-<mouse-1>" . mc/add-cursor-on-click))



;; -----------
;; Git support
;; -----------
(use-package magit)



;; ---------------------------
;; Keep git repositories clean
;; ---------------------------
(use-package gitignore-templates)



;; -----------------
;; Prettify org-mode
;; -----------------
(use-package org-modern                 ; style headlines, keywords, tables and source blocks in org-mode
  :config
  (setq org-modern-star nil             ; {nil = hide the default org-modern symbols,
	                                ; '("◉" "○" "◈" "◇" "✳"),
					; '("▏" "▎" "▍" "▌" "▋" "▊" "▉" "█"),
					; '("█" "▉" "▊" "▋" "▌" "▍" "▎" "▏")}
        org-modern-hide-stars t         ; {t = hide the default org-mode stars,
                                        ; nil = show the default org-mode stars}
	org-modern-list '((45 . "-")
			  (43 . "•")
			  (42 . "*"))
	org-modern-table-vertical 1	; Prettify tables
	org-modern-todo-faces '(("TODO"  :inverse-video t :inherit org-todo
				 ("PROJ" :inverse-video t :inherit +org-todo-project)
				 ("STRT" :inverse-video t :inherit +org-todo-active)
				 ("[-]"  :inverse-video t :inherit +org-todo-active)
				 ("HOLD" :inverse-video t :inherit +org-todo-onhold)
				 ("WAIT" :inverse-video t :inherit +org-todo-onhold)
				 ("[?]"  :inverse-video t :inherit +org-todo-onhold)
				 ("KILL" :inverse-video t :inherit +org-todo-cancel)
				 ("NO"   :inverse-video t :inherit +org-todo-cancel)))
	org-modern-footnote (cons nil (cadr org-script-display))
	org-modern-block-fringe 4)
  :hook
  (org-mode . org-num-mode)		; use numeric headlines
  (org-mode . org-modern-mode))



;; ---------------------------------------------------
;; Make invisible parts of Org elements appear visible
;; ---------------------------------------------------
(use-package org-appear
  :after org
  :custom
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  (org-appear-autokeywords t)
  (org-appear-inside-latex t)
  (org-appear-delay 0)
  (org-appear-trigger #'always)
  :hook (org-mode . org-appear-mode))



;; -------------------------------------
;; Spaced repetition system for org-mode
;; -------------------------------------
(use-package org-drill)



;; -----------------------------------------------------------
;; Slipbox support for org-mode (requires 'mkdir "~/.slipbox")
;; -----------------------------------------------------------
(use-package org-roam
  :after org
  :init (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/.slipbox")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+date: %U\n#+title: ${title}\n#+filetags: :draft:\n")
      :unnarrowed t)))
  :bind
  (("C-c n a" . org-roam-alias-add)
   ("C-c o c" . org-roam-capture)
   ("C-c n f" . org-roam-node-find)
   ("C-c o g" . org-roam-graph)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n l" . org-roam-buffer-toggle)
   ("C-c n o" . org-id-get-create)
   ("C-c n t" . org-roam-tag-add)
   :map org-mode-map
   ("C-M-i" . completion-at-point))
  :config (org-roam-setup))



;; ---------------------------------------------------------------------------
;; Browse the notes network in an interactive graph in the browser to remember
;; certain relationships between notes (requires 'mkdir "~/.slipbox")
;; ---------------------------------------------------------------------------
(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-browser-function #'browse-url-chromium
        org-roam-ui-open-on-start nil))



;; ------------------------------------------
;; send html email using org-mode html export
;; ------------------------------------------
(use-package org-mime
  :config (setq org-mime-export-options '(:preserve-breaks t))
          (add-hook 'message-mode-hook
          (lambda ()
            (local-set-key "\C-c\M-o" 'org-mime-htmlize)))
          (add-hook 'message-send-hook 'org-mime-confirm-when-no-multipart))



;; ---------------------------------------
;; Use pandoc to convert files to org-mode
;; ---------------------------------------
(use-package org-pandoc-import
  :after org
  :quelpa (org-pandoc-import
           :fetcher github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))



;; ---------------------------------------------------------------------
;; Perform document conversions using the pandoc library (requires "brew
;; install pandoc")
;; ---------------------------------------------------------------------
(use-package pandoc-mode
  :after org)



;; ----------------------
;; Ergonomic command mode
;; ----------------------
(use-package meow
  :after gruvbox-theme
  :if (not my/modus-tollens)
  :init
  (require 'meow)
  (setq meow-replace-state-name-list '((normal . "CMD")   ; command state
                                       (motion . "MOT")   ; motion state
                                       (keypad . "KPD")   ; keypad state
                                       (insert . "INS")   ; insert state
                                       (beacon . "BCN"))) ; beacon state
  (setq-default mode-line-format '((:eval (meow-indicator))
                                   "%e"
                                   (:eval (let*
                                              ((active
                                                (powerline-selected-window-active))
                                               (mode-line-buffer-id
                                                (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                                               (mode-line
                                                (if active 'mode-line 'mode-line-inactive))
                                               (face0
                                                (if active 'powerline-active0 'powerline-inactive0))
                                               (face1
                                                (if active 'powerline-active1 'powerline-inactive1))
                                               (face2
                                                (if active 'powerline-active2 'powerline-inactive2))
                                               (separator-left
                                                (intern
                                                 (format "powerline-%s-%s"
                                                         (powerline-current-separator)
                                                         (car powerline-default-separator-dir))))
                                               (separator-right
                                                (intern
                                                 (format "powerline-%s-%s"
                                                         (powerline-current-separator)
                                                         (cdr powerline-default-separator-dir))))
                                               (lhs
                                                (list
                                                 (when (bound-and-true-p meow-global-mode)
                                                   (let* ((state (meow--current-state))
                                                          (indicator-face (alist-get state meow-indicator-face-alist)))
                                                     (funcall separator-left indicator-face face0)))
                                                 (powerline-raw "%*" face0 'l)
                                                 (when powerline-display-buffer-size
                                                   (powerline-buffer-size face0 'l))
                                                 (when powerline-display-mule-info
                                                   (powerline-raw mode-line-mule-info face0 'l))
                                                 (powerline-buffer-id
                                                  `(mode-line-buffer-id ,face0)
                                                  'l)
                                                 (when
                                                     (and
                                                      (boundp 'which-func-mode)
                                                      which-func-mode)
                                                   (powerline-raw which-func-format face0 'l))
                                                 (powerline-raw " " face0)
                                                 (funcall separator-left face0 face1)
                                                 (when
                                                     (and
                                                      (boundp 'erc-track-minor-mode)
                                                      erc-track-minor-mode)
                                                   (powerline-raw erc-modified-channels-object face1 'l))
                                                 (powerline-major-mode face1 'l)
                                                 (powerline-process face1)
                                                 (powerline-minor-modes face1 'l)
                                                 (powerline-narrow face1 'l)
                                                 (powerline-raw " " face1)
                                                 (funcall separator-left face1 face2)
                                                 (powerline-vc face2 'r)
                                                 (when
                                                     (bound-and-true-p nyan-mode)
                                                   (powerline-raw
                                                    (list
                                                     (nyan-create))
                                                    face2 'l))))
                                               (rhs
                                                (list
                                                 (powerline-raw global-mode-string face2 'r)
                                                 (funcall separator-right face2 face1)
                                                 (unless window-system
                                                   (powerline-raw
                                                    (char-to-string 57505)
                                                    face1 'l))
                                                 (powerline-raw "%4l:" face1 'l)
                                                 (powerline-raw (format-mode-line '(4 "%c")) face1)
                                                 (funcall separator-right face1 face0)
                                                 (powerline-raw " " face0)
                                                 (powerline-raw "%3p" face0 'r)
                                                 (when powerline-display-hud
                                                   (powerline-hud face0 face2))
                                                 (powerline-fill face0 0))))
                                            (concat
                                             (powerline-render lhs)
                                             (powerline-fill face2
                                                             (powerline-width rhs))
                                             (powerline-render rhs))))))
  ;;:custom
  ;;(cursor-position-hack t)
  ;;(meow-goto-line-function 'consult-goto-line)
  :config
  (when (string-match-p "gruvbox.*" (symbol-name (car custom-enabled-themes)))
    (set-face-attribute 'meow-normal-indicator nil :foreground "#ffdfaf" :background "#000087")  ; blue
    (set-face-attribute 'meow-motion-indicator nil :foreground "#ffdfaf" :background "#8f3f71")  ; purple
    (set-face-attribute 'meow-keypad-indicator nil :foreground "#ffdfaf" :background "#af3a03")  ; orange
    (set-face-attribute 'meow-insert-indicator nil :foreground "#ffdfaf" :background "#9d0006")  ; red
    (set-face-attribute 'meow-beacon-indicator nil :foreground "#ffdfaf" :background "#005f5f")) ; aqua
  (setq meow-expand-hint-remove-delay 2)
  (defun my/meow-scroll-up ()
    (interactive)
    (scroll-up-command 1))
  (defun my/meow-scroll-down ()
    (interactive)
    (scroll-down-command 1))
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swaap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("N" . my/meow-scroll-up)
     '("M" . my/meow-scroll-down)
     '("'" . repeat)
     '("<escape>" . ignore)))
  (meow-setup)
  (meow-setup-indicator))



;; -------------
;; Snow in Emacs
;; -------------
(use-package snow
  :custom
  (snow-show-background t)
  (snow-debug nil))


;; -----------------------------------------------------------
;; Interface for sdcv (startdict - console version). Requires:
;; $ brew install sdcv
;; $ mkdir -p $HOME/.stardict/dic
;; Copy dictionaries to $HOME/.stardict/dic
;; -----------------------------------------------------------
(use-package sdcv
  :config
  (setq sdcv-dictionary-data-dir "~/.stardict/dic"))
