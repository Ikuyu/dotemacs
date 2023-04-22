;;; ========
;;; Packages
;;; ========



;; ---------------------
;; Bootstrap use-package
;; ---------------------
(setq package-archives '(("melpa" . "https://melpa.org/packages/") ; package repositories
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize) ; activate all the packages (in particular autoloads)

(unless (package-installed-p 'use-package) obsolete in Emacs 29
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile                         ; obsolete in Emacs 29
  (setq use-package-always-ensure t        ; specify ':ensure nil' for internal packages/libraries
        load-prefer-newer t
        use-package-expand-minimally t     ; make the byte-compiled file as minimal as possible
        warning-minimum-level :emergency)) ; when using Emacs 29 these settings can be moved to to global-setting.el



;; ------------------------------------------
;; Use Quelpa to install packages from source
;; ------------------------------------------
(use-package quelpa)		      ; obsolete in: emacs 29. Use package-vc-install instead

(use-package quelpa-use-package)      ; example: (package-vc-install '(combobulate :url "https://github.com/mickeynp/combobulate"))



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
;; spaceduck-emacs (copy the file 'spaceduck.el' from 'ctrl-dlahr/spaceduck-emacs' to '~.emacs.d' and use '(load-theme 'spaceduck t)'). Note: this theme doesn't work too well with package 'Powerline' and needs a lot of adjustments
;; solarized-theme:
;;   {'solarized-light,'solarized-dark}.
;; standard-themes:
;;   {'standard-light,'standard-dark}.
;; modus-themes (updated):
;;   {'modus-operandi,'modus-vivendi,'modus-operandi-tinted, 'modus-vivendi-tinted,'modus-operandi-deuteranopia,'modus-vivendi-deuteranopia}.

;; The standard-, ef- and the modus-themes are the work of Protesilaos Stavrou.
;; Prot is one of the offical Emacs built-in themes creators/maintainers).

;; The standard-themes are a pair of light and dark themes for GNU Emacs. They emulate the out-of-the-box looks of Emacs
;; (which technically do NOT constitute a theme) while bringing to them thematic consistency, customizability, and extensibility.
;; In practice, the Standard themes take the default style of the font-lock and Org faces, complement it with a wider colour palette,
;; address some inconsistencies, and apply established semantic patterns across all interfaces.

;; The modus-themes are (updated) variations of the modus-operandi and modus-vivendi themes (included in Emacs since August 2020)
;; that will be merged with Emacs 30.

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
;;         standard-themes-fringes 'subtle                  ; {nil,'subtle,'intense}
;;         ;;standard-themes-links '(neutral-underline faint) ; {nil,neutral-underline,no-underline,faint,bold,italic}
;;         ;;standard-themes-prompts '(bold italic)           ; {nil,background,bold,italic}
;;         )
;;   (mapc #'disable-theme custom-enabled-themes)           ; disable all other themes to avoid awkward blending
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
  :init (load-theme 'gruvbox-dark-medium t)
  :custom-face
  (avy-lead-face ((t (:foreground "#fbf1c7"))))
  (avy-lead-face-0 ((t (:foreground "#fbf1c7"))))
  (avy-lead-face-1 ((t (:foreground "#fbf1c7"))))
  (avy-lead-face-2 ((t (:foreground "#fbf1c7"))))
  (isearch ((t (:foreground "#fbf1c7"))))
  ;;(lazy-highlight ((t (:foreground "#fbf1c7"))))
  (isearch-fail ((t (:foreground "#fbf1c7")))) ; gruvbox-dark0
  (powerline-inactive2 ((t (:background "#32302f"))))) ; gruvbox-light-medium: #f2e5bc gruvbox-dark-medium: #32302f (gruvbox-dark0_soft)



;; -----------------
;; Prettify modeline
;; -----------------
(use-package powerline
  :config
  (setq powerline-default-separator 'wave ; {'alternate,'arrow,'arrow-fade,'bar,'box,'brace,'butt,'chamfer,'contour,'curve,'rounded,'roundstub,'slant,'wave,'zigzag,nil}
        powerline-display-buffer-size nil)
  (powerline-default-theme))



;; ---------------------------------------------------------------------------------
;; Enforce sneaky garbage collection to minimize interference with the user activity
;; ---------------------------------------------------------------------------------
(use-package gcmh
  :diminish gcmh-mode
  :config
  (setq gcmh-idle-delay 5
	gcmh-high-cons-threshold (* 16 1024 1024)) ; 16mb
  (gcmh-mode 1)
  :hook (emacs-startup . (lambda () (setq gc-cons-percentage 0.1))))



;; ------------------------------------------------------------------------------------------------------
;; Get environment variables inside Emacs (probaly not nescessary when using the Emacs Railwaycat's port)
;; ------------------------------------------------------------------------------------------------------
(use-package exec-path-from-shell
  :if (my/macos-p)
  :hook (emacs-startup . (lambda ()
                           (setq exec-path-from-shell-arguments '("-l")) ; removed the -i for faster startup
                           (exec-path-from-shell-initialize))))



;; ----------------------------------
;; Kep installed packages up-to-date
;; ----------------------------------
(use-package auto-package-update        ; Emacs 29: use the command 'package-update-all'
  :custom
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t))



;; ----------------------------------------------------------------------------------------
;; Disable the arrow, end, home and delete keys, as well as their control and meta prefixes
;; ----------------------------------------------------------------------------------------
(use-package no-easy-keys		; force using the proper Emacs movement keys
  :if my/modus-tollens
  :quelpa (no-easy-keys
	   :fetcher github
	   :repo "danamlund/emacs-no-easy-keys")
  :config (no-easy-keys t))



;; -----------------
;; Disable the mouse
;; -----------------
(use-package disable-mouse      	; force using the proper Emacs movement keys
  :if my/modus-tollens
  :quelpa (disable-mouse
	   :fetcher github
	   :repo "purcell/disable-mouse")
  :config (global-disable-mouse-mode t))



;; ----------------------
;; Ergonomic command mode
;; ----------------------
(use-package boon                      ; boon set the default cursor type to 'bar
  :if (not my/modus-tollens)
  :config
  (require 'boon-qwerty)
  (require 'boon-powerline)
  (boon-powerline-theme)
  ;;(boon-mode)                          ; enable/disble with 's-u b' (see user-keymap.el)
  :bind (("C-g" . 'boon-set-command-state) ; or: ("C-g" . [escape])
  ))



;; ------------------------------------
;; Center document and increase margins
;; ------------------------------------
(use-package olivetti
  :defer t
  :config
  (setq ;;olivetti-body-width 0.65
        olivetti-minimum-body-width fill-column
        ;;olivetti-recall-visual-line-mode-entry-state t
        olivetti-style nil))



;; ---------------------------
;; Try new packages in advance
;; ---------------------------
(use-package try
  :defer t)



;; ---------------------------------------------
;; Highlight delimiters according to their depth
;; ---------------------------------------------
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))



;; -------------------------------------------------------------------------------------------------------------
;; Highlight keywords in comments: TODO FIXME NEXT THEM DONE FAIL NOTE OKAY PROG DONT TEMP KLUDGE THEM HACK XXXX
;; -------------------------------------------------------------------------------------------------------------
(use-package hl-todo
  :defer t
  :init (global-hl-todo-mode t))



;; ------------------------------
;; Browse Teletext pages in Emacs
;; ------------------------------
(use-package teletext)



;; --------------------------------
;; Read NOS Teletext pages in Emacs
;; --------------------------------
(use-package teletext-nos               ; start NOS Teletekst automatically with 's-u t' (see user-keymap.el)
  :defer t
  :quelpa (teletext-nos
           :fetcher github
           :repo "Ikuyu/emacs-teletext-nos")
  :after teletext
  :init (require 'teletext-nos))



;; ----------------------------
;; Fast jumping in visible text
;; ----------------------------
(use-package avy
  :defer t
  :config (avy-setup-default)
  :bind (("s-j" . avy-goto-char-timer))) ; jump back in sequence with C-x C-SPC



;; ----------------------------
;; Fast jumping between buffers
;; ----------------------------
(use-package frog-jump-buffer
  :defer t
  :config
  (dolist (regexp '("TAGS" "^\\*Compile-log" "-debug\\*$" "^\\:" "errors\\*$" "^\\*Backtrace" "-ls\\*$"
                    "stderr\\*$" "^\\*Flymake" "^\\*vc" "^\\*Warnings" "checkout\\*$" "\\^*Shell Command"))
    (push regexp frog-jump-buffer-ignore-buffers))
  :bind (("s-b" . frog-jump-buffer )))

(use-package frog-menu
  :custom-face (frog-menu-posframe-background-face ((t (:background "#32302f")))))



;; ------------------------------------------
;; Increase selected region by semantic units
;; ------------------------------------------
(use-package expand-region
  :bind ("C-=" . er/expand-region)) ; press C-= once to enable this mode



;; ----------------------------------------------
;; Add/change/delete pairs based on expand-region
;; ----------------------------------------------
(use-package embrace
  :config
  (setq embrace-show-help-p t) ; show/hide the help message
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
  :defer t
  :custom
  (dired-listing-switches "-lha --group-directories-first") ; human-readable listing
  (dired-recursive-copies 'always)              ; "always" means no asking
  (dired-recursive-deletes 'top)                ; "top" means ask once for top level directory
  :config
  (when (my/macos-p)
    (setq dired-use-ls-dired t
          insert-directory-program "gls"        ; make sure 'gls' is installed with 'brew install coreutils'
          trash-directory "~/.Trash"))
  (setq dired-dwim-target t                     ; if another Dired buffer is visible in another window, use that directory as target for rename/copy
        dired-kill-when-opening-new-dired-buffer t ; use 'a' to replace the current Dired buffer with the file/directory the cursor is on
        ;; dired-make-directory-clickable t        ; Emacs 29.1
        ;; dired-mouse-drag-files t                ; Emacs 29.1
        delete-by-moving-to-trash t)            ; move deleted files to trash
  :hook
  (dired-mode . dired-hide-details-mode)
  (dired-mode-hook . auto-revert-mode)) ; automatically refresh dired buffer on changes



;; --------------------------------------------------------
;; Extend build-in directory editor support with Dired-plus
;; --------------------------------------------------------
(use-package dired+
  :quelpa (dired+
           :fetcher github
           :repo "emacsmirror/dired-plus")
  :defer t
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
(use-package unkillable-scratch         ; Emacs 29: use the command 'scratch-buffer' to switch to this buffer or create a new one if it has been deleted
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
;; Markdown support (requires 'brew install pandoc')
;; -------------------------------------------------
(use-package markdown-mode
  :defer t
  :init (setq markdown-command '("pandoc" "--from=markdown" "--to=html5"))
  :config (setq markdown-fontify-code-blocks-natively t)
  :mode
  (("README\\.md\\'" . markdown-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :hook ((markdown-mode-hook)))

(use-package markdown-preview-mode
  :defer t
  :after markdown-mode
  :init (setq markdown-preview-stylesheets nil)
  :bind (:map markdown-mode-map ("C-c C-c p" . markdown-preview-mode)))



;; -----------
;; PDF support
;; -----------
(use-package pdf-tools
  :defer t
  :config
  (pdf-tools-install :no-query)
  (custom-set-variables
    '(pdf-tools-handle-upgrades t)))



;; ------------
;; EPUB support
;; ------------
(use-package esxml)

(use-package nov		    ; currently the only viable option
  :defer t
  :mode ("\\.epub\\'" . nov-mode)
  :preface
  (defun init-nov-delayed-render ()
    (run-with-idle-timer 0.2 nil 'nov-render-document))
  :custom (nov-text-width t)
  :after esxml
  :hook
  (nov-mode-hook . olivetti-mode)
  (nov-mode-hook . init-nov-delayed-render)
  (nov-mode-hook . no-fringes))



;; ------------------
;; Support for Scheme
;; ------------------
;; (use-package geiser
;;   :defer t
;;   :init
;;   (setenv "LANG" "en_US.UTF-8")     ; fix 'warning failed to install locale' (en_NL is not a locale)
;;   (setq geiser-default-implementation 'guile
;; 	geiser-mode-start-repl-p t
;; 	geiser-active-implementations '(guile))
;;   :commands (geiser run-geiser))

;; (use-package geiser-guile
;;   :after geiser)



;; -------------------------
;; Autocompletion for Geiser
;; -------------------------
;; (use-package ac-geiser
;;   :defer t
;;   :config (eval-after-load "auto-complete"
;; 	    '(add-to-list 'ac-modes 'geiser-repl-mode))
;;   :hook
;;   (geiser-mode  . ac-geiser-setup)
;;   (geiser-repl-mode . ac-geiser-setup))



;; ------------------
;; Support for Racket
;; ------------------
;; (use-package racket-mode
;;   :defer t
;;   :hook (racket-mode . racket-xp-mode))



;; -----------------------
;; Support for Common Lisp
;; -----------------------
(use-package sly
  :defer t
  :init
  (setq sly-net-coding-system 'utf-8-unix
        sly-contribs '(sly-fancy sly-repl-ansi-color sly-autodoc)
	inferior-lisp-program "clisp -q -ansi -modern -I -on-error abort"))



;; ----------------------
;; Autocompletion for Sly
;; ----------------------
(use-package ac-sly
  :defer t
  :custom (eval-after-load 'auto-complete
	    '(add-to-list 'ac-modes 'sly-mrepl-mode))
  :hook (sly-mode . set-up-sly-ac))



;; -----------------------------
;; Extend build-in ELisp support
;; -----------------------------
(use-package emacs-lisp-mode
  :defer t
  :bind (:map emacs-lisp-mode-map
              ("C-c C-b" . eval-buffer)
              ("C-c C-c" . emacs-lisp-byte-compile)
              ("C-c C-d" . eval-defun)
              ("C-c C-l" . emacs-lisp-byte-compile-and-load)
              ("C-c C-r" . eval-region)
              ("C-c C-t" . ert)))         ; run test



;; ----------------------------------
;; behavior-driven Emacs lisp testing
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
  :defer t
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
  :config
  (setq c-block-comment-prefix "* "
        c-default-style "gnu"
        c-basic-offset 4)
  :bind (:map c-mode-map
              ("C-c c"   . compile)
              ("C-c C-c" . recompile)
              ("C-c g"   . gdb)
              ("C-c C-r" . gdb-run)))



; ----------------------------------------
;; Edit multiple occurrences simultaneously
;; ----------------------------------------
;; https://www.youtube.com/watch?v=3O-bDYqhFos
(use-package multiple-cursors		; alternative iedit
  :if (not my/modus-tollens)
  :init (setq mc/always-run-for-all t)
  :bind ("S-s-<mouse-1>" . mc/add-cursor-on-click))



;; -----------
;; Git support
;; -----------
(use-package magit
  :defer t)



;; ---------------------------
;; Keep git repositories clean
;; ---------------------------
(use-package gitignore-templates
  :defer t)



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



;; ----------------
;; SPS for org-mode
;; ----------------
(use-package org-drill			; alternatives: {'Pamparam,'Org-fc,'Anki-editor,'Anki.el,'Ankifier.el}
  :defer t)



;; ----------------------------
;; Slipbox support for org-mode
;; ----------------------------
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



;; ---------------------------------------------------------------------------------------------------------------
;; Browse the notes network in an interactive graph in the browser to remember certain relationships between notes
;; ---------------------------------------------------------------------------------------------------------------
(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-browser-function #'browse-url-chromium
        org-roam-ui-open-on-start nil))



;; ---------------------------------------
;; Use pandoc to convert files to org-mode
;; ---------------------------------------
(use-package org-pandoc-import
  :defer t
  :after org
  :quelpa (org-pandoc-import
           :fetcher github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))



;; -----------------------------------------------------
;; Perform document conversions using the pandoc library
;; -----------------------------------------------------
(use-package pandoc-mode
  :after org)
