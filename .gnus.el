;; ====
;; GNUS
;; ====



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
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANYr
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;    __ _ _ __  _   _ ___
;;   / _` | '_ \| | | / __|
;;  | (_| | | | | |_| \__ \
;; (_)__, |_| |_|\__,_|___/
;;   |___/

;; Gnus supports newsgroups (NNTP), email accounts (IMAP/POP) and sending mails (IMAP). Each is called a server.
;; A server can have many groups. For email servers, groups are just mail folders.
;; By default, all folders are invisible. Unless a group contains `favorites' (ticked items' you need to subscribe to it to make it visible.
;; Gmail for example, counts by mail thread. Gnus counts by individual mail.

;; Passwords are stored in '~/.authinfo'. Make sure no one else can read your them:
;; $ chmod 600 ~/.authinfo

;; Close Gnus with 'q' instead of 'C-x k'. This prevents Gnus from displaying the message 'Gnus auto-save file exists. Do you want to read it? (y or n) when it start up.
;; Use 'L' to list all groups.
;; Use 'u' to subsribe to one or more groups/inboxes/accounts.



;; ----------------------------------------------------------
;; Setup build-in email support (requires 'brew install w3m')
;; ----------------------------------------------------------
(use-package gnus
  :defer t
  :init (gnus-delay-initialize)
  :config
  (setq gnus-save-newsrc-file nil            ; don't save '.newsrc' (for using other newsreaders) on exit
        gnus-read-newsrc-file nil            ; ignore the '.newsrc' file
        gnus-active-file nil                 ; Gnus will only know about the groups in the `.newsrc' file
        gnus-summary-goto-unread 'never      ; commands should not atempt to go to the next unread article
        gnus-group-list-inactive-groups t    ; list inactive groups
        gnus-select-method '(nnnil nil)      ; avoid Gnus trying to connect to a non-existing local news server
        gnus-secondary-select-methods        ; setup multiple accounts
        '((nntp "news.gwene.org")
          (nnimap "imap.gmail.com"           ; as for Gmail, make sure imap support is enabled in the account settings of the Gmail account
                  (nnimap-address "imap.gmail.com")
                  (nnimap-server-port 993)
                  (nnimap-stream ssl)
                  (nnimap-authinfo-file "~/.authinfo"))
          (nnimap "edwin.tope.nu"
                  (nnimap-address "imap.one.com")
                  (nnimap-server-port 993)
                  (nnimap-stream ssl)
                  (nnimap-authinfo-file "~/.authinfo"))
          (nnimap "velijnboeken.tope.nu"
                  (nnimap-address "imap.one.com")
                  (nnimap-server-port 993)
                  (nnimap-stream ssl)
                  (nnimap-authinfo-file "~/.authinfo")))
        gnus-ignored-newsgroups ""           ; don't ignore [Gmail]/* groups
        gnus-posting-styles '(((header "to" "hetlevenkronen@gmail.com") ; reply-to with the same address as it was sent to
                               (address "hetlevenkronen@gmail.com"))
                              ((header "to" "edwin@tope.nu")
                               (address "edwin@tope.nu"))
                              ((header "to" "velijnboeken@tope.nu")
                               (address "velijnboeken@tope.nu")))
        gnus-add-timestamp-to-message t
        gnus-expert-user t                   ; never ask for confirmation about anything
        gnus-large-newsgroup nil             ; groups of 200+ articles are not considered big
        gnus-always-read-dribble-file t      ; unconditionally read the dribble file
        gnus-treat-display-smileys nil  ; do not display smileys as pictures
        gnus-treat-fill-article 0
        gnus-treat-buttonize t               ; add buttons
        gnus-treat-buttonize-head 'head
        gnus-inhibit-images nil
        mm-text-html-renderer 'gnus-w3m      ; or 'shr, eww's text renderer
        mm-inline-text-html-with-images t    ; allow retrieving images in html contents with the <img> tags
        mm-attachment-override-types '("image/.*") ; inline images?
        mm-file-name-rewrite-functions '(mm-file-name-delete-control       ; rewrite file names of MIME parts (delete control characters, delete shell gotchas, handle evil white spaces)
                                         mm-file-name-delete-gotchas
                                         mm-file-name-trim-whitespace
                                         mm-file-name-collapse-whitespace
                                         mm-file-name-replace-whitespace)
        mm-default-directory "~/Downloads/"  ; default directory for saving attachments
        gnus-summary-line-format (concat
                                  "%0{%U%R%z%}"                 ;; Status
                                  "%3{│%}" "%1{%d%}" "%3{│%}"   ;; date
                                  "  "
                                  "%4{%-20,20f%}"               ;; name
                                  "  "
                                  "%3{│%}"
                                  " "
                                  "%B"
                                  "%s\n")
        gnus-summary-make-false-root 'adopt  ; adopt a previous line in for our false roots
        gnus-thread-sort-functions '(gnus-thread-sort-by-number
                                     gnus-thread-sort-by-most-recent-date))
  (if window-system
      (setq
       ;; Summary Threading Marks
       gnus-sum-thread-tree-indent          " "
       gnus-sum-thread-tree-false-root      "☆ "
       gnus-sum-thread-tree-single-indent   "◎ "
       gnus-sum-thread-tree-root            "● "
       gnus-sum-thread-tree-vertical        "┃"
       gnus-sum-thread-tree-leaf-with-other "┣━❯ "
       gnus-sum-thread-tree-single-leaf     "┗━❯ "
       ;; Summary Mode Marks
       gnus-score-over-mark  ?↑
       gnus-score-below-mark ?↓
       gnus-ticked-mark      ?⚑
       gnus-dormant-mark     ?⚐
       gnus-expirable-mark   ?♻
       gnus-read-mark        ?✓
       gnus-del-mark         ?✗
       gnus-killed-mark      ?☠
       gnus-replied-mark     ?⟲
       gnus-forwarded-mark   ?⤳
       gnus-cached-mark      ?☍
       gnus-recent-mark      ?★
       gnus-unseen-mark      ?✩
       gnus-unread-mark      ?✉))
  :hook
  (gnus-save-quick-newsrc-hook . (lambda () (set (make-local-variable 'backup-inhibited) t)))
  (gnus-save-standard-newsrc-hook . (lambda () (set (make-local-variable 'backup-inhibited) t)))
  (gnus-after-getting-new-news-hook . gnus-group-first-unread-group))



;; ---------------------------
;; Setup built-in html support
;; ---------------------------
(use-package shr
  :defer t
  :init
  (defun oni:shr-colorize-remove-last-arg (args)
    "If ARGS has more than 3 items, remove the last one."
    (if (> (length args) 3)
        (butlast args)
      args))
  :config
  (setq shr-color-visible-distance-min 10
        shr-color-visible-luminance-min 60)
  (advice-add #'shr-colorize-region :filter-args
              #'oni:shr-colorize-remove-last-arg))



;; ---------------------------------------------------
;; Change accounts on the fly while composing messages
;; ---------------------------------------------------
(use-package gnus-alias
  :defert t
  :init
  (autoload 'gnus-alias-determine-identity "gnus-alias" nil t)
  (gnus-alias-init)                                                   ; call to message mode hook
  :config
  ;; setup identities
  (setq gnus-alias-identity-alist '(("personal"
                                     nil                              ; not referencing another identity
                                     "Edwin H. Jonkvorst-Claes <hetlevenkronen@gmail.com>"
                                     nil                              ; no organization header
                                     nil                              ; no extra headers
                                     "Met een groet,\n"               ; extra body text
                                     "Dhr. Edwin H. Jonkvorst-Claes")
                                    ("partnership"
                                     nil
                                     "Edwin H. Jonkvorst-Claes <edwin@tope.nu>"
                                     "Tope.nu bv"
                                     nil
                                     "Met een groet,\n"
                                     "Dhr. Edwin H. Jonkvorst-Claes\nTOPE.NU bv\nKattenstraat 78b\n8800 Roeselare")
                                    ("sales"
                                     nil
                                     "Edwin H. Jonkvorst-Claes <velijnboeken@tope.nu>"
                                     "Velijn Boeken"
                                     nil
                                     "Met een groet,\n"
                                     "Edwin & Ingeborg\nVelijn Boeken\nPartner van Bol.com"))
        gnus-alias-default-identity "personal"   ; set default identity
        gnus-alias-unknown-identity-rule 'error  ; identity to use when gnus-alias finds an unknown identity
        gnus-alias-overlay-identities nil        ; old identity is completely removed before the new one is added.
        gnus-alias-override-user-mail-address t  ; allow your `Return-Path' to be set properly
        gnus-alias-point-position 'start-of-sig  ; after an identity is used, where should point be moved to?
        gnus-alias-use-buttonized-from t         ; 'From' header becomes a button that you can click on
        gnus-alias-verbosity 1)
  :bind (:map message-mode-map ("C-c i" . (lambda ()
                                            (interactive)
                                            (goto-char (point-min))
                                            (search-forward "Met een groet," nil t)
                                            (delete-region (match-beginning 0) (point-max))
                                            (gnus-alias-select-identity))))
  :hook (message-setup-hook . gnus-alias-determine-identity))
