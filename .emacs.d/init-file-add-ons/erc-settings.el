;; ------------
;; Erc settings
;; ------------



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



;; ---------------------------
;; Extend built-in irc support
;; ---------------------------
(use-package erc
  :defer t
  :config
  (setq erc-user-full-name user-full-name
        erc-nick "ehjc"
        erc-email-userid user-mail-address
        erc-prompt-for-password nil
        erc-prompt-for-nickserv-password nil
        erc-autojoin-channels-alist '(("irc.libera.chat" "#emacs"))
        erc-prompt (lambda () (concat erc-nick "@ERC " (buffer-name) " %")) ; change prompt
        ;;bye-erc-message "Stay well!"
        ;;erc-auto-query 'bury		            ; hide new messages in the bufferlist instead of popping up
        ;;erc-fill-function 'erc-fill-static        ; align chat-usernames
        ;;erc-fill-static-center 20	            ; with chat-messages with the chat-usernames column being 20 characters wide
        erc-server-coding-system '(utf-8 . utf-8) ; encoding with utf-8
        erc-hide-timestamps nil
        ;;erc-interpret-mirc-color t ; mIRC-style color commands in IRC chats
        erc-hide-list '("JOIN" "PART" "QUIT" "NICK") ; exclude server messages
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
				  "324" "329" "332" "333" "353" "477") ; hide server messages
        erc-kill-buffer-on-part t          ; kill buffers after /part
        erc-kill-queries-on-quit t         ; kill buffers for private queries after quitting the server
        erc-kill-server-buffer-on-quit t)) ; kill buffers for server messages after quitting the server
