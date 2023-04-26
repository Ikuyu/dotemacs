;; ==============
;; Email settings
;; ==============



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



;; Emacs-wide variables to be used in other locations besides email.
(setq user-full-name "Edwin H. Jonkvorst-Claes"
      user-mail-address (concat "hetlevenkronen" "@" "gmail" ".com")) ; spammers are everywhere.



;; ----------------
;; Read mail (Gnus)
;; ----------------
(setq read-mail-command 'gnus) ; used by some keybindings that support reading mail



;; ----------
;; Write mail
;; ----------
(setq mail-user-agent 'gnus-user-agent) ; which compose window should appear when activating a mailto-link



;; ---------
;; Send mail
;; ---------
;; Install 'msmtp' (msmtp can use email account passwords stored in Apple's Keychain):
;; $ brew install msmtp
;; $ mkdir ~/.msmtprc
;;
;; Modify '~/.msmtprc' (msmts's configuration file) and change its read/write permissions:
;; $ chmod 600 ~/.msmtprc
;;
;; Add msmtp's path to '~/.mailrc':
;; $ echo set sendmail="/opt/homebrew/bin/msmtp" > ~/.mailrc
(setq message-send-mail-function 'message-send-mail-with-sendmail
      message-sendmail-f-is-evil t                                       ; don’t add "-f username" to the sendmail command line
      message-sendmail-envelope-from 'header                             ; use the 'From:' header of the message
      sendmail-program (substring (shell-command-to-string "which msmtp") 0 -1)
      message-default-mail-headers "Cc: \nBcc: \n"
      message-signature (concat "Dhr. " user-full-name)                  ; default signature
      message-confirm-send t                                             ; ask for confirmation before sending a message
      message-kill-buffer-on-exit t)                                     ; delete message window after sending
