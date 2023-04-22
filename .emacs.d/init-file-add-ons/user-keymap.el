;; ===========
;; User keymap
;; ===========


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



(define-prefix-command 'user-map)
(global-set-key (kbd "s-u") 'user-map)                   ; add prefix key ⌘u | Emacs 29: (keymap-global-set "s-u" 'user-map)
(define-key user-map (kbd "b") (lambda ()
                                 (interactive)
                                 (if (and (package-installed-p 'boon) (not modus-tollens))
                                     (boon-mode 'toggle)
                                   (message "Error enabeling Boon:\n(Searching for program No such file or directory boon)"))
                                 ;;(setq cursor-type 'box)
                                 ))
(define-key user-map (kbd "d") 'dired)
(define-key user-map (kbd "e") (lambda ()                ; to kill the ERC buffer and terminate its child process use C-x k
				 (interactive)
				 "Start ERC using TLS with preset information."
				 (erc-tls :server "irc.libera.chat"
					  :port "6697"
					  :nick "ehjc")))
(define-key user-map (kbd "f") 'find-name-dired)
(define-key user-map (kbd "g") (lambda ()
				 (interactive)
                                 (if (package-installed-p 'geiser)
                                     (geiser)
                                   (message "Error enabeling Geiser:\n(Searching for program No such file or directory geiser)"))))
(define-key user-map (kbd "G") 'gnus)
(define-key user-map (kbd "i") 'ielm)
(define-key user-map (kbd "m") (lambda ()
                                 (interactive)
                                 (if (package-installed-p 'magit)
                                     (magit)
                                   (message "Error enabeling Magit:\n(Searching for program No such file or directory magit)"))))
(define-key user-map (kbd "N") (lambda ()
                                 (interactive)
                                 (if (package-installed-p 'nov)
                                     (nov-mode)
                                   (message "Error enabeling Nov:\n(Searching for program No such file or directory nov)"))))
(define-key user-map (kbd "o") (lambda ()
                                 (interactive)
                                 (if (package-installed-p 'olivetti)
                                     (olivetti-mode 'toggle)
                                   (message "Error enabeling Olivetti:\n(Searching for program No such file or directory olivetti)"))))
(define-key user-map (kbd "O") (lambda ()
                                 (interactive)
                                 (if (package-installed-p 'org-drill)
                                     'org-drill
                                   (message "Error enabeling Org Drill:\n(Searching for program No such file or directory org-drill)"))))
(define-key user-map (kbd "r") (lambda ()
                                 (interactive)
                                 (load-file user-init-file)))
(define-key user-map (kbd "u") (lambda ()
                                 (interactive)
                                 (if (package-installed-p 'auto-package-update)
                                     (auto-package-update-now)
                                   (message "Error enabeling Auto Package Update:\n(Searching for program No such file or directory auto-package-update))")))) ; from package 'auto-package-update' (see packages.el) | obsolete in Emacs 29?
(define-key user-map (kbd "s") (lambda ()
                                 (interactive)
                                 (if (package-installed-p 'sly)
                                     (sly)
                                   (message "Error enabeling Sly:\n(Searching for program No such file or directory sly)"))))
(define-key user-map (kbd "t") (lambda ()
                                 (interactive)
                                 (if (package-installed-p 'teletext)
                                     (teletext)
                                   (message "Error enabeling Teletext:\n(Searching for program No such file or directory teletext)"))
                                 (if (package-installed-p 'teletext-nos)
                                     (progn
                                       (teletext-select-network "NOS"))
                                   (message "Error enabeling Teletext NOS:\n(Searching for program No such file or directory teletext-nos)"))))
(define-key user-map (kbd "T") (lambda ()
				 (interactive)
                                 (if (package-installed-p 'terminal-here)
                                     (terminal-here)
                                   (message "Error enabeling Terminal Here:\n(Searching for program No such file or directory terminal-here)"))))
