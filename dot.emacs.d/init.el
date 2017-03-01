;;; Init for GNU Emacs
;;; (C) 2004-2007 Stefan Kamphausen www.skamphausen.de

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; It is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;; -----------------------------------------------------------------------

(setq debug-on-error nil)
(defvar ska-init-verbose nil)

(defmacro ska-init-message (&rest args)
  (when ska-init-verbose
    `(message (concat 
               (format "init [%s]: " (marknow))
               (format ,@args)))))

(defun marknow ()
  (interactive)
  (let* ((ct (current-time))
         (timest (format-time-string "%H:%M:%S" ct))
         (msec (car (cdr (cdr ct))))
         )
    (format "%s:%s" timest msec)))

(defun try-require (feature)
  (ska-init-message "Requiring: %s" feature)
  (condition-case nil
      (require feature)
    (error (progn
             (message "could not require %s" feature)
             nil))))

(ska-init-message "Start of init.el")

(when window-system
  ;; enable wheelmouse support by default
  (mwheel-install)
  ;; use extended compound-text coding for X clipboard
  (set-selection-coding-system 'compound-text-with-extensions))

;; package
(setq package-enable-at-startup nil)
(require 'package)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; package usually loads packages after init.el; this breaks my way of
;; configuring things here.  See
;; e.g. http://www.emacswiki.org/emacs/ELPA
;; Thus, I initialize explicitly:
(package-initialize)

;; paths and files
(setq load-path
     (append
      (list
       "~/.emacs.d/lisp"
; install from package       "~/local/opt/helm"
       "~/local/opt/cider"
       "~/local/opt/clojure-mode"
       "~/local/opt/smartparens/"
;       "~/local/opt/slime/"
       )
      load-path))

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))


;; Gentoo
;(setq gentoo-site-file "/usr/share/emacs/site-lisp/site-gentoo")
;(when (file-exists-p gentoo-site-file)
;  (load gentoo-site-file))
;; (try-require 'site-gentoo)

;; User Specific
(setq ska-user-init-file "~/.emacs.d/user.el")
(when (file-exists-p ska-user-init-file)
  (load ska-user-init-file))
;;; packages
(ska-init-message "Requiring Packages")

(try-require 'comint)
(try-require 'speedbar)
(try-require 'cperl-mode)
(try-require 'cc-mode)
(try-require 'cdargs)
(try-require 'chb-util)
(try-require 'highlight-context-line)
(try-require 'll-debug)
(try-require 'eldoc)
(try-require 'apropos-toc)
(try-require 'skeleton)
;; Generic Configuration files (ini, properties...)
(try-require 'conf-mode)

;; since emacs 22 the greying out is default and I will try it..
;;(try-require 'minibuf-electric-gnuemacs)
;; since emacs 22 completion works on M-x load-library
;;(try-require 'find-library)



;;;; =================================================================
;;;; DEFUNS

;; functions
(ska-init-message "Defining functions")

(defun ska-untabify ()
  "My untabify function as discussed and described at
http://www.jwz.org/doc/tabs-vs-spaces.html
and improved by Claus Brunzema:
 - return nil to get `write-contents-hooks' to work correctly
   (see documentation there)
 - `make-local-hook' instead of `make-local-variable'
 - when instead of if
Use some lines along the following for getting this to work in the
modes you want it to:
 
\(add-hook 'some-mode-hook  
          '(lambda () 
              (make-local-hook 'write-contents-hooks) 
               (add-hook 'write-contents-hooks 'ska-untabify nil t)))"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "\t" nil t)
      (untabify (1- (point)) (point-max)))
    nil))

(defun show-message-log ()
  (interactive)
  (switch-to-buffer-other-window "*Messages*")
  (goto-char (point-max)))

(defun chb-home ()
  (interactive)
  (if (not (bolp))
      (beginning-of-line)
    (if (eq this-command last-command)
        (cond
         ((not (= (point) (window-start)))
          (move-to-window-line 0)
          (beginning-of-line))
         (t
          (goto-char (point-min)))))))

(defun chb-end ()
  (interactive)
  (if (not (eolp))
      (end-of-line)
    (if (eq this-command last-command)
        (cond
         ((not (= (point) (save-excursion
                            (move-to-window-line -1)
                            (end-of-line)
                            (point))))
          (move-to-window-line -1)
          (end-of-line))
         (t
          (goto-char (point-max)))))))

(defun ska-point-to-register()
  "Store cursorposition _fast_ in a register. Use ska-jump-to-register
to jump back to the stored position."
  (interactive)
  (point-to-register 8))

(defun ska-jump-to-register()
  "Switches between current cursorposition and position
that was stored with ska-point-to-register."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp)))

(defun ska-insert-x-selection ()
  "Insert the current X selection at point."
  (interactive)
  (insert (x-get-selection)))

(defun ska-kill-entire-line ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (kill-region (point)
                 (save-excursion
                   (forward-line 1)
                   (point)))))

(defun ska-line-to-kill-ring ()
  "Puts the current line on top of the kill ring."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (copy-region-as-kill (point)
                         (save-excursion
                           (forward-line 1)
                           (point)))))

;; f4 fuer kill-buffer ist ja nett, aber oft brauche ich auch
;; killbuffer und schliess window (auf shift f4...)
(defun ska-kill-this-window ()
  "Kill buffer in current window and delete window after that."
  (interactive)
  (kill-this-buffer)
  (delete-window))

;; aehnliches fuer switch-to-buffer:
;; switch to buffer und kill this window
;; key-Vorschlag C-x B
(defun ska-switch-to-buffer-whole-window (BUFNAME)
  "Switch to chosen buffer and make that the only window."
  (interactive "BSwitch to buffer in whole window: ")
  (switch-to-buffer BUFNAME)
  (delete-other-windows))

(defun ska-mail-insert-answer-gap ()
  "Insert a nicely formatted gap when answering an email."
  (interactive)
  (if (bolp)
      (insert-char ?\n 1)
    (insert-char ?\n 2))
  (save-excursion
    (insert-char ?\n 2)
    (insert " >")
    (when (not (looking-at " ")) (insert " "))))

(defun ska-electric-transpose-chars ()
  "Replacement for the default transpose-chars command.
This is usually bound to C-t and it behaves somewhat unintelligent
because I always have to move back one char when I mistyped
something. This function checks whether the user is typing and then
goes back one char itself."
  (interactive)
  (if (eq last-command 'self-insert-command)
      (progn (transpose-chars -1)
             (forward-char))
    (transpose-chars -1)))


(defun insert-iso-time ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun ska-toggle-long-lines ()
  (interactive)
  (setq truncate-lines (not truncate-lines)))



(defun ska-coding-keys (map)
  "Sets the key bindings which shall be available in all programming
languages. Argument MAP is the local keymap (e.g. cperl-mode-map)."
  (define-key map '[(return)]                 'newline-and-indent)
  (define-key map '[(control b) (control c)]  'll-debug-copy-and-comment-region-or-line)
  (define-key global-map '[(control v) (control d)]
    #'(lambda ()
        (interactive)
        (if current-prefix-arg
            (ll-debug-insert-variable-output)
          (ll-debug-insert-debug-output))))
  )



;;;; =================================================================
;;;; Packages
(ska-init-message "Setting up packages")

;; Company
(when (try-require 'company)
  (add-hook 'after-init-hook 'global-company-mode))


;; Recent Files
(ska-init-message "   RecentFiles")
(when (try-require 'recentf)
  (setq recentf-exclude 
        '("~$" "^/ftp:" "^/ssh:"))
  (setq recentf-max-saved-items 99)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Here I override some functionf of recentf to get a merged list
  ;; This will have to prove stable
  (defun recentf-save-list ()
    "Save the recent list.
Load the list from the file specified by `recentf-save-file',
merge the changes of your current session, and save it back to the
file."
    (interactive)
    (let ((instance-list (copy-list recentf-list)))
      (recentf-load-list)
      (recentf-merge-with-default-list instance-list)
      (recentf-cleanup)
      (recentf-write-list-to-file)))

  (defun recentf-merge-with-default-list (other-list)
    "Add all items from `other-list' to `recentf-list'."
    (dolist (oitem other-list)
      ;; add-to-list already checks for equal'ity
      (add-to-list 'recentf-list oitem)))

  (defun recentf-write-list-to-file ()
    "Write the recent files list to file.
Uses `recentf-list' as the list and `recentf-save-file' as the
file to write to."
    (condition-case error
        (with-temp-buffer
          (erase-buffer)
          (set-buffer-file-coding-system recentf-save-file-coding-system)
          (insert (format recentf-save-file-header (current-time-string)))
          (recentf-dump-variable 'recentf-list recentf-max-saved-items)
          (recentf-dump-variable 'recentf-filter-changer-current)
          (insert "\n\n;;; Local Variables:\n"
                  (format ";;; coding: %s\n" recentf-save-file-coding-system)
                  ";;; End:\n")
          (write-file (expand-file-name recentf-save-file))
          (when recentf-save-file-modes
            (set-file-modes recentf-save-file recentf-save-file-modes))
          nil)
      (error
       (warn "recentf mode: %s" (error-message-string error)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (recentf-mode 1))
;;  (recentf-mode 1))

;; MTorus
(ska-init-message "   MTorus")
(when (try-require 'mtorus)
  (mtorus-init)
  (mtorus-install-suggested-bindings)

  (defun mtorus-my-buffer-skip-p (buffer)
    "The my predicate used for `mtorus-buffer-skip-p'
is to skip only the special buffers whose name begins with a space."
    (or
     (string-match "^\\*\\(Messages\\|helm\\|Help\\)\\*" (buffer-name buffer))
     (string-match "^\\*\\(cider-err\\|cider-doc\\)" (buffer-name buffer))
     (string-match "^\\*\\(nrepl-mess\\)" (buffer-name buffer))
     (string-match "^[ ]+"         (buffer-name buffer))
     (string-match "[Cc]ompletio" (buffer-name buffer))
     (string-match "slime-events" (buffer-name buffer))
     (string-match "SLIME Note" (buffer-name buffer))
     (string-match "inferior-lisp" (buffer-name buffer))
     ;; interferes with compiler.clj in morocco
     ;; (string-match "[Cc]ompil" (buffer-name buffer))
     ))
  
  (setq mtorus-buffer-skip-p 'mtorus-my-buffer-skip-p))

  
;; Picklist
;; (when (try-require 'picklist)
;;   (setq picklist-permanent-submenu t)
;;   (setq picklist-commands-submenu t)
;;   (setq picklist-number-of-menu-entries 50)
;;   (setq picklist-dont-include 
;;         '("~$" "tmp/." "contact/." "INBOX" ".bbdb" ".newsrc."
;;           "places.el"))
;;   (setq picklist-sort-function #'picklist-sort-atime)
;;   ;(setq picklist-add-menu-before "Edit") 
;;   (picklist-initialize))

;; Autoinsert
(ska-init-message "   Autoinsert")
(when (try-require 'autoinsert)
  (if (boundp 'my-copyright-holder)
      (setq auto-insert-copyright my-copyright-holder)
    (setq auto-insert-copyright (user-full-name)))

  (add-hook 'find-file-hooks 
            '(lambda ()
               (auto-insert)
               )))


;; Menubar, now with ActiveMenu for Gnu Emacs
;; For Gtk-Gnu-Emacs this is just broken, so disable it for the time being
;;(ska-init-message "   ActiveMenu")
;;(menu-bar-mode nil)
;; (defvar menubar-active-p nil)
;; (defun ska-toggle-menubar-mode ()
;;   (interactive)
;;   (setq menubar-active-p (not menubar-active-p))
;;   (menu-bar-mode menubar-active-p))
;;(when (try-require 'active-menu)
;;  (turn-on-active-menu)
;;  (setq active-menu-frame-compensation 2)) ;; depends on menubarfont


(ska-init-message "   Ispell")
(setq ispell-program-name "aspell")

(ska-init-message "   Flyspell")
(require 'flyspell)
(define-key flyspell-mode-map '[(control \.)] 'ska-point-to-register)
(define-key flyspell-mode-map '[(control \,)] 'ska-jump-to-register)


;; (ska-init-message "   Anything")
;; (when (try-require 'anything-config)
;;   (defun ska-anything ()
;;     (interactive)
;;     (anything-other-buffer
;;      '(anything-c-source-buffers+
;;        anything-c-source-file-name-history
;;        anything-c-source-recentf
;;        anything-c-source-files-in-current-dir+
;;        anything-c-source-locate
;;        anything-c-source-man-pages
;;        anything-c-source-emacs-commands)
;;      " *ska-anything*"))
  
;;   nil)

;; Helm, heir of anything
(ska-init-message "   Helm")
(when (and (try-require 'helm)
           (try-require 'helm-config))
  ; I hate the interference with find-file (helm-mode 1)
  ;; Move helm prefix 
  (global-set-key (kbd "C-S-SPC") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  
  t)

(ska-init-message "   Mail And News (Gnus, Message, BBDB)")

(define-skeleton ska-skel-mail-sgdh
  "Opening (german with women)."
  nil
  "Sehr geehrte Damen und Herren,\n\n"
  )

(define-skeleton ska-skel-mail-sgh
  "Opening (german)."
  nil
  "Sehr geehrte Herren,\n\n"
  )


(define-skeleton ska-skel-mail-ska
  "My Name."
  nil
  "Stefan Kamphausen\n"
  )

(define-skeleton ska-skel-mail-regards
  "Closing (english)."
  nil
  "Regards\n"
  "Stefan Kamphausen\n"
  )

(define-skeleton ska-skel-mail-mfg
  "Closing (german)."
  nil
  "Mit freundlichen Grüßen\n"
  "Stefan Kamphausen\n"
  )

(define-skeleton ska-skel-mail-coffee-or-tee
  "Ask my colleagues for coffee or tee."
  nil
  "(kaff|t)ee?"
  )

(defun ska-message-keys ()
  "Set keybindings for mail writing buffers."
  (interactive)
  (local-set-key '[(control b) (control d)]  'ska-skel-mail-sgdh)
  (local-set-key '[(control b) (control h)]  'ska-skel-mail-sgh)
  (local-set-key '[(control b) (control m)]  'ska-skel-mail-mfg)
  (local-set-key '[(control b) (control n)]  'ska-skel-mail-ska)
  (local-set-key '[(control b) (control r)]  'ska-skel-mail-regards)
  (local-set-key '[(control b) (control k)]  'ska-skel-mail-coffee-or-tee)
  (local-set-key '[(control b) (control b)]  'ska-mail-insert-answer-gap)
  (local-set-key '[(control b) (control s) (control p)]  'ska-gnus-sent-mail-as-private)
  (local-set-key '[(control b) (control s) (control b)]  'ska-gnus-sent-mail-as-bn)
  (local-set-key '[(control b) (control s) (control t)]  'ska-gnus-sent-mail-as-tt)
  (local-set-key '[(control b) (control c)]  'ska-gruesse)
  (local-set-key '[(control b) (control f)]  'Footnote-add-footnote)
  (local-set-key '[(control b) (f)]          'Footnote-back-to-message)
  )

(defun ska-article-keys ()
  "Set keybindings in Gnus Article buffers."
  (local-set-key '[(control b) (control s)]  'gnus-mime-save-part)
)

(setq gnus-init-file (expand-file-name "~/.emacs.d/gnus-init.el"))
(setq gnus-directory (expand-file-name "~/contact/gnus/"))
(setq message-directory (expand-file-name     "~/contact/gnus/"))
(setq bbdb-file (expand-file-name   "~/.emacs.d/bbdb"))
(add-hook 'gnus-select-group-hook
          '(lambda ()
             (setq bbdb/news-auto-create-p
                   (cond
                    ((string-match  "^nnml.mail.acrolinx"
                                    gnus-newsgroup-name)
                     t)
                    ((string-match  "^nnml.mail.software.*"
                                    gnus-newsgroup-name)
                     nil)
                    ((string-match  "^nntp" gnus-newsgroup-name)
                     nil)
                    (t
                     'bbdb-ignore-most-messages-hook)))))
(setq bbdb-ignore-most-messages-alist 
      '(("Subject" . "\\(emacs\\|cdargs\\|cl-ipc\\|Web-Kontakt\\)"))
      bbdb-ignore-some-messages-alist
      '(("From" . "daemon")
        ("From" . "abuse@")
        ("From" . "news@")
        ("From" . "cron")
        ("From" . "root@")
        ("From" . "noreply@")
        ("From" . "prontomail")
        ("From" . "majordomo")
        ("From" . "postmaster")
        ("From" . "quota")
        ("Subject" . "MAILER DAEMON")))
(setq bbdb-notice-hook 'bbdb-auto-notes-hook)
(setq bbdb-auto-notes-alist
      '(("Organization" (".*" company 0))
        ("Newsgroup" ("[^,]+" newsgroups 0))
        ("Subject" (".*" last-subj 0 t))
        ("User-Agent" (".*" mailer 0))
        ("X-Newsreader" (".*" mailer 0))
        ("X-gpg-key-ID" (".*" key 0))
        ("X-Mailer" (".*" mailer 0))
        ("X-URL" (".*" url 0))
        ;;("X-Face" (".+" face 0 'replace))
        ;;("Face" (".+" cface 0 'replace))
        ))
(setq bbdb-default-area-code "030"
            bbdb-north-american-phone-numbers-p nil
            bbdb-complete-name-allow-cycling t)

;; Organizing with Org and Remember
(ska-init-message "   OrgMode")

(setq org-replace-disputed-keys t)
(when (try-require 'org)
  ;; basic setup
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (define-key global-map "\C-cl" 'org-store-link)
  (setq org-log-done t)
  (setq org-hide-leading-stars t)
  (setq org-startup-folded nil)
  ;; agenda
  (setq org-agenda-files 
        (list "~/Dokumente/org.org"))
  ;; setup according to 
  ;; http://www.newartisans.com/blog_files/org.mode.day.planner.php
  (setq remember-data-file (expand-file-name "~/org/inbox.org"))
  (setq remember-handler-functions '(org-remember-handler))
  (setq remember-annotation-functions '(org-remember-annotation))
  (setq org-default-notes-file nil)
  (setq org-remember-store-without-prompt t)
  (setq org-remember-templates
        ;; name accesschar template file headline
        '(("default" ?d "* TODO %?\n  %u" "~/org/inbox.org"
           "Tasks")))
  ;; shiny and new: mobileorg
  (setq org-mobile-directory "~/org/mobile/")

  ;; (setq org-mobile-directory "~/org/")
  (add-hook 'org-mobile-post-push-hook
            (lambda () (shell-command "scp -r ~/org/mobile/* ska@vvoo.de:org/webdav/export/")))
  (add-hook 'org-mobile-pre-pull-hook
            (lambda () (shell-command "scp ska@vvoo.de:org/webdav/export/mobileorg.org ~/org/mobile/ ")))
  (add-hook 'org-mobile-post-pull-hook
            (lambda () (shell-command "scp ~/org/mobile/mobileorg.org ska@vvoo.de:org/webdav/export/")))

)


(ska-init-message "   Remember")
(when (try-require 'remember)
  (add-hook 'remember-mode-hook 'org-remember-apply-template))
;; Keybinding in ska-ctrl-v-map below


;; We always need footnotes
(ska-init-message "   Footnotes")
(try-require 'footnote)

;; Highlight current line
;; (ska-init-message "   HighLine")
;; (when (try-require 'highline)
;;   (global-highline-mode))


;; Magit
(ska-init-message "   Magit")
(when (try-require 'magit)
  t)


;; Filladapt
;; (ska-init-message "   FillAdapt")
;; (when (try-require 'filladapt)
;;   t)

(ska-init-message "   MouseDrag")
(when (try-require 'mouse-drag)
  (setq mouse-throw-with-scroll-bar t)
  (global-set-key [down-mouse-2] 'mouse-drag-drag)
  (global-set-key [C-down-mouse-2] 'mouse-drag-throw))

(ska-init-message "   LinNum")
(when (try-require 'linum)
  (global-linum-mode))

(ska-init-message "   TimeStamp")
(when (try-require 'time-stamp)
  (add-hook 'write-file-hooks 
            '(lambda ()
               (time-stamp)
               )))

(ska-init-message "   WindResize")
(when (try-require 'windresize)
  (global-set-key '[(f8)] 'windresize)
  (global-set-key '[(control shift left)]  'windresize-select-left)
  (global-set-key '[(control shift right)] 'windresize-select-right)
  (global-set-key '[(control shift up)]    'windresize-select-up)
  (global-set-key '[(control shift down)]  'windresize-select-down))

(ska-init-message "   FDLCaps")
(try-require 'fdlcap)

(ska-init-message "   iedit")
(when (try-require 'iedit)
  (global-set-key '[(control \;)] 'iedit-mode))

(ska-init-message "   key-chord")
(when (try-require 'key-chord)
  (key-chord-mode 1))
  ;; Moved keychords to local hooks and test for availability of key-chord

(ska-init-message "   which-func")
(when (try-require 'which-func)
  (setq which-func-modes t) ; enable for all modes
  (which-function-mode 1))

(ska-init-message "   ERC")
(when (try-require 'erc)
  ;; defaults to freenode (setq erc-default-server "10.42.254.90")
  (setq erc-autojoin-channels-alist
        '(("#clojure")))
  (setq erc-keywords '("ska" "kamphausen" "ska2342"))
)

;; Pulse might have been funny for highlight-context-line but it keeps
;; the cursor in the pulsing line, so kick it. sad.
;; (ska-init-message "   Pulse")
;; (try-require 'pulse)
;; Keybinding in ska-ctrl-v-map below

(ska-init-message "   Taskjuggler")
(try-require 'taskjuggler-mode)


(ska-init-message "   Markdown")
(when (try-require 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))
;;;; =================================================================
;;;; Hooks

(ska-init-message "Customizing Hooks")

(defun prog-modes-common-hook-function (&optional no-auto-fill 
                                             no-untabify
                                             no-setnu)
  (unless no-auto-fill (auto-fill-mode 1))
  ;; (unless no-setnu (turn-on-setnu-mode))
  (unless no-untabify
    (add-hook 'write-contents-hooks #'ska-untabify
              nil t)))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Text
(ska-init-message "  Text Mode")
(add-hook 'text-mode-hook
          '(lambda () 
             (auto-fill-mode 1)
             ;;(filladapt-mode)
             ))

;; Emacs Lisp
(ska-init-message "  Elisp")
(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (prog-modes-common-hook-function)
              (turn-on-eldoc-mode)))


;; C/C++

(define-skeleton ska-skel-c-main
  "Insert a typical main function."
  nil
  "int main(int argc, char** argv) {\n"
  > _
  "\n}"
  )

(define-skeleton ska-skel-c-comment
  "Insert comment."
  nil
  "/* "
  > _
  " */"
  )

(define-skeleton ska-skel-c-printf
  "Insert the common printf statement at point."
  nil
  > "printf(\""
  _
  "\");"
  )

(define-skeleton ska-skel-c-printf-flush
  "Insert the common printf statement followed by an fflush at point."
  nil
  > "printf(\""
  _
  "\");fflush(stdout);"
  )

(define-skeleton ska-skel-c-printf-newline
  "Insert a printf statement with newline"
  nil
  "printf(\"\\n\");"
)

(define-skeleton ska-skel-cc-loop-for
  "Insert a for-loop with an int counter variable."
  "Counter variable(int): "
  >"for(int " str "=0;" str "<" _ ";" str "++) {" \n
  \n
  > "}" \n
  )

(define-skeleton ska-skel-c-loop-for
  "Insert a for-loop with an int counter variable."
  "Counter variable(int): "
  >"for(" str "=0;" str "<" _ ";" str "++) {" \n
  \n
  > "}" \n
  )

(define-skeleton ska-skel-c-loop-while
  "Insert a while-loop template."
  > "while(" _ ") {"\n
  \n
  >"}" \n
  )

(define-skeleton ska-skel-c-fflush
  "Insert a fflush of stdout."
  nil
  > "fflush(stdout);"
  )

(define-skeleton ska-skel-c-fprintf
  "Insert a fprintf statement at point asking for the stream."
  "STREAM: "
  > "fprintf(" str ",\""
  _
  "\\n\");"
  )


(define-skeleton ska-skel-c-include
  "Insert a precompiler include statement, asking for what to include.       
You need to give the quotation marks or the angles yourself."
  "include what? "
  > "# include " str
  )


;; C++ Special
(defun ska-skel-cc-endl ()
  "Insert the correct endl-string."
  (interactive)
  (when (not (save-excursion
               (skip-chars-backward " \t\n<")
               (looking-at "\\s-*<")))
    (just-one-space)
    (insert "<<"))
    (just-one-space)
  (insert "endl;\n"))
           
(defun ska-skel-cc-class (name)
  "Insert a C++ class definition.
It creates a matching header file, inserts the class definition and
creates the  most important function templates in a named after the
class name. This might still be somewhat buggy."
  (interactive "sclass name: ")
  (let* ((header-file-name (concat name ".hh"))
         (header-include-string (upcase (concat name "_HH_INCLUDED")))
         (def-file-name    (concat name ".cc")))

    ;; write header file
    (set-buffer (get-buffer-create header-file-name))
    (set-visited-file-name header-file-name)
    (c++-mode)
    (turn-on-font-lock)
    (insert (concat
             "// -*- C++ -*-\n"
             "// File: " header-file-name "\n//\n"
             "// Time-stamp: <>\n"
             "// $Id: $\n//\n"
             "// Copyright (C) "(substring (current-time-string) -4)
             " by " auto-insert-copyright "\n//\n"
             "// Author: " (user-full-name) "\n//\n"
             "// Description: \n// "
             ;; get this point...
             "\n\n" 
             "# ifndef " header-include-string "\n"
             "# define " header-include-string "\n\n"
             "# include <stdio.h>\n\n"
             "# include <stdlib.h>\n\n"
             "# include <string>\n"
             "# include <vector>\n\n"
             "# include <mtrandom>\n\n"
             "class " name ";\n\n"
             "class " name " {\n"
             "public:\n"
             name "();" "\n"
             name "(const " name "& src);\n"
             "~" name "();" "\n"
             name "& operator=(const " name "& rv);\n"
             "\nprivate:\n"
             "void init();\n"
             "void reset();\n"
             "void init_and_copy(const " name "& src);\n\n"
             "protected: \n\n"
             "};"
             "\n\n# endif"))
    (beginning-of-buffer)
    (while (and (not (eobp)) (forward-line))
      (indent-according-to-mode))
    
    ;; create CC file
    (set-buffer (get-buffer-create def-file-name))
    (set-visited-file-name def-file-name)
    (switch-to-buffer (current-buffer))
    (c++-mode)
    (turn-on-font-lock)
    (insert (concat
             "// -*- C++ -*-\n"
             "// File: " def-file-name "\n//\n"
             "// Time-stamp: <>\n"
             "// $Id: $\n//\n"
             "// Copyright (C) "(substring (current-time-string) -4)
             " by " auto-insert-copyright "\n//\n"
             "// Author: " (user-full-name) "\n//\n"
             "// Description: \n//\n\n "
             "# include <stdio.h>\n\n"
             "# include <string>\n"
             "# include <vector>\n\n"
             "# include <mtrandom>\n"
             "\n# include \"" header-file-name "\"\n\n"
             name "::\n" name "() {\ninit();\n}\n\n"
             name "::\n" name "(const " name "& src) {\ninit_and_copy(src);\n}\n\n"
             name "::\n~" name "() {\nreset();\n}\n\n"
             "void\n" name "::\ninit() {\n\n}\n\n"
             "void\n" name "::\nreset() {\n\n}\n\n"
             "void\n" name "::\ninit_and_copy(const " name "& src) {\n\n}\n\n"
             name "&\n" name "::\noperator=(const " name "& src) {\n\n}\n\n"
             ))
    (beginning-of-buffer)
    (while (and (not (eobp)) (forward-line))
      (indent-according-to-mode))
    (beginning-of-buffer)
    (search-forward "Description:")
    )
)


;; This defun is slightly different from the others since it works for
;; two major modes. Therefore the map to use is passed
(defun ska-c-common-mode-keys (map)
  "Set my personal keys for C and C++.
Argument MAP is c-mode-map or c++-mode-map."
  (message "Setting keys for common c mode.")
  (define-key map '[(meta tab)]                       'hippie-expand)
  (define-key map '[(control b) (control a)]          'ska-edit-automake-file)
  (define-key map '[(control b) (control b)]          'compile)
  (define-key map '[(control b) (control f)]          'fume-rescan-buffer)
  (define-key map '[(control b) (control m)]          'manual-entry)
  ;; macros, templates, skeletons:
  (define-key map '[(control b) (m) (f) (f)]          'ska-skel-c-fflush)
  (define-key map '[(control b) (m) (f) (p)]          'ska-skel-c-fprintf)
  (define-key map '[(control b) (m) (i)]              'ska-skel-c-include)
  (define-key map '[(control b) (m) (m)]              'ska-skel-c-main)
  (define-key map '[(control b) (m) (c)]              'ska-skel-c-comment)
  (define-key map '[(control b) (m) (n)]              'ska-skel-c-printf-newline)
  (define-key map '[(control b) (m) (p)]              'ska-skel-c-printf)
  (define-key map '[(control b) (m) (P)]              'ska-skel-c-printf-flush)
  (define-key map '[(control b) (m) (l) (w)]          'ska-skel-c-loop-while)
  )

(defun ska-c++-mode-keys ()
  "Set my personal keys for C++ mode."
  (local-set-key '[(control b) (m) (c)]              'ska-skel-cc-class)
  (local-set-key '[(control b) return]               'ska-skel-cc-endl)
  (local-set-key '[(control b) (m) (l) (f)]          'ska-skel-cc-loop-for)
  )
(defun ska-c-mode-keys ()
  "Set my personal keys for plain C mode."
  (local-set-key '[(control b) (m) (c)]              'ska-skel-c-comment)
  (local-set-key '[(control b) return]               'ska-skel-cc-endl)
  (local-set-key '[(control b) (m) (l) (f)]          'ska-skel-c-loop-for)
  )

(add-hook 'c++-mode-hook
          '(lambda ()
             ;; my keybindings
             (ska-coding-keys c++-mode-map)
             (ska-c-common-mode-keys c++-mode-map)
             (ska-c++-mode-keys)
             ))

(add-hook 'c-mode-hook
          '(lambda ()
             ;; my keybindings
             (ska-coding-keys c-mode-map)
             (ska-c-common-mode-keys c-mode-map)
             (ska-c-mode-keys)
             ))

(add-hook 'c-mode-common-hook
          '(lambda ()
             (auto-fill-mode 1)
             ;; with c-ignore-auto-fill we have indention only in comments :)
             (setq fill-column 70)
             ;; determines which context to show when matching paren off screen
             (setq paren-backwards-message t)
             ;; ctype dynamically scans files for new type definitions
             ;; (ska-safe-require 'ctypes)
             ;; hereby we collect all we've seen
             ;;(setq ctypes-write-types-at-exit t)
             ;; .. and read it
             ;;(ctypes-read-file nil nil t t)
             ;; needed for ever collecting, too:
             ;;(ctypes-auto-parse-mode 1)
             ;; testing the oo-browser from beopen.com
             ;;(load "br-start")
             (setq c-indent-comments-syntactically-p t)
             ;; testing the electric and hungry features...
             ;; (c-toggle-auto-hungry-state t)
             ;; ... no, I don't like em...
             (add-hook 'write-contents-hooks #'ska-untabify
                       nil t)
             (setq indent-tabs-mode nil)
             ))

(add-to-list 'auto-insert-alist
             ;; C Program
             '(("\\.c$" . "C Program")
               nil
               "/* -*- C -*- */\n"
               "/* File: " (file-name-nondirectory buffer-file-name) "\n *\n"
               " * Time-stamp: <>\n"
               " * $Id: $\n *\n"
               " * Copyright (C) "(substring (current-time-string) -4)
               " by " auto-insert-copyright "\n *\n"
               " * Author: "(user-full-name) "\n *\n"
               " * Description: \n * " _ "\n */\n" 
               "# include <stdio.h>\n"
               "# include <stdlib.h>\n\n"
               "# include \""
               (file-name-nondirectory
                (file-name-sans-extension
                 buffer-file-name))
               ".h\"\n"
               ))

(add-to-list 'auto-insert-alist
             ;; C++ Header
             '(("\\.\\([H]\\|hh\\|hpp\\)\\'" . "C++ Header")
               (upcase (concat
                        (file-name-nondirectory
                         (substring buffer-file-name 0 (match-beginning 0)))
                        "_"
                        (substring buffer-file-name
                                   (1+ (match-beginning 0)))
                        "_INCLUDED"))
               "// -*- C++ -*-\n"
               "// File: " (file-name-nondirectory buffer-file-name) "\n//\n"
               "// Time-stamp: <>\n"
               "// $Id: $\n//\n"
               "// Copyright (C) "(substring (current-time-string) -4)
               " by " auto-insert-copyright "\n//\n"
               "// Author: "(user-full-name) "\n//\n"
               "// Description: \n// " _ "\n\n" 
               "# ifndef " str "\n"
               "# define " str "\n\n"
               "\n\n# endif"
               ))
(add-to-list 'auto-insert-alist         ;
             ;; C Header
             '(("\\.h*$" . "C Header")
               (upcase (concat
                        (file-name-nondirectory
                         (substring buffer-file-name 0 (match-beginning 0)))
                        "_"
                        (substring buffer-file-name
                                   (1+ (match-beginning 0)))
                        "_INCLUDED"))
               "/* -*- C -*- */\n"
               "/* File: " (file-name-nondirectory buffer-file-name) "\n *\n"
               " * Time-stamp: <>\n"
               " * $Id: $\n *\n"
               " * Copyright (C) "(substring (current-time-string) -4)
               " by " auto-insert-copyright "\n *\n"
               " * Author: "(user-full-name) "\n *\n"
               " * Description: \n * " _ "\n */\n" 
               "# ifndef " str "\n"
               "# define " str "\n\n"
               "\n\n# endif"
               ))
(add-to-list 'auto-insert-alist
             ;; C++ Program
             '(("\\.\\([C]\\|cc\\|cpp\\)\\'" . "C++ Program")
               nil
               "// -*- C++ -*-\n"
               "// File: " (file-name-nondirectory buffer-file-name) "\n//\n"
               "// Time-stamp: <>\n"
               "// $Id: $\n//\n"
               "// Copyright (C) "(substring (current-time-string) -4)
               " by " auto-insert-copyright "\n//\n"
               "// Author: "(user-full-name) "\n//\n"
               "// Description: \n// " _ "\n//\n\n" 
               "# include \""
               (file-name-nondirectory
                (file-name-sans-extension
                 buffer-file-name))
               ".hh\"\n"
               "# include <stdio.h>\n"
               "# include <stdlib.h>\n"
               "# include <string>\n"
               "# include <vector>\n\n"
               "# include <mtrandom>\n\n"
               ))


;; Python
(ska-init-message "  Python")
(defun ska-python-mode-keys ()
  "Setting local keybindings for major mode: python."
  (local-set-key '[(meta tab)]               'hippie-expand))
(defun ska-python-mode-hook ()
  (interactive)
  (ska-coding-keys python-mode-map)
  (ska-python-mode-keys))

(defun ska-python-make-optparse ()
  "Instrument the current python program to use optparse.
This relies on a certain structure of the code."
  (interactive)
  (save-excursion
    ;; first find a place to insert the import
    (goto-char (point-min))
    (when (and 
           (re-search-forward "^def main(")
           (re-search-backward "^import"))
      (forward-line)
      (insert "from optparse import OptionParser\n"))
    ;; now insert the option parse routine
    (when (re-search-forward "^if __name__ == '__main__':")
      (forward-line)
      (insert "    parse_cmdline_args()\n")
      (re-search-backward "^\n")
      (insert "\ndef parse_cmdline_args():\n"
              "    \"\"\"\n"
              "    Parse command line arguments.\n"
              "    \"\"\"\n"
              "    parser = OptionParser()\n"
              "    parser.add_option(\"--file\", \"-f\", dest=\"filename\",\n"
              "                      help=\"Input file to read data from\")\n"
              "    parser.add_option(\"--version\", action=\"store_true\", dest=\"version\",\n"
              "                      help=\"Print version information.\")\n"
              "    (options, args) = parser.parse_args()\n"
              "    if options.version:\n"
              "        print \"Version: \" + __version__\n"
              "        try:\n"
              "            print \"Filedate: \" + __date__\n"
              "        except:\n"
              "            None\n"
              "        sys.exit(0)\n"
              ""))))

(add-to-list 'auto-insert-alist
             '(("\\.py$" . "Python Program")
               nil
               "#! /usr/bin/python\n\n"
               "\"\"\"" (file-name-nondirectory buffer-file-name) "\"\"\"\n\n"
               "__version__ = \"$Id: $\"\n"
               "__date__ = \"\"\n"
               "__author__ = \"" (user-full-name) "\"\n"
               "__copyright__ = \"" (substring (current-time-string) -4)
               " " auto-insert-copyright "\"\"\"\n\n\n"
               "import os, sys, re, datetime\n"
               "import fileinput\n\n"
               "def main():\n"
               "    \"\"\"main\"\"\"\n\n"
               "if __name__ == '__main__':\n"
               "    try:\n"
               "        main()\n"
               "    finally:\n"
               "        None\n\n"
               "### Local Variables:\n"
               "### time-stamp-start: \"__date__ = \\\"\"\n"
               "### time-stamp-end: \"\\\"\"\n"
               "### time-stamp-format: \"%:y-%02m-%02d\"\n"
               "### End:\n"

               (progn (save-buffer)
                      (shell-command (format "chmod +x %s"
                                             (buffer-file-name)))
                      "")
               ))

;; Bash/Shell scripts
(ska-init-message "  Shell scripts")
(add-to-list 'auto-insert-alist
             '(sh-mode
               nil
               "#!/bin/bash\n#\n"
               "# File: " (file-name-nondirectory buffer-file-name) "\n"
               "# Time-stamp: <>\n"
               "# $Id: $\n#\n"
               "# Copyright (C) " (substring (current-time-string) -4)
               " by " auto-insert-copyright "\n#\n"
               "# Author: "(user-full-name) "\n#\n"
               (progn (save-buffer)
                      (shell-command (format "chmod +x %s"
                                             (buffer-file-name)))
                      "")
               "# Description:\n# " _ "\n"
               ))
;; Ruby
(ska-init-message "  Ruby")
(add-to-list 'auto-insert-alist
             '(("\\.rb$" . "Ruby Program")
               nil
               "#! /usr/bin/ruby\n#\n"
               "# File: " (file-name-nondirectory buffer-file-name) "\n"
               "# Time-stamp: <>\n"
               "# $Id: $\n#\n"
               "# Copyright (C) " (substring (current-time-string) -4)
               " by " auto-insert-copyright "\n#\n"
               "# Author: "(user-full-name) "\n#\n"
               (progn (save-buffer)
                      (shell-command (format "chmod +x %s"
                                             (buffer-file-name)))
                      "")
               "# Description:\n# " _ "\n"
                 ))

;; Perl
(ska-init-message "  CPerl")
(defun ska-cperl-mode-keys ()
  "Setting local keybindings for major mode: perl."
  (local-set-key '[(meta tab)]               'hippie-expand)
  (local-set-key '[(control b) (control b)]  'executable-interpret)
  ;; skeletons and makros MIGHT use C-b C-s as prefix if they get too many
  (local-set-key '[(control b) (control f)]  'ska-skel-perl-file-loop)
  (local-set-key '[(control b) (control d)]  'ska-skel-perl-dbi-connect)
  (local-set-key '[(control b) (control s)]  'ska-skel-perl-sub)
  (local-set-key '[(control b) (control i)]  'ska-skel-perl-prog-id)
  (local-set-key '[(control b) (control o)]  'ska-skel-perl-options)
  (local-set-key '[(control b) (control h)]  'cperl-perldoc-at-point)
  (local-set-key '[(control b) (control v)]  'ska-skel-perl-svn-version)
  )

(defalias 'perl-mode 'cperl-mode)
(defun ska-cperl-mode-hook ()
  (interactive)
  ;; my keybindings
  (ska-coding-keys cperl-mode-map)
  (ska-cperl-mode-keys)
  (auto-fill-mode 1)
  (turn-on-font-lock)
  ;; if you don't want tabs
  (setq indent-tabs-mode nil)
  (setq cperl-indent-level 4)
  ;; full featured mode:
  ;;(setq cperl-hairy t)
  ;; alternatively:
  ;;(setq cperl-auto-newline-after-colon t)
  ;;(setq cperl-electric-parens "({[")
  (setq cperl-auto-newline nil)
  (setq cperl-electric-linefeed nil)
  (setq cperl-electric-parens nil)
  (setq cperl-electric-lbrace-space t)
  (setq cperl-electric-keywords nil)
  (setq cperl-mode-abbrev-table nil)
  ;; turn on autohelp
  (setq cperl-lazy-help-time 1)
  (cperl-lazy-install)
  (setq cperl-pod-here-fontify nil)
  (setq cperl-highlight-variables-indiscriminately t)
  (setq Manual-buffer-view-mode 1)      ; for a separate buffer
  (prog-modes-common-hook-function)
  )

(define-skeleton ska-skel-perl-svn-version
  "Insert some perl code to have automatic SVN versioning."
  nil
  \n "my $svn_id = '$Id:  $';"
  \n "my $Version = $svn_id;"
  \n "if ($Version =~ m/Id:\s*(.*?)\s*\$$/) {"
  \n "$Version = $1;"
  \n "}" '(progn (indent-according-to-mode) nil)
  \n)

(define-skeleton ska-skel-perl-dbi-connect 
  "Insert a standard DBI connection string."
  nil
  \n "my $dbh = DBI->connect('DBI:mysql:database=eddy3;host=localhost',"
  \n "'eddy',"
  \n "'edifact',"
  \n "{"
  \n "PrintError => 1,"
  \n "RaiseError => 1"
  \n "});"
  \n)

(define-skeleton ska-skel-perl-file-loop
  "Insert a typical perl file loop construct."
  "Handle-Variable name: "
  "my $" str " = new IO::File(\""
  (let ((fname (read-string "Filename-expression: ")))
    (concat fname "\") or die \"Couldn't open " fname ": $!\\n\";"))
  \n "while(my $line = <$" str ">) {"
  \n "chomp($line);"
  \n "next if $line =~ m/^\\s*$/;"
  \n "next if $line =~ m/^\\#/;"
  "\n"
  \n _
  \n "}" '(progn (indent-according-to-mode) nil)
  \n "$" str "->close();"
  \n)

(define-skeleton ska-skel-perl-sub-unused
  "Insert a perl subroutine with arguments."
  "Subroutine name: "
  "sub " str " {"
  \n "my (" ("Argument name: " "$" str ", ") -2 ") = @_;"
  "\n"
  \n _
  \n "}" '(progn (indent-according-to-mode) nil)
  \n)

(define-skeleton ska-skel-perl-sub
  "Insert a perl subroutine stub."
  nil
  "sub " _ "{"
  \n "my () = @_;"
  \n 
  \n "}" '(progn (indent-according-to-mode) nil)
  \n)

;; any other way to get a time format string?
(try-require 'time-stamp)

(define-skeleton ska-skel-perl-prog-id
  "Insert perl program identification."
  (nil)
  (insert-char ?# 60) \n
  "#" (insert-char ? 18) "PROGRAMM IDENTIFICATION" (insert-char ? 17)
  "#"\n
  (insert-char ?# 60) \n
  "my $program = \"" (file-name-nondirectory
                      (file-name-sans-extension buffer-file-name))
                      "\";"\n
  "my $filedate=\"" (time-stamp-strftime "%y-%m-%d") "\";"\n
  "my $fileversion=\"0.01\";"\n
  "my $copyright = \"Copyright (C) " (substring (current-time-string) -4)
  " by Stefan Kamphausen\";"\n
  "my $title = \"$program $fileversion, $filedate - $copyright\\n\";"
)

(define-skeleton ska-skel-perl-options
  "Insert perl program getopt stuff."
  (nil)
  (save-excursion
    (if (re-search-backward "^use" (beginning-of-buffer) t)
        (progn (end-of-line)
               (newline-and-indent))
      (progn (beginning-of-buffer)
             (while (string-match "^#" (char-to-string (char-after)))
               (forward-line))))
    (insert "use Getopt::Long;") 
    (indent-according-to-mode)
    (insert "\n")
    nil)
  (insert-char ?# 60) \n
  "#" (insert-char ? 26) "OPTIONS" (insert-char ? 25)
  "#"\n
  (insert-char ?# 60) \n
  "my $ret = GetOptions("\n
  "\"help|h!\" => \\my $help,"\n
  "\"version|v!\" => \\my $version"\n
  ");"\n
) 

(define-skeleton ska-skel-perl-project
  "Insert much perl code, preparing a real world project."
  (nil)
  "use Getopt::Long;\n"
  "use Pod::Usage;\n"
  "\nmy $Version = \"1.0\";\n"
  "######################################################################\n"
  "##                             OPTIONS\n"
  "######################################################################\n"
  "GetOptions("
  \n "\"help|h!\"    => \\my $help,"
  \n "\"longhelp!\"  => \\my $longhelp,"
  \n "\"version|v!\" => \\my $version"
  \n ") or pod2usage("
  \n "verbose    => 0,"
  \n "exitstatus => 1"
  \n ");"
  \n "if ($help) {"
  \n "pod2usage("
  \n "verbose    => 1,"
  \n "exitstatus => 0"
  \n ");\n"
  "}"
  \n "if ($longhelp) {"
  \n "pod2usage("
  \n "verbose    => 2,"
  \n "exitstatus => 0"
  \n ");\n"
  "}"
  \n "if ($version) {"
  \n "print $Version;"
  \n "exit 0;\n"
  "}"
  \n "######################################################################"
  \n "##                               MAIN"
  \n "######################################################################"
  \n ""
  \n "######################################################################"
  \n "##                               SUBS"
  \n "######################################################################"
  \n "__END__\n"
  "######################################################################\n"
  "##                             Now Docs...\n"
  "######################################################################\n"
  "=head1 NAME"
  "\n"
  \n (file-name-nondirectory buffer-file-name) " - DESCRIBE ME"
  "\n\n"
  "=head1 SYNOPSIS"
  "\n"
  \n (file-name-nondirectory buffer-file-name) " [-h] [-v]" 
  "\n\n"
  "=head1 OPTIONS"
  "\n\n"
  "=over"
  "\n\n"
  "=item B<-h, --help>"
  "\n\n"
  "Print help message and exit successfully."
  "\n\n"
  "=item B<--longhelp>"
  "\n\n"
  "Print program documentation."
  "\n\n"
  "=item B<-v, --version>"
  "\n\n"
  "Print version information and exit successfully."
  "\n\n"
  "=back"
  "\n\n"
  "=cut\n"
  "")

(add-to-list 'auto-insert-alist
             '(("\\.pl$" . "Perl Program")
               nil
               "#! /usr/bin/perl\n#\n"
               "# File: " (file-name-nondirectory buffer-file-name) "\n"
               "# Time-stamp: <>\n"
               "# $Id: $\n#\n"
               "# Copyright (C) " (substring (current-time-string) -4)
               " by " auto-insert-copyright "\n#\n"
               "# Author: "(user-full-name) "\n#\n"
               (progn (save-buffer)
                      (shell-command (format "chmod +x %s"
                                             (buffer-file-name)))
                      "")
               "# Description:\n# " _ "\n"
               "use strict;\n"
               "use warnings;\n"
               "use Data::Dumper;\n"
               "use IO::File;\n"
               "use Carp;\n\n"
               (when (yes-or-no-p "Is this a real project (or just a script)? ")
                 (ska-skel-perl-project)
                 ""
                 )))

(add-to-list 'auto-insert-alist
             '(("\\.pm$" . "Perl Module")
               nil
               "# File: " (file-name-nondirectory buffer-file-name) "\n"
               "# Time-stamp: <>\n"
               "# $Id: $\n#\n"
               "# Copyright (C) " (substring (current-time-string) -4)
               " by " auto-insert-copyright "\n#\n"
               "# Author: "(user-full-name) "\n#\n"
               "# Description:\n# " _ "\n"
               "package " (file-name-sans-extension (file-name-nondirectory buffer-file-name)) ";\n"
               "use strict;\n"
               "use warnings;\n"
               "use Carp;\n"
               "use IO::File;\n"
               "use Data::Dumper;\n\n"
               "sub new {\n"
               > "my $proto = shift;\n"
               > "my $class = ref($proto) || $proto;\n"
               > "my $self  = {};\n\n"
               > "bless ($self, $class);\n\n"
               > "return $self;\n"
               "}\n\n\n1;\n"
               ))

(add-hook 'cperl-mode-hook 'ska-cperl-mode-hook)

;; Help Mode
(ska-init-message "  Help Mode")
(add-hook 'help-mode-hook 
          #'(lambda ()
              (local-set-key (kbd "f") #'find-function-at-point)))

;; Smartparens, successor to paredit
;; ... give it another shot
(when (try-require 'smartparens)
  (smartparens-global-mode 1))

;; Common Lisp
(ska-init-message "  Common Lisp")
(setq inferior-lisp-program "sbcl")
(setq common-lisp-hyperspec-root "/usr/share/doc/hyperspec/")


(when (try-require 'slime)
  (slime-setup '(inferior-slime
                 slime-fancy))
  (load "cl-indent")

  ;;(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
  (setq slime-conservative-indentation nil))

(defun ska-lisp-mode-keys ()
  "Set keys for programming CL."
  (local-set-key '[(control b) (control b)]  'slime-selector))


(defun ska-skel-cl-make-system (sysname)
  (interactive "sSystem Name: ")
  ;; make sure we live in a reasonable directory
  (let* ((cpath (directory-file-name default-directory))
         (cwd (file-name-nondirectory cpath)))
    (unless (string= sysname cwd)
      (if (y-or-n-p
           (format "%s is not the current directory. Create it in %s? "
                   sysname cpath))
        (progn 
          (make-directory sysname)
          (cd sysname))
        (cd (read-directory-name "Choose system directory: ")))))

  ;; now create files
  (cl-flet ((create-lisp-file (name content)
           (with-temp-buffer
             (let ((author (user-full-name))
                   (copyright (or (and (boundp 'my-copyright-holder)
                                       my-copyright-holder)
                                  (user-full-name)))
                   (year (substring (current-time-string) -4)))
               (insert
                ";;; -*- Mode: LISP; -*-\n"
                "\n"
                ";;; Time-stamp: <>\n"
                ";;; $Id: $\n;;;\n"
                ";;; Copyright (C) " year " by " copyright "\n;;;\n"
                ";;; Author: " author "\n;;;\n\n"))
             (insert content)
             (unless (file-writable-p name)
               (error "%s is not writeable" name)) 
             (when (or (not (file-exists-p name))
                       (y-or-n-p (concat name " exists. Overwrite? ")))
               (write-region (point-min) (point-max) name)))))
    (create-lisp-file (concat sysname ".asd")
                      (concat "\n(in-package :cl-user)\n\n"
                              "(defpackage :" sysname ".system\n"
                              "  (:use :cl :asdf))\n\n"
                              "(in-package :" sysname ".system)\n\n"
                              "(defsystem :" sysname "\n"
                              "  :serial t\n"
                              "  :components ((:file \"packages\")\n"
                              "               (:file \"specials\")\n"
                              "               (:file \"" sysname "\"))\n"
                              "  :depends-on ())\n"))
    (create-lisp-file "packages.lisp"
                      (concat
                       "(in-package :cl-user)\n\n"
                       "(defpackage :" sysname "\n"
                       "  (:use :cl)\n"
                       "  (:export ))\n\n"))
    (create-lisp-file "specials.lisp"
                      (concat "(in-package :" sysname ")\n"))
    (create-lisp-file (concat sysname ".lisp")
                      (concat "(in-package :" sysname ")\n")))
  (find-file (concat sysname ".lisp"))
  (goto-char (point-max)))


(setq auto-mode-alist
      (append
       '(("\\.lsp$" . lisp-mode)
         ("\\.asd$" . lisp-mode)
         ("\\.cl$" . lisp-mode)
         ("\\.system$" . lisp-mode))
       auto-mode-alist))

(add-hook 'lisp-mode-hook
          (lambda ()
            (prog-modes-common-hook-function)
            (ska-lisp-mode-keys)
            (slime-mode t)
            (setq fill-column 70)
            (speedbar-add-supported-extension ".lisp")
            (setq indent-tabs-mode nil)
            (set (make-local-variable 'lisp-indent-function)
                 'common-lisp-indent-function)
            ))

(add-hook 'inferior-lisp-mode-hook
          (lambda ()
            (inferior-slime-mode t)))

(add-hook 'slime-repl-mode-hook
          (lambda ()
            (slime-repl-read-history)
            ;(ska-coding-keys shared-lisp-mode-map)
            (ska-lisp-mode-keys)))

;; To use other Lisps...
;; Incidentally, you can then choose different Lisps with
;;   M-- M-x slime <tab>
(when (try-require 'slime)
  (add-to-list 'slime-lisp-implementations
               '(sbcl   ("sbcl"))))


(ska-init-message "  Rainbow Delimiters")
(when (try-require 'rainbow-delimiters)
  ;; Only use them to highlight superfluous closing parens
  ;; http://timothypratley.blogspot.nl/2015/07/seven-specialty-emacs-settings-with-big.html
  (setq rainbow-delimiters-max-face-count 1)
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error)
  (global-rainbow-delimiters-mode))

(ska-init-message "  Clojure")
(defun ska-clojure-keys ()
  "Setting local keybindings for CIDER and Clojure."
  (interactive)
  (local-set-key '[(control b) (control p)] 'cider-present-last-sexp))

(when (and (try-require 'cider)
           (try-require 'clojure-mode))

  (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
  (add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))

  (setq nrepl-hide-special-buffers t)
  (setq cider-repl-tab-command 'indent-for-tab-command)
  (setq cider-repl-history-file "~/.emacs.d/cider-history.eld")
  (setq cider-auto-select-error-buffer t)
  (setq cider-repl-display-help-banner nil)
  (try-require 'company)

  (add-hook 'clojure-mode-hook
            (lambda ()
              (prog-modes-common-hook-function)
              (setq fill-column 70)
              (setq indent-tabs-mode nil)
              (key-chord-define-local "jj" "(")
              (key-chord-define-local "kk" ")")
              (subword-mode)
              ))
  (add-hook 'cider-mode-hook 
            (lambda ()
              (eldoc-mode)
              (company-mode)
              (ska-clojure-keys)))
  (add-hook 'cider-repl-mode-hook 
            (lambda ()
              (eldoc-mode)
              (company-mode)
              (key-chord-define-local "jj" "(")
              (key-chord-define-local "kk" ")")
              (setq cider-repl-use-clojure-font-lock t)
              (subword-mode)))

  (defun cider-present-last-sexp ()
    (interactive)
    (if text-scale-mode
        (let ((text-scale text-scale-mode-amount))
          (cider-pprint-eval-last-sexp)
          (with-current-buffer (get-buffer-create cider-result-buffer)
            (text-scale-set text-scale)))
      (cider-pprint-eval-last-sexp))))


  ;; (defun cljlayout ()
  ;; (interactive)
  ;; (delete-other-windows)
  ;; (split-window-horizontally)
  ;; (split-window-vertically)
  ;; (other-window 2)
  ;; (split-window-vertically)
  ;; (split-window-vertically)
  ;; (set-window-buffer (frame-selected-window) 
  ;;                    (get-buffer-create nrepl-error-buffer))
  ;; (other-window 1)
  ;; (set-window-buffer (frame-selected-window) 
  ;;                    (get-buffer-create nrepl-doc-buffer))
  ;; (other-window 2)
  ;; (when (nrepl-current-connection-buffer)
  ;;   (save-excursion
  ;;     (other-window 1)
  ;;     (set-window-buffer (frame-selected-window)
  ;;                        (nrepl-repl-buffer)))))
  ;; )
  
  
;; TeX, LaTeX
(ska-init-message "  LaTeX")
;;(try-require 'tex-site)
(setq TeX-parse-self t) ; Enable parse on load.
(setq TeX-auto-save t) ; Enable parse on save.
(setq-default TeX-master nil)
(setq reftex-plug-into-AUCTeX t)

(defun ska-skel-latex-bn ()
  (interactive)
  (let ((title (read-string "Project Title: "))
        (subject (read-string "Subject: "))
        (final-position))
    (insert "\\documentclass[11pt,a4paper,oneside,BCOR0mm,DIV14,titlepage,headsepline,footsepline,parskip]{scrartcl}

\\usepackage{bnstyle}
\\usepackage{ngerman}\n\n")

    (insert (concat "\\title{"
                    title
                    "}\n"))
    (insert (concat "\\subject{"
                    subject
                    "}\n"))
    (insert "\\author{Stefan Kamphausen}

\\begin{document}
\\bndocstart\n\n")
    (setq final-position (point))
    (insert "\n\\end{document}")
    (goto-char final-position)))


(defun ska-skel-latex-whitepaper ()
  (interactive)
  (let ((title (read-string "Project Title: "))
        (subject "Whitepaper\\\\Version 0.0\\\\ -- confidential --")
        (final-position))
    (insert "\\documentclass[11pt,a4paper,oneside,BCOR0mm,DIV12,titlepage,headsepline,footsepline,parskip]{scrartcl}\n\n")
    
    (insert (concat "\\title{" title "}\n"))
    (insert (concat "\\subject{" subject "}\n"))
    (insert "\\author{Stefan Kamphausen}

\\usepackage{cgwp}
% \\usepackage{ngerman}

\\begin{document}
\\cgdocstart

\\section{Management Summary}
\\label{sec:management-summary}
")
    (setq final-position (point))
    (insert "

\\subsection{Short description of project}
\\label{sec:management_summary_descr}

\\section{Vision}
\\label{sec:vision}

\\section{Theory}
\\label{sec:theory}

\\section{Description of Application}
\\label{sec:application}

\\subsection{User Interfaces}
\\label{sec:application_ui}


\\subsection{Data Structures and Application Logic}
\\label{sec:application_data}

\\end{document}

%%% Local Variables: 
%%% mode: latex
%%% TeX-master: t
%%% End: 
")
    (goto-char final-position)))

(defun ska-skel-latex-plan ()
  (interactive)
  (ska-skel-latex-bn)
    (insert "
\\section{Ziele -- Warum dieses Projekt?}

\\section{Vision des erreichten Ziels}

\\section{Brainstorming}

\\begin{itemize}
\\item
\\end{itemize}

\\section{Organisation}

\\section{Nächste Schritte}
"))



(defun ska-latex-mode-keys ()
  "Set keys for latex-mode (AUCTeX)."
  (local-set-key '[(tab)]                    'LaTeX-indent-line)
;  (define-key LaTeX-math-keymap
;    (concat LaTeX-math-abbrev-prefix "/")    'LaTeX-math-frac)
)
(defun ska-bibtex-mode-keys ()
  "Set keys for bibtex-mode."
  (local-set-key '[(control return)]         'bibtex-next-field)
)

(add-hook 'LaTeX-mode-hook
          '(lambda ()
             ;; meine keybindings
             ;; (ska-coding-keys TeX-mode-map)
             (ska-latex-mode-keys)
             ;; (fume-add-menubar-entry)
             ;; (setq fume-display-in-modeline-p nil)
             (auto-fill-mode 1)
             ;(setq ispell-parser 'tex)
             (LaTeX-math-mode)
             (turn-on-reftex)
;             (when (string-match "diary" buffer-file-name)
;               (turn-on-tex-diary))
             ))

(add-hook 'TeX-language-de-hook
          '(lambda ()
             ;;(ispell-change-dictionary "deutsch")
             ))

 (add-hook 'BibTeX-mode-hook
          '(lambda ()
             (ska-bibtex-mode-keys)
             ))

;; Java
;; (ska-init-message "  Java")
;; (defun ska-jde-mode-keys ()
;;   "Setting local keybindings for major mode: JDE."
;;   (local-set-key '[(control b) (control b)]      'jde-build)
;;   (local-set-key '[(control b) (control h)]      'jde-help-symbol)
;;   (local-set-key '[(control b) (control space)]  'jde-complete-in-line)
;;   (local-set-key '[(control b) (space)]          'jde-complete)
;;   (local-set-key '[(control b) (control r)]      'jde-run)
;;   )

;; (setq defer-loading-jde t)

;; (if defer-loading-jde
;;     (progn
;;       (autoload 'jde-mode "jde" "JDE mode." t)
;;       (setq auto-mode-alist
;;         (append
;;          '(("\\.java\\'" . jde-mode))
;;          auto-mode-alist)))
;;   (try-require 'jde))

;; ;; Note that the customization for JDE is done via customize, because
;; ;; JDE wants it that way
;; (defun my-jde-mode-hook ()
;;   ;;(turn-on-setnu-mode)
;;   (turn-on-font-lock)
;;   (auto-fill-mode 1)
;;   (setq indent-tabs-mode nil)

;;   (setq c-basic-offset 2)

;;   (ska-coding-keys jde-mode-map)
;;   (ska-jde-mode-keys)
;;   (add-hook 'write-contents-hooks #'ska-untabify
;;             nil t))

;; (add-hook 'jde-mode-hook 'my-jde-mode-hook)


;; (add-to-list 'auto-insert-alist
;;              '(("\\.java" . "Java Basic")
;;                nil
;;                "/**\n"
;;                " * File: " (file-name-nondirectory buffer-file-name) "\n *\n"
;;                " * Time-stamp: <>\n"
;;                " * $Id: $\n *\n"
;;                " * Copyright (C) "(substring (current-time-string) -4)
;;                " by " auto-insert-copyright "\n *\n"
;;                " * @author "(user-full-name) "\n"
;;                " * @version 1.0\n"
;;                " */\n\n"
;;                "import java.io.*;" \n
;;                "import java.util.*;" \n
;;                \n
;;                ))

;; Linmag-Mode
(ska-init-message "   LinMagMode")
(when (try-require 'linmag-mode)
  (setq linmag-compile-programm "sf")
  (add-hook 'linmag-mode-hook
            '(lambda ()
               (ispell-change-dictionary "deutsch")
               (flyspell-mode 1)
               (auto-fill-mode 1))))

;; XML
(ska-init-message "  nXML Mode")
(defun ska-nxml-mode-keys ()
  "Set keys for nXML mode."
  ;; I'm so used to that from ol' psgml
  (local-set-key '[(control c) (/)]  'nxml-finish-element)
  ;; fuck for flyspell messing with C-, and C-.!
  (local-set-key '[(control c) (control \.)]  'ska-point-to-register)
  (local-set-key '[(control c) (control \,)]  'ska-jump-to-register)
)

(add-hook 'nxml-mode-hook
          #'(lambda ()
              (setq nxml-sexp-element-flag t)
              (auto-fill-mode 1)
              (setq fill-column 80)
              ;;(filladapt-mode)
              (setq nxml-bind-meta-tab-to-complete-flag t)
              (setq nxml-sexp-element-flag t)
              (setq nxml-slash-auto-complete-flag t)
              ;(flyspell-mode 1)
              (ska-nxml-mode-keys)
              ))

;; HTML
(ska-init-message "  HTML")

(add-hook 'html-mode-hook
          '(lambda ()
             (auto-fill-mode 1)
             ;;(setq sgml-insert-missing-element-comment nil)
             ;;(setq sgml-set-face nil)
             ;;(setq sgml-markup-faces nil)
             (setq fill-column 100)
             ;;(setq html-helper-use-expert-menu t)
             ;;(setq html-helper-do-write-file-hooks t)
             ;;(add-to-list 'sgml-catalog-files
             ;;             (concat sgml-data-directory "/CATALOG"))
             ))

;;;; =================================================================
;;;; MISC SETTINGS
(ska-init-message "Misc Settings")

(fset 'yes-or-no-p 'y-or-n-p)
(setq eshell-ask-to-save-history 'always)
(setq track-eol t)
(setq inhibit-splash-screen t)
;; to avoid a large memory footprint:
(setq message-log-max 1000)
(global-font-lock-mode t)
(setq-default transient-mark-mode t)
(setq require-final-newline nil)
(setq next-line-add-newlines nil)
(setq apropos-do-all t)
;;(setq setnu-line-number-format "%4d ")
;;(setq setnu-line-number-face (copy-face 'default 'setnu-my-face))
;;(set-face-background 'setnu-my-face "#bfbfbf")
;;(set-face-foreground 'setnu-my-face "#777777")
(setq column-number-mode t)
;;(setq browse-url-netscape-program "firefox")
(setq browse-url-browser-function 
      '(("hyperspec" . w3m-browse-url)
        ("." . browse-url-default-browser)))

;; some commands are disabled; novice.el
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
;;;; =================================================================
;;;; KEYS

;; first my own prefixes
(defvar ska-ctrl-v-map nil "My global keybindings")
(define-prefix-command 'ska-ctrl-v-map)
(global-set-key '[(control v)] 'ska-ctrl-v-map)
(define-key ska-ctrl-v-map '[(control a)] 'org-agenda)
(define-key ska-ctrl-v-map '[(a)] 'align)
(define-key ska-ctrl-v-map '[(f)]         'find-function)
(define-key ska-ctrl-v-map '[(control f)] 'ffap)
(define-key ska-ctrl-v-map '[(control g)] 'magit-status)
(define-key ska-ctrl-v-map '[(g)]         'fume-prompt-function-goto)
(define-key ska-ctrl-v-map '[(i)]         'imenu)
(define-key ska-ctrl-v-map '[(control l)] 'locate)
(define-key ska-ctrl-v-map '[(control o)] 'occur)
(define-key ska-ctrl-v-map '[(control r)] 'remember)
(define-key ska-ctrl-v-map '[(control s)] 'svn-status)
(define-key ska-ctrl-v-map '[(control t)] 'fdlcap-change-case-current-word)
(define-key ska-ctrl-v-map '[(control v)] 'll-debug-toggle-comment-region-or-line)
;(define-key ska-ctrl-v-map '[(control x)] 'ska-anything)
(define-key ska-ctrl-v-map '[(control y)] 'ska-insert-x-selection)

(global-set-key '[(control z)]            'yank)
(global-set-key '[(control up)]           '(lambda ()
                                              (interactive)
                                              (scroll-down 1)))
(global-set-key '[(control down)]         '(lambda ()
                                             (interactive)
                                             (scroll-up 1)))
(global-set-key '[(control \.)]           'ska-point-to-register)
(global-set-key '[(control \,)]           'ska-jump-to-register)
(global-set-key '[(control \')]           'point-to-register)
(global-set-key '[(meta \')]              'register-to-point)
(global-set-key '[(control backspace)]    'backward-kill-word)
(global-set-key '[(control delete)]       'kill-word)
(global-set-key '[(control tab)]          '(lambda ()
                                             (interactive)
                                             (other-window 1)))
;; fallback..
(global-set-key '[(pause)]                '(lambda ()
                                             (interactive)
                                             (other-window 1)))
(global-set-key '[(control pause)]        '(lambda ()
                                             (interactive)
                                             (other-window -1)))
(global-set-key '[(f1)]                   'kmacro-end-or-call-macro)
(global-set-key '[(control f1)]           'kmacro-start-macro-or-insert-counter)

(global-set-key '[(f3)]                   '(lambda ()
                                             (interactive)
                                             (term "/bin/bash")))
(global-set-key '[(control f3)]           'eshell)
(global-set-key '[(f4)]                   'kill-this-buffer)
(global-set-key '[(control f4)]           'ska-kill-this-window)
(global-set-key '[(f5)]                   'delete-other-windows)
(global-set-key '[(control f5)]           'delete-window)
(global-set-key '[f6]                     'split-window-vertically)
(global-set-key '[(control f6)]           'split-window-horizontally)
;; remove these in favour of windresize
;; (global-set-key '[f7]                     'shrink-window)
;; (global-set-key '[(control f7)]           'shrink-window-horizontally)
;; (global-set-key '[f8]                     'enlarge-window)
;; (global-set-key '[(control f8)]           'enlarge-window-horizontally)
(global-set-key '[f9]                     'repeat-complex-command)
;;(global-set-key '[(control f9)]            ')
(global-set-key (kbd "S-SPC")             'dabbrev-expand)
;(global-set-key (kbd "C-S-SPC")           'helm-mini)
(global-set-key '[(shift iso-lefttab)]    'comint-dynamic-complete)
(global-set-key '[(backtab)]              'comint-dynamic-complete)
(global-set-key '[(meta k)]               'ska-kill-entire-line)
(global-set-key '[(home)]                 'chb-home)
(global-set-key '[(end)]                  'chb-end)

;; some xemacs defaults:
(global-set-key (kbd "<M-left>") #'backward-sexp)
(global-set-key (kbd "<M-right>") #'forward-sexp)
(global-set-key (kbd "M-g") #'goto-line)

(global-set-key (kbd "C-h a") #'apropos)


(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

;; Default seince Emacs 22
;; (global-set-key (kbd "C-M-<SPC>")
;;                 #'(lambda ()
;;                     (interactive)
;;                     (if (not (and transient-mark-mode mark-active))
;;                         (mark-sexp)
;;                       (let ((oldpoint (point)))
;;                         (goto-char (mark t))
;;                         (set-mark (save-excursion
;;                                     (forward-sexp 1)
;;                                     (point)))
;;                         (goto-char oldpoint)))))


;; taken from http://jaderholm.com/configs/emacs will give it a try
;; run occur with M-o when in C-s 
(defvar ska-isearch-occur-opened nil)
(defvar ska-isearch-window-configuration nil)

(defun ska-isearch-occur ()
  (interactive)
  (when (fboundp 'occur)
    (setq ska-isearch-occur-opened t)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

(defun ska-isearch-maybe-remove-occur-buffer ()
  "Restore window-configuration when quitting isearch.

This function is meant to be used together with a function storing the
window configuration into a variable and together with a setup opening
the occur buffer from within isearch.

This function ...

-  will do nothing if you you did not cancel the search,

- will kill the occur buffer if occur buffer was opened from
  isearch,

- will restore your old window configuration when you saved it in
  `isearch-mode-hook'."

  (interactive)
  (let ((occ-buffer (get-buffer "*Occur*")))
    (when (and ska-isearch-occur-opened
               isearch-mode-end-hook-quit
               (buffer-live-p occ-buffer))
      (kill-buffer occ-buffer)
      (when (and ska-isearch-window-configuration
                 (window-configuration-p (car ska-isearch-window-configuration)))
        (set-window-configuration (car ska-isearch-window-configuration))
        (goto-char (cadr ska-isearch-window-configuration))))))

(add-hook 'isearch-mode-hook 
          '(lambda ()
             (setq ska-isearch-window-configuration
                   (list (current-window-configuration) (point-marker)))))

(add-hook 'isearch-mode-end-hook 
          '(lambda ()
             (ska-isearch-maybe-remove-occur-buffer)
             (setq ska-isearch-occur-opened nil)))

(define-key isearch-mode-map (kbd "M-o") 'ska-isearch-occur)

;; We extened it a bit to remove the occur buffer when *quitting* the
;; search.
;; (defun ska-isearch-maybe-remove-occur-buffer ()
;;   "Remove the occur buffer when quiting isearch.
;; Removes the window, too.  Problem is: it also removes the window when
;; you had a split-window view earlier."
;;  (interactive)
;;  (let ((occ-buffer (get-buffer "*Occur*")))
;;    (let ((occ-window (get-window-with-predicate
;;             (lambda (window)
;;               (equal (window-buffer window) occ-buffer)) nil t)))
;;      (when (and isearch-mode-end-hook-quit
;;         (buffer-live-p occ-buffer))
;;        (delete-window occ-window)
;;        (kill-buffer occ-buffer)))))

;; (defun ska-isearch-maybe-remove-occur-buffer ()
;;   "Simple approach to remove the occur buffer when quitting isearch.
;; It will not restore your window configuration, though."
;;   (interactive)
;;   (let ((occ-buffer (get-buffer "*Occur*")))
;;     (when (and isearch-mode-end-hook-quit
;;                (buffer-live-p occ-buffer))
;;       (kill-buffer occ-buffer))))



;;;; =================================================================
;;;; More variables

(ska-init-message "End of init.el")
(put 'upcase-region 'disabled nil)
