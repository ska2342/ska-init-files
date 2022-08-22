;; Init for GNU Emacs
;;; (C) 2004-2020 Stefan Kamphausen www.skamphausen.de

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


(defvar ska-init-verbose nil)

(when ska-init-verbose
  (setq debug-on-error nil)
  (setq use-package-verbose nil))

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

(ska-init-message "Start of init.el")

(add-to-list 'exec-path "~/local/bin")

;; Some manually maintained libraries. Most of them moved to
;; use-package and installation by (m)elpa in 2017.
(setq load-path
     (append
      (list
       "~/.emacs.d/lisp"
       )
      load-path))

;; Bootstrap `use-package'
(setq package-enable-at-startup nil)
;; Just expect this to be available these days.
(require 'package)

(dolist (pkg '(("melpa-stable" . "https://stable.melpa.org/packages/")
               ("melpa-unstable" . "https://melpa.org/packages/")))
  (add-to-list 'package-archives pkg))

;; package usually loads packages after init.el; this breaks my way of
;; configuring things here.  See
;; e.g. http://www.emacswiki.org/emacs/ELPA
;; Thus, I initialize explicitly:
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Ideally, I do not need a custom file anymore.
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))


;; User Specific
(setq ska-user-init-file "~/.emacs.d/user.el")
;; user mail adress copied from startup.el which is not yet loaded at
;; this time.
(when (not (file-exists-p ska-user-init-file))
  (message "Missing user.el. Creating it.")
  (with-temp-buffer
    (insert
     ";;; user.el --- my specific settings to keep init.el portable

;;; Commentary:

;; User-specific settings so that to take your kinda secret data. 
;; Usually, you do not want to share this file.

;;; Code:

\(setq user-mail-address \""
     (completing-read
      "Enter your email adress to seed new ~/.emacs.d/user.el: "
      nil)
     "\")
\(setq my-copyright-holder \""
     (completing-read
      "Enter your full name for copyrights: "
      (list (user-full-name))) "\")

\(provide 'user)
;;; user.el ends here
")
    (write-region (point-min) (point-max) ska-user-init-file)))
(load ska-user-init-file)

;;; Local, not in version control
(setq ska-local-init-file "~/.emacs.d/local.el")
(when (file-exists-p ska-local-init-file)
  (load ska-local-init-file))

;; I have my own global keymaps. C-v corresponds to C-x and C-b to
;; C-c. C-v is my own global bindings, while C-b is for mode-specific
;; bindings using the appropriate mode-map.
(defvar ska-ctrl-v-map nil "My global keybindings")
(define-prefix-command 'ska-ctrl-v-map)
(global-set-key '[(control v)] 'ska-ctrl-v-map)

;; Experimental
(if (and (>= emacs-major-version 27)
         (featurep 'cairo))
    (set-fontset-font t '(#x1f000 . #x1faff)
                      (font-spec :family "Noto Color Emoji")))

;;; Functions
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
          (lambda () 
             (make-local-hook 'write-contents-hooks) 
             (add-hook 'write-contents-hooks #'ska-untabify nil t)))"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "\t" nil t)
      (untabify (1- (point)) (point-max)))
    nil))

(defun ska-current-buffer-is-tramp-file ()
  (interactive)
  (and (buffer-file-name)
       (tramp-tramp-file-p (buffer-file-name))))

;; Superseeded by use-package 'executable below
;; (defun ska-make-executable ()
;;   "Magic function to never have to chmod a script again.
;; This function is intended to be hooked to `after-save-hook` like this:

;;    (add-hook 'after-save-hook #'ska-make-executable)

;; Having done this you will automatically find all files starting with a
;; shebang executable."
;;   (interactive)
;;   (and 
;;    (not (ska-current-buffer-is-tramp-file))
;;    (save-excursion
;;          (save-restriction
;;            (widen)
;;            (goto-char (point-min))
;;            (save-match-data
;;              (looking-at "^#!"))))
;;        (not (file-executable-p buffer-file-name))
;;        (shell-command (concat "chmod u+x " buffer-file-name))
;;        (message
;;         (concat "Saved as script: " buffer-file-name))))


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

;; Something new
;; Should probably integrate transparently with detour. Does it even improve pop-global-mark?
;; (defvar ska-point-stack)
;; (defun ska-point-push ()
;;   (interactive)
;;   (push (point-marker) ska-point-stack))
;; (defun ska-point-pop ()
;;   (interactive)
;;   (when ska-point-stack
;;     (let* ((m (pop ska-point-stack))
;;            (b (marker-buffer m)))
;;       (switch-to-buffer b)
;;       (goto-char m))))
;; (global-set-key (kbd "C-:") #'ska-point-push)
;; (global-set-key (kbd "C-;") #'ska-point-pop)

(defun ska-insert-x-selection (arg)
  "Insert the current X selection at point.
Without any arguments uses the PRIMARY selection, with prefix
arguments looks into the CLIPBOARD."
  (interactive "P")
  (push-mark (point))
  (if arg
      (insert (x-get-selection 'CLIPBOARD))
    (insert (x-get-selection))))

(defun ska-kill-entire-line ()
  (interactive)
  (kill-region (line-beginning-position)
	       (+ 1 (line-end-position))))

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
;; killbuffer und schliess window
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


;;; Packages
(ska-init-message "Setting up packages")

;; Now using John Wiegley's use-package loaded earlier

;; First loading packages that add functions to use-package itself.
(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))

;; (use-package darktooth-theme
;;   :ensure t
;;   :config
;;   (load-theme 'darktooth t)
;;   (darktooth-modeline))
;; I seem to like nimbus because the contrast is better than darktooth
;;   for my eyes. I just can't see buffer boundaries so would have to
;;   either set the mode-line faces directly or try if powerline works
;;   for me.
;; (use-package powerline
;;   :ensure t
;;   :config
;;   (powerline-default-theme))
(use-package nimbus-theme
  :ensure t
  :config
  (copy-face 'mode-line 'mode-line-inactive)
  (load-theme 'nimbus t))

(use-package mood-line
  :ensure t
  :hook (after-init . mood-line-mode))


;;; Built-in packages you usually don't even care about requiring.
(use-package saveplace
  :demand t
  :config
  (setq-default save-place t))

(use-package paren
  :config
  (show-paren-mode 1))

(use-package frame
  :config
  (blink-cursor-mode -1))

(use-package tool-bar
  :config
  (tool-bar-mode -1))

(use-package menu-bar
  :bind
  (([(f4)] . kill-this-buffer)
   ([(control f4)] . ska-kill-this-window)
   :map ska-ctrl-v-map
   ([(control k)] . kill-this-buffer)
   ([(k)]         . ska-kill-this-window)))

(use-package simple
  :config
  (column-number-mode 1)
  (setq next-line-add-newlines nil)
  (setq track-eol t)
  (auto-fill-mode 1)
  :bind
  ([(control z)]         . yank)
  ([(control backspace)] . backward-kill-word)
  ([(control delete)]    . kill-word)
  ([(meta k)]            . ska-kill-entire-line)
  ([f9]                  . repeat-complex-command)
  ([(meta g)]            . goto-line))

(use-package files
  :config
  (setq require-final-newline nil))

(use-package align
  :bind
  (:map ska-ctrl-v-map
	([(control a)] . align)))

(use-package kmacro
  :bind
  ([(f1)] . kmacro-end-or-call-macro)
  ([(control f1)] . kmacro-start-macro-or-insert-counter))

;; use-package's :bind does not support lambdas.
(use-package window
  :init
  (defun ska-other-window-1 ()
    (interactive)
    (other-window 1))
  :bind
  ([(f5)]          . delete-other-windows)
  ([(control f5)]  . delete-window)
  ([f6]            . split-window-vertically)
  ([(control f6)]  . split-window-horizontally)
  ([(control tab)] . ska-other-window-1))

(use-package select
  :bind
  (:map ska-ctrl-v-map
	([(control y)] . ska-insert-x-selection)))

(use-package dabbrev
  :bind
  ("S-SPC" . dabbrev-expand))

(use-package minibuffer
  :bind
  ([(shift iso-lefttab)] . completion-at-point)
  ([(backtab)]           . completion-at-point))

(use-package lisp
  :bind
  ;; old XEmacs defaults deep in finger memory
  ("<M-left>"  . backward-sexp)
  ("<M-right>" . forward-sexp))

(use-package apropos
  :bind
  ;; default is just commands
  ("C-h a" . apropos))

;;; My own packages
(use-package highlight-context-line
  :ensure t
  :config 
  (highlight-context-line-mode))

(use-package detour
  :ensure t
  :bind
  ([(control \.)] . detour-mark)
  ([(control \,)] . detour-back))

;; Packages living in my ~/.emacs.d/lisp dir. These are not
;; package-installable. Mostly written by me or friends or found
;; somewhere on the net                                            . 

(use-package cdargs)

(use-package chb-util)

(use-package mtorus
  :config
  (mtorus-init)
  (mtorus-install-suggested-bindings)
  (defun mtorus-my-buffer-skip-p (buffer)
    "The my predicate used for `mtorus-buffer-skip-p'
is to skip only the special buffers whose name begins with a space . "
    (or
     (string-match "^[ ]+"         (buffer-name buffer))
     (string-match "[Cc]ompletio" (buffer-name buffer))
     (string-match "slime-events" (buffer-name buffer))
     (string-match "SLIME Note" (buffer-name buffer))
     (string-match "inferior-lisp" (buffer-name buffer))
     (string-match "[Cc]ompil" (buffer-name buffer))))
  
  (setq mtorus-buffer-skip-p #'mtorus-my-buffer-skip-p
        mtorus-notify-popup-separator "  "))

(use-package ll-debug
  :bind
  (:map ska-ctrl-v-map
	([(control v)] . ll-debug-toggle-comment-region-or-line)
	([(d)]         . ll-debug-insert)
	([(r)]         . ll-debug-revert)))

;;; More packages usually shipped with Emacs
(use-package recentf
  :config
  (setq recentf-exclude 
        '("~$" "^/ftp:" "^/ssh:" "sync-recentf-marker"))
  (setq recentf-max-saved-items 99)
  ;; required for sync-recentf to work. Sync every 60s
  (setq recentf-auto-cleanup 60)
  (setq recentf-save-file "~/.emacs.d/recentf.eld")
  (recentf-mode 1))

(use-package autoinsert
  :config
  ;; A user . el is always there, even with bad defaults.
  (setq auto-insert-copyright my-copyright-holder)

  (add-hook 'find-file-hooks 
            (lambda ()
              (auto-insert))))

(use-package ispell
  :config
  (setq ispell-program-name "aspell"))

(use-package flyspell
  :bind
  (:map flyspell-mode-map
        ([(control \. )] . detour-mark)
        ([(control \,)]  . detour-back)))

(use-package locate
  :bind
  (:map ska-ctrl-v-map
	([(control l)] . locate)))

(use-package occur
  :bind
  (:map ska-ctrl-v-map
	([(control o)] . occur)))

(use-package find-func
  :bind
  (:map ska-ctrl-v-map
	([(f)] . find-function)))

(use-package imenu
  :bind
  (:map ska-ctrl-v-map
	([(control f)] . imenu)))

(use-package term
  :init
  (defun ska-run-bash ()
    (interactive)
    (term "/bin/bash"))
  :bind
  ([(control f3)] . ska-run-bash))

(use-package eshell
  :bind
  ([(f3)] . eshell))

(use-package hl-line
  :config (global-hl-line-mode))

(use-package time-stamp
  :config
  (add-hook 'write-file-hooks 
            (lambda ()
              (time-stamp))))

(use-package executable
  :config
  (add-hook 'after-save-hook
            #'executable-make-buffer-file-executable-if-script-p))

;;; Now packages that you usually have to install from ELPA and
;; friends. 

;; had my own recentf sync for years. Switch to this one on melpa in
;; 2017
(use-package sync-recentf
  :ensure t)

(use-package ivy
  :demand
  :ensure t
  :bind
  (:map ska-ctrl-v-map
	([(control r)] . ivy-resume))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "
        ivy-extra-directories nil
        ivy-on-del-error-function #'ignore
        ))

;; (use-package ctrlf
;;   :ensure t
;;   :config
;;   (ctrlf-mode +1))

(use-package ag
  :ensure t
  :pin melpa-stable)

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (global-company-mode 1))

(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (global-unset-key (kbd "C-x c")))
  :bind
  (:map ska-ctrl-v-map 
        ([(control h)] . helm-command-prefix)))

;; Has to wait until I get Emacs >= 25.3.x
;; (use-package dashboard
;;   :ensure t
;;   :config
;;   (dashboard-setup-startup-hook))

(use-package which-key
  :ensure t
  :pin melpa-stable
  :config
  (which-key-mode 1))

(use-package projectile
  :ensure t
  :after ivy
  :diminish projectile-mode
  :bind
  (:map ska-ctrl-v-map
	([(control p)] . projectile-commander))
  :config
  (projectile-global-mode 1)
  (setq projectile-completion-system 'ivy))

(use-package magit
  :ensure t
  :chords (("GG" . magit-status)
           ("FF" . magit-file-dispatch))
  ;; :bind
  ;; (:map ska-ctrl-v-map 
  ;;       ([(g)] . magit-status))
  :config
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :ensure t
  :pin melpa-unstable
  :after magit)

(use-package org
  :ensure t
  :config
  (add-hook 'org-mode-hook #'auto-fill-mode)
  (setq org-todo-keywords
        '((sequence "TODO" "|" "DONE" "CANCELED"))))



(use-package linum
  :ensure t
  :config (global-linum-mode t))

;; (use-package docker
;;   :ensure t
;;   :pin melpa-stable
;;   :chords (("DD" . docker)))

(use-package windresize
  :ensure t
  :bind
  (([(control shift left)]  . windresize-select-left)
   ([(control shift right)] . windresize-select-right)
   ([(control shift up)]    . windresize-select-up)
   ([(control shift down)]  . windresize-select-down)
   ([(f8)] . windresize)))

(use-package subword
  :ensure t)

(use-package restclient
  :ensure t)

(use-package smartparens
  :ensure t
  :config (smartparens-global-mode 1))


(use-package command-log-mode
  :ensure t)
  
;;; Packages related to major modes (from any source)

;; (use-package text-mode
;;   :config
;;   (auto-fill-mode 1))

(use-package markdown-mode
  :ensure t
  :config
  (add-hook 'markdown-mode-hook
          (lambda ()
            (auto-fill-mode 1))))

(use-package yaml-mode
  :ensure t)

(use-package groovy-mode
  :ensure t
  :pin melpa-stable)

(use-package terraform-mode
  :ensure t)

(use-package graphviz-dot-mode
  :ensure t)

(use-package conf-mode)

(use-package lisp-mode
  :chords
  (("jj" . "("))
  :config
  (auto-fill-mode 1))

(use-package clojure-mode
  :ensure t
  :pin melpa-stable
  :after subword
  :chords
  (("jj" . "(")
   ("JJ" . "[")
   ("BB" . "{"))
  :config
  (add-to-list 'auto-mode-alist '("\\.boot$"  . clojure-mode))
  (add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'auto-fill-mode)
  (add-to-list 'auto-insert-alist
             '(clojure-mode
               nil
               (clojure-insert-ns-form) "\n"
               ";; Copyright (C) " (substring (current-time-string) -4)
               " " auto-insert-copyright "\n;;\n"
               ";; Author: "(user-full-name) "\n;;\n"
               )))

(use-package cider
  :ensure t
  :pin melpa-stable
  :init
  (setq cider-use-tooltips t ; breaks mouse selection for me
        cider-repl-tab-command #'indent-for-tab-command
        cider-repl-history-file "~/.emacs.d/cider-history.eld"
        cider-auto-select-error-buffer t
        cider-repl-display-help-banner nil
        cider-mode-line-show-connection nil)
  
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'subword-mode)
  (add-hook 'cider-repl-mode-hook
            (lambda ()
              (setq cider-repl-use-clojure-font-lock t))))

(use-package cider-mode
  :ensure cider
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode))

(use-package cperl-mode
  :init
  (defalias 'perl-mode 'cperl-mode)
  :bind
  (:map cperl-mode-map
	([(meta tab)]                       . hippie-expand)	
	([(control b) (control f)]          . ska-skel-perl-file-loop)
	([(control b) (control b)]          . executable-interpret)
	([(control b) (control d)]          . ska-skel-perl-dbi-connect)
	([(control b) (control s)]          . ska-skel-perl-sub)
	([(control b) (control i)]          . ska-skel-perl-prog-id)
	([(control b) (control o)]  . ska-skel-perl-options)
	([(control b) (control h)]  . cperl-perldoc-at-point)
	([(control b) (control v)]          . ska-skel-perl-svn-version)
	)
  ;; It's been ages I last tunes these vars . Seems to still work for
  ;; me in 2017, though
  :config
  (add-hook
   'cperl-mode-hook
   (lambda ()
     (setq indent-tabs-mode nil
	   cperl-indent-level 4
	   cperl-auto-newline nil
	   cperl-electric-linefeed nil
	   cperl-electric-parens t
	   cperl-electric-lbrace-space t
	   cperl-electric-keywords nil
	   cperl-mode-abbrev-table nil
	   cperl-lazy-help-time 1
	   cperl-pod-here-fontify nil
	   cperl-highlight-variables-indiscriminately t
	   )
     (auto-fill-mode 1)
     (cperl-lazy-install)
     (turn-off-smartparens-mode)
     ))
  )

(use-package python)

(use-package sh-script
  :mode (("\\.*bashrc$" . sh-mode)
         ("\\.*bash_profile" . sh-mode)))

(use-package help-mode
  :after find-func
  :bind
  (:map help-mode-map
	("f" . find-function-at-point)))

(use-package sgml-mode
  :config
  (add-hook 'html-mode-hook
            (lambda ()
             (auto-fill-mode 1)
             (setq fill-column 100))))

;; Giving this a try (2017)
(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  :config 
  (add-hook 'web-mode-hook
	    (lambda ()
	      (setq web-mode-enable-css-colorization t)
	      (setq web-mode-markup-indent-offset 2)
	      (setq web-mode-style-padding 2)
	      (setq web-mode-script-padding 2))))

(use-package nxml-mode
  :mode (("\\.gpx$" . nxml-mode)
         ("\\.plist$" . nxml-mode)
         ("\\.rng$" . nxml-mode)
         ("\\.rss$" . nxml-mode)
         ("\\.sch$" . nxml-mode)
         ("\\.svg$" . nxml-mode)
         ("\\.tcx$" . nxml-mode)
         ("\\.xml$" . nxml-mode)
         ("\\.xsd$" . nxml-mode)
         ("\\.xslt$" . nxml-mode))
  :bind
  (:map nxml-mode-map
	([(control c) (/)]          . nxml-finish-element)
	([(control c) (control \.)] . detour-mark)
	([(control c) (control \,)] . detour-back))
  :config
  (setq magic-mode-alist
	(cons '("<\\?xml " . nxml-mode) magic-mode-alist))

  (add-hook 'nxml-mode-hook
	    (lambda ()
	      (auto-fill-mode 1)
	      (setq nxml-sexp-element-flag t
		    fill-column 80
		    nxml-bind-meta-tab-to-complete-flag t
		    nxml-sexp-element-flag t
		    nxml-slash-auto-complete-flag t)
	      (flyspell-mode 1))))

(use-package tex
  :ensure auctex
  :init
  (setq TeX-parse-self t) ; Enable parse on load . 
  (setq TeX-auto-save t) ; Enable parse on save . 
  (setq-default TeX-master nil)
  (setq reftex-plug-into-AUCTeX t)
  :config
  (add-hook 'LaTeX-mode-hook
	    (lambda ()
	      (auto-fill-mode 1)
	      (LaTeX-math-mode)
	      (turn-on-reftex))))

  
(use-package company-auctex
  :ensure t
  :config
  (company-auctex-init))



(ska-init-message "Defining skeletons")

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
  (define-key map '[(meta tab)]               #'hippie-expand)
  (define-key map '[(control b) (control a)]  #'ska-edit-automake-file)
  (define-key map '[(control b) (control b)]  #'compile)
  (define-key map '[(control b) (control m)]  #'manual-entry)
  ;; macros, templates, skeletons:
  (define-key map '[(control b) (m) (f) (f)]  #'ska-skel-c-fflush)
  (define-key map '[(control b) (m) (f) (p)]  #'ska-skel-c-fprintf)
  (define-key map '[(control b) (m) (i)]      #'ska-skel-c-include)
  (define-key map '[(control b) (m) (m)]      #'ska-skel-c-main)
  (define-key map '[(control b) (m) (c)]      #'ska-skel-c-comment)
  (define-key map '[(control b) (m) (n)]      #'ska-skel-c-printf-newline)
  (define-key map '[(control b) (m) (p)]      #'ska-skel-c-printf)
  (define-key map '[(control b) (m) (P)]      #'ska-skel-c-printf-flush)
  (define-key map '[(control b) (m) (l) (w)]  #'ska-skel-c-loop-while)
  )

(defun ska-c++-mode-keys ()
  "Set my personal keys for C++ mode."
  (local-set-key '[(control b) (m) (c)]     #'ska-skel-cc-class)
  (local-set-key '[(control b) return]      #'ska-skel-cc-endl)
  (local-set-key '[(control b) (m) (l) (f)] #'ska-skel-cc-loop-for)
  )
(defun ska-c-mode-keys ()
  "Set my personal keys for plain C mode."
  (local-set-key '[(control b) (m) (c)]     #'ska-skel-c-comment)
  (local-set-key '[(control b) return]      #'ska-skel-cc-endl)
  (local-set-key '[(control b) (m) (l) (f)] #'ska-skel-c-loop-for)
  )

(add-hook 'c++-mode-hook
          (lambda ()
            ;; my keybindings
            (ska-c-common-mode-keys c++-mode-map)
            (ska-c++-mode-keys)
            ))

(add-hook 'c-mode-hook
          (lambda ()
            ;; my keybindings
            (ska-c-common-mode-keys c-mode-map)
            (ska-c-mode-keys)
            ))

(add-hook 'c-mode-common-hook
          (lambda ()
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

(add-to-list 'auto-insert-alist
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
               "        None;\n\n"
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


;; My old Common Lisp, Slime SBCL stuff is missing here.


;; TeX, LaTeX
;;(ska-init-message "  LaTeX")
;;(try-require 'tex-site)

;; (setq TeX-parse-self t) ; Enable parse on load.
;; (setq TeX-auto-save t) ; Enable parse on save.
;; (setq-default TeX-master nil)
;; (setq reftex-plug-into-AUCTeX t)


;; (defun ska-latex-mode-keys ()
;;   "Set keys for latex-mode (AUCTeX) . "
;;   (local-set-key '[(tab)]                    'LaTeX-indent-line)
;; ;  (define-key LaTeX-math-keymap
;; ;    (concat LaTeX-math-abbrev-prefix "/")    'LaTeX-math-frac)
;; )
;; (defun ska-bibtex-mode-keys ()
;; 
;; 
;; 

;; (add-hook 'LaTeX-mode-hook
;;           '(lambda ()
;;              ;; meine keybindings
;;              (ska-latex-mode-keys)
;;              (auto-fill-mode 1)
;;              ;(setq ispell-parser 'tex)
;;              (LaTeX-math-mode)
;;              (turn-on-reftex)
;; ;             (when (string-match "diary" buffer-file-name)
;; ;               (turn-on-tex-diary))
;;              ))

;; (add-hook 'TeX-language-de-hook
;;           '(lambda ()
;;              ;;(ispell-change-dictionary "deutsch")
;;              ))

 ;; (add-hook 'BibTeX-mode-hook
 ;;          '(lambda ()
 ;;             (ska-bibtex-mode-keys)
 ;;             ))


;; Java JDE missing


;;;; =================================================================
;;;; MISC SETTINGS
(ska-init-message "Misc Settings")
(setq-default indent-tabs-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)
;(setq eshell-ask-to-save-history 'always)
(setq inhibit-splash-screen t)
(setq visible-bell t)
;(setq apropos-do-all t)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)


(define-key ska-ctrl-v-map '[(control t)] #'fdlcap-change-case-current-word)

(global-set-key '[(control up)] (lambda ()
                                  (interactive)
                                  (scroll-down 1)))
(global-set-key '[(control down)] (lambda ()
                                    (interactive)
                                    (scroll-up 1)))

(global-set-key '[(home)] #'chb-home)
(global-set-key '[(end)] #'chb-end)
