;;; linmag-mode.el --- Editing for the Linux Magazine
;; $Id: $
;; Copyright (C) 2007-2009 by Stefan Kamphausen
;; Author: Stefan Kamphausen <http://www.skamphausen.de>
;; Keywords: user
;; This file is not part of any flavour of Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.


;;; Commentary:
;; This is a major mode for writing articles for the Linux Magazin
;; (German). 

;;; History
;; Version 1.8: Fixed invalid read syntax, added skeleton for boxes.
;; Version 1.7: added error regexps to compilation
;; Version 1.6: added compilation and stats
;; Version 1.5: added eldoc
;;
;; Prehistoric:
;; This mode was written for and used for the first time for the
;; writing of
;; http://www.linux-magazin.de/Heft-Abo/Ausgaben/2007/08/Gross-in-Mode
;; After that changes were made on the fly.  History starts with
;; version 1.5

;;; Code:
(require 'compile)
(require 'eldoc)

(defvar linmag-version "1.8")

(defvar linmag-commands
  '(("@R:" . "Rubrik: Programmieren, Know-how, Sysadmin usw")
    ("@SW:" . "Schlagwort: Landet unterhalb der Rubrik am Seitenstreifen. ")
    ("@D:" . "Dachzeile: Der sachliche Titel, in kleinerer Schrift über dem Titel")
    ("@T:" . "Titel: Der Titel (die Schlagzeile) des Artikels (ca. 1-3 Worte).")
    ("@V:" . "Vorspann: Der kurze Einleitungstext, der direkt unter dem Titel steht")
    ("@A:" . "Autor: Name des Autors, steht am Ende des Vorspanns.")
    ("@L:" . "Lauftext: Der eigentliche Text (Fließtext).")
    ("@ZT:" . "Ein Zwischen-Titel")
    ("@LI:" . "Listing: Wird in Courier-Schrift gedruckt.")
    ("@B:" . "Die Bildunterschrift")
    ("@Bi:" . "Eine Angabe zum Bild, meist Dateiname")
    ("@#:" . "Ein Kommentar bis zum Ende der Zeile")
    ("@IT:" . "Infos-Titel: (Autor, Infos)")
    ("@IL:" . "Infos-Lauftext")
    ("@IE:" . "Infos-Ende: Optional")
    ("@KT:" . "Ein Kasten-Titel (bei Listings mit \"Listing 1:\")")
    ("@KL:" . "Kasten-Lauftext: Listings und Zwischentitel wie im Lauftext")
    ("@KE:" . "Das (optionale) Kasten-Ende")
    ("@TT:" . "Tabellen-Titel: Name der Tabelle, beginnen mit \"Tabelle 1:\"")
    ("@TH:" . "Tabellen-Header: Spaltenüberschriften in der Tabelle")
    ("@TL:" . "Tabellen-Lauftext: TAB-separiert")
    ("@TE:" . "Tabellen-Ende: Optional. "))
  "List of available markup commands for linmag-mode together with docs.
Documentation is German since this library is used for writing
articles for the German Linux Magazin.")

(defvar linmag-formatter
  '("<I>" "<C>" "<B>" "<+>" "<->" "<U>")
  "List of formatting instructions for linmag-mode.")

;; FIXME How to write this in a maintainable way??
(defvar linmag-curious-constructs-re
  (concat
   "\\<" ;; beginning of word
    ;; Indefinite pronouns 
   "\\(man\\|darf\\|soll"

   "\\|" ;; more alternatives
    ;; Nominalization
   "\\([A-ZÄÖÜ]\\w+ung\\)\\|\\([A-ZÄÖÜ]\\w+ion\\)"

   "\\|" ;; more alternatives
    ;; passive
   "werden\\|wird\\|\\(ge\\w+\\)"
    ;;"\\bwerden\\b" "\\bwird\\b" "\\bge\\w+\\b"

   ;; End of word
   "\\)\\>")
;;    )
  "List of regular expression matching probably bad writing.")

;;;; Imenu
(defvar linmag-imenu-generic-expression
  '((nil "^@ZT:[ \t]+\\([^\n]+\\)" 1)))


;;;; Convenience Functions
(define-skeleton linmag-insert-listing
    "Inserts an inline listing command."
  nil
  "@LI: \n")

(define-skeleton linmag-insert-text
    "Inserts the command for normal text."
  nil
  "@L: ")

(define-skeleton linmag-insert-subtitle
    "Inserts the command for a subtitle."
  nil
  "@ZT: ")

(define-skeleton linmag-insert-italics
    "Inserts italic markup."
  nil
  "<I>" _ "<I>")

(define-skeleton linmag-insert-code
    "Inserts code markup."
  nil
  "<C>" _ "<C>")

(define-skeleton linmag-insert-bold
    "Inserts bold markup."
  nil
  "<B>" _ "<B>")

(define-skeleton linmag-insert-url
    "Inserts URL markup."
  nil
  "<U>http://" _ "<U>")

(define-skeleton linmag-insert-box
    "Inserts a box."
  nil
  "@KT:"_ "\n\n@KL:\n\n@KE:\n")

(define-skeleton linmag-insert-boxlisting
    "Inserts markup for a listing in a separate box.
Actually this should count the already defined listings and number
them automagically."
  "Name: "
  "@KT: Listing " _ ": " str
  "\n\n"
  "@LI: \n"
  _
  "\n@KE:\n"
  )

(defun linmag-new-article-template ()
  "Insert a new article template into the current buffer.

This is a totally rudimentary implementation and should use variables
from a customization group for AUTHOR(S), authorinformation and other
predefined information.  A completing read for RUBRIK and THEMA would
be nice.
"
  (interactive)
  (flet ((insert-part (name)
           (insert "@#: ")
           (loop repeat 66 do (insert "="))
           (insert "\n@#: ")
           (loop repeat 30 do (insert " "))
           (insert name)
           (insert "\n@#: ")
           (loop repeat 66 do (insert "="))
           (insert "\n")))
    (let ((final-pos nil))
      (insert-part "KOPF")
      (insert "\n@R:RUBRIK")
      (setq final-pos (point))
      (insert "\n@SW:THEMA\n")
      (insert "\n@D:DACHTITEL")
      (insert "\n@T:TITEL\n")
      (insert "\n@V:VORTEXT\n\n")
      (insert "@A: AUTOR(EN)\n\n")
      (insert-part "TEXT")
      (insert "\n\n@L:\n\n\n")
      (insert-part "FUSS")
      (insert "\n\n@IT:Der Autor")
      (insert "\n@IL: ...\n")
      (insert "@IT:Infos\n\n")
      (insert "@IL:\n")
      (goto-char final-pos))))

;;;; Compile/Generate HTML
(defvar linmag-compile-programm nil
  "The external programm to use to translate linmag format.
Probably you'll have to have access to a programm which is only for
internal use for Linux Magazin editors")

(defvar linmag-compile-additional-args "-i -t %f %F.html"
  "Additional arguments to the compile command.
Use %f as a placeholder for the linmag file your are working on and %F
for the same without extension")

(defvar linmag-compile-stats-args "-dsvq %f"
  "Additional arguments to the statistics command.
These are used when you call `linmag-stats' and not `linmag-compile'.
Use %f as a placeholder for the linmag file your are working on and %F
for the same without extension")

(defvar linmag-compile-current-filename nil
  "Global storage for the filename of the current article.
Used during compilation, see `linmag-compile-get-filename'.")

(defun linmag-compile-get-filename (&optional args)
  "Return the filename of the current article.
Since the \"sf\" program does not output the filename it is
working on, we need to provide our own function which does that.
The compilation mode thing is quite complicated, but you can do
everything. "
  ;; needs a list, for whatever reason
  (list linmag-compile-current-filename))

(defvar linmag-compilation-error-regexp-alist
  ;; the 1 means match 1 and gives linenumber
  '(("Zeile \\([0-9]+\\)" linmag-compile-get-filename 1)))

;; stolen from cperl mode.  Don't ask me, I don't fully understand.
(define-compilation-mode linmag-sf-mode "LinMagSF"
  "Mode for compiling linmag articles."
  (set (make-local-variable 'compilation-disable-input) t)
  (cond ((boundp 'compilation-error-regexp-alist-alist) ;; xemacs 20.x
         (make-local-variable 'compilation-error-regexp-alist-alist)
         (set 'compilation-error-regexp-alist-alist
              (cons (cons 'linmag (car linmag-compilation-error-regexp-alist))
                    (symbol-value 'compilation-error-regexp-alist-alist)))
         (if (fboundp 'compilation-build-compilation-error-regexp-alist)
             (let ((f 'compilation-build-compilation-error-regexp-alist))
               (funcall f))
           (make-local-variable 'compilation-error-regexp-alist)
           (push 'linmag compilation-error-regexp-alist)))
        ((boundp 'compilation-error-regexp-alist) ;; xmeacs 19.x
         (make-local-variable 'compilation-error-regexp-alist)
         (set 'compilation-error-regexp-alist
              (append linmag-compilation-error-regexp-alist
                      (symbol-value 'compilation-error-regexp-alist))))))


(defun linmag-build-compile-command (buffer mode)
  "Use `linmag-compile-programm' and `linmag-compile-additional-args'
  to create the shell command to call.  Translate %f and %F to the
  filename of the current articel with and without extension
  respectively"
  (let* ((f (buffer-file-name buffer))
         (F (file-name-sans-extension f))
         (cmd))
    (setq cmd (concat 
               linmag-compile-programm " "
               (if (and (stringp mode) (string= mode "stats"))
                   linmag-compile-stats-args
                 linmag-compile-additional-args)))
    (when (string-match "%f" cmd)
      (setq cmd (replace-match (shell-quote-argument f) 
                               t t cmd)))
    (when (string-match "%F" cmd)
      (setq cmd (replace-match (shell-quote-argument F)
                               t t cmd)))
    cmd))


(defun linmag-compile (&optional mode)
  (interactive)
  (if (not linmag-compile-programm)
      (error "Your missing special program for linmag translation.")
    (let* ((cmd (linmag-build-compile-command (current-buffer) mode)))
      (setq linmag-compile-current-filename 
            (buffer-file-name (current-buffer)))
    
      (compilation-start cmd 
                         'linmag-sf-mode 
                         #'(lambda (arg) 
                             "*Linmag Compilation*")
                         ))))


(defun linmag-stats ()
  (interactive)
  (linmag-compile "stats"))

;;;; Completion

;; (defun linmag-completion-looking-at-command-part ()
;;   (save-excursion
;;     (let ((end (point))
;;        (start (save-excursion
;;                 (search-backward "@" (point-at-bol) t))))
;;       (when start
;;      (buffer-substring-no-properties start end)))))

;; (defun linmag-complete ()
;;   (interactive)
;;   (let ((cmd-part (linmag-completion-looking-at-command-part)))
;;     (when cmd-part
;;       (insert
      
;;;; Mode Stuff
(defvar linmag-mode-hook nil
  "Hook to run when entering linmag-mode.")

(setq auto-mode-alist
        (append '(("\\.linmag$" . linmag-mode))
                auto-mode-alist))

(defvar linmag-font-lock-keywords
  (list
   (cons (regexp-opt (mapcar #'car linmag-commands)) font-lock-keyword-face)
   (cons (regexp-opt linmag-formatter) font-lock-variable-name-face)
   (cons linmag-curious-constructs-re font-lock-warning-face))
  "Rules for highlighting in linmag mode.")

(defvar linmag-mode-map ()
  "Keymap used in linmag-mode.")
(when (not linmag-mode-map)
  (setq linmag-mode-map (make-sparse-keymap))
  (define-key linmag-mode-map [(control c) (control l)] 'linmag-insert-listing)
  (define-key linmag-mode-map [(control c) (control t)] 'linmag-insert-text)
  (define-key linmag-mode-map [(control c) (control i)] 'linmag-insert-italics)
  (define-key linmag-mode-map [(control c) (control c)] 'linmag-insert-code)
  (define-key linmag-mode-map [(control c) (control b)] 'linmag-insert-bold)
  (define-key linmag-mode-map [(control c) (b)]         'linmag-insert-box)
  (define-key linmag-mode-map [(control c) (control u)] 'linmag-insert-url)
  (define-key linmag-mode-map [(control c) (control p)] 'linmag-insert-boxlisting)
  (define-key linmag-mode-map [(control c) (control s)] 'linmag-stats)
  (define-key linmag-mode-map [(control c) (c)]         'linmag-compile)
)

(defvar linmag-eldoc-command-regexp 
  (regexp-opt (mapcar #'car linmag-commands) t))

(defun linmag-eldoc-function ()
  (interactive)
  (save-excursion
    (goto-char (point-at-bol))
    (when (looking-at linmag-eldoc-command-regexp)
      (let ((docs (cdr (assoc (match-string-no-properties 1) linmag-commands))))
        docs))))

(defun linmag-mode ()
  "Major mode for writing articles for the German Linux Magazin.

\\{linmag-mode-map}"
  (interactive)
  ;; Initializing
  (kill-all-local-variables)

  ;; Setting up font-locking
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(linmag-font-lock-keywords nil t nil nil))

  (use-local-map linmag-mode-map)

  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (setq comment-start "@#: "
        comment-end ""
        comment-start-skip "@#: +")

  ;; eldoc
  (make-local-variable 'eldoc-documentation-function)
  (setq eldoc-documentation-function 'linmag-eldoc-function)
  (eldoc-mode 1)

  ;; Imenu
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression linmag-imenu-generic-expression)

;; geht auch nicht
;; (cond ((boundp 'compilation-error-regexp-alist-alist);; xemacs 20.x
;;          (make-local-variable 'compilation-error-regexp-alist-alist)
;;          (set 'compilation-error-regexp-alist-alist
;;               (cons (cons 'linmag (car linmag-compilation-error-regexp-alist))
;;                     (symbol-value 'compilation-error-regexp-alist-alist)))
;;          (if (fboundp 'compilation-build-compilation-error-regexp-alist)
;;              (let ((f 'compilation-build-compilation-error-regexp-alist))
;;                (funcall f))
;;            (make-local-variable 'compilation-error-regexp-alist)
;;            (push 'linmag compilation-error-regexp-alist)))
;;         ((boundp 'compilation-error-regexp-alist);; xmeacs 19.x
;;          (make-local-variable 'compilation-error-regexp-alist)
;;          (set 'compilation-error-regexp-alist
;;                (append linmag-compilation-error-regexp-alist
;;                        (symbol-value 'compilation-error-regexp-alist)))))
  ;; common major mode stuff
  (setq mode-name "LinMag"
        major-mode 'linmag-mode)
  (run-hooks 'linmag-mode-hook))


(provide 'linmag-mode)
;;; linmag-mode.el ends here
