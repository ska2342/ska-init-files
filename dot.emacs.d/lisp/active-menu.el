;;; chb-util.el --- editing utilities          -*- Mode: Emacs-Lisp -*-
;; Time-stamp: <09-Aug-2004 07:51:17 skamphausen>

;; Copyright (C) 2001 Claus Brunzema <mail@cbrunzema.de>
;; see http://www.cbrunzema.de

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Here are some editing utilities I find useful.

;;; Code:

;; string utils
(defun chb-string-reverse (str)
  (apply #'string (reverse (string-to-list str))))

(defun chb-string-trim-matching-head (a b)
  (let ((diff-a (copy-sequence a))
    (diff-b (copy-sequence b)))
    (while (and
        (> (length diff-a) 0)
        (> (length diff-b) 0)
        (char= (elt diff-a 0) (elt diff-b 0)))
      (setq diff-a (substring diff-a 1)
        diff-b (substring diff-b 1)))
    (list diff-a diff-b)))

(defun chb-string-diff (a b)
  (let* ((head-trim-ret-list (chb-string-trim-matching-head a b))
     (a-head-trimmed (first head-trim-ret-list))
     (b-head-trimmed (second head-trim-ret-list))
     (tail-trim-ret-list (chb-string-trim-matching-head
                  (chb-string-reverse a-head-trimmed)
                  (chb-string-reverse b-head-trimmed)))
     (a-trimmed (first tail-trim-ret-list))
     (b-trimmed (second tail-trim-ret-list)))
    (list (chb-string-reverse a-trimmed)
      (chb-string-reverse b-trimmed))))


;; text utils
(defvar chb-no-whitespace-re "[^ \t]")
(defvar chb-whitespace-re "[ \t\r\n]")

(defun chb-current-line-number ()
  (1+ (count-lines (point-min) (point-at-bol))))

(defun chb-current-line-string ()
  (buffer-string (point-at-bol) (point-at-eol)))

(defun chb-current-line-whitespace-only-p ()
  (not (string-match chb-no-whitespace-re (chb-current-line-string))))

(defun chb-current-line-empty ()
  (= (point-at-bol) (point-at-eol)))

(defun chb-highlight-region (start end)
  (interactive)
  (let ((ex (make-extent start end))
    (fa (make-face 'chb-highlight-line-face)))
    (set-face-background fa "LightSkyBlue2")
    (set-extent-face ex fa)
    ex))


;; delete and backspace
(defun chb-delete ()
  (interactive)
  (if (region-exists-p)
      (kill-region (region-beginning) (region-end))
    (call-interactively 'backward-or-forward-delete-char)))

(defun chb-backspace ()
  (interactive)
  (if (region-exists-p)
      (kill-region (region-beginning) (region-end))
    (call-interactively 'backward-delete-char-untabify)))


;; killing whitespace
(defun chb-kill-newline-or-tabspace-forward ()
  (interactive)
  (cond
   ((char= (following-char) ?\n)
    (delete-char 1))
   (t
    (while (or (char= (following-char) ?\ )
           (char= (following-char) ?\t))
      (delete-char 1)))))

(defun chb-kill-newline-or-tabspace-backward ()
  (interactive)
  (cond
   ((char= (preceding-char) ?\n)
    (delete-char -1))
   (t
    (while (or (char= (preceding-char) ?\ )
           (char= (preceding-char) ?\t))
      (delete-char -1)))))

(defun chb-just-one-space ()
  (interactive)
  (skip-chars-backward " \t\n")
  (let ((start (point)))
    (skip-chars-forward " \t\n")
    (delete-region start (point)))
  (insert " "))


;; easy keyboard macros
(defun chb-start-kbd-macro ()
  (interactive)
  (define-key
    global-map
    (events-to-keys (this-command-keys) t)
    'chb-end-kbd-macro)
  (start-kbd-macro nil))

(defun chb-end-kbd-macro ()
  (interactive)
  (define-key
    global-map
    (events-to-keys (this-command-keys) t)
    'chb-start-kbd-macro)
  (end-kbd-macro))


;; special home and end
(defun chb-home ()
  (interactive)
  (setq zmacs-region-stays t)
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
  (setq zmacs-region-stays t)
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


;; page movement
(defun chb-page-down ()
  (interactive)
  (setq zmacs-region-stays t)
  (next-line
   (- (window-displayed-height) next-screen-context-lines)))

(defun chb-page-up ()
  (interactive)
  (setq zmacs-region-stays t)
  (next-line
   (- (- (window-displayed-height) next-screen-context-lines))))


;; buffer switching
(defun chb-grep (l predicate)
  (defun helper (ret-list rest)
    (if (null rest)
    (reverse ret-list)
        (progn
          (if (funcall predicate (car rest))
          (setq ret-list (cons (car rest) ret-list)))
          (helper ret-list (cdr rest)))))
  (helper '() l))

(defun chb-buffer-should-be-skipped-p (buffer)
  (string-match "\\(^[ \\*]+\\|xemacs-custom\\)" (buffer-name buffer)))

(defun chb-next-buffer ()
  (interactive)
  (let ((blist (chb-grep (buffer-list)
             (lambda (buf)
               (not (chb-buffer-should-be-skipped-p buf))))))
    (if blist
    (switch-to-buffer (car (reverse blist))))))

(defun chb-previous-buffer ()
  (interactive)
  (let ((blist (chb-grep (buffer-list)
             (lambda (buf)
               (not (chb-buffer-should-be-skipped-p buf))))))
    (when (> (length blist) 1)
      (bury-buffer)
      (while (chb-buffer-should-be-skipped-p (current-buffer))
    (bury-buffer)))))



;; misc stuff
(defun chb-align-to-char-in-previous-line ()
  (interactive)
  (when (and (> (chb-current-line-number) 1)
             (> (current-column) 1))
    (backward-char)
    (let ((current-char (char-after))
          (target-column nil))
      (save-excursion
        (previous-line 1)
        (while (chb-current-line-whitespace-only-p)
          (previous-line 1))
        (when (search-forward (char-to-string current-char) (point-at-eol) t)
          (setq target-column (- (current-column) 1))))
      (when target-column
        (indent-to-column target-column)))
    (forward-char)))

(provide 'chb-util)
