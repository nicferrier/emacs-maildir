;;; maildir-notmuch.el --- notmuch indexing                    -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: mail

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This provides notmuch integration for the maildir MUA.

;; You can search an index and present the results as a folder.

;;; Code:

(require 'shadchen)
(require 's)
(require 'kv)
(require 'dash)

(defun maildir-index/match-msg (msg)
  (match msg
    ((list (list nil)) (list)) ; when there's an error we get this
    ((list
      (list
       (plist :filename filename
              :headers (plist :Subject subject
                              :From from
                              :To to
                              :Date date)) _))
     (list
      (cons "filename" filename)
      (cons "subject" subject)
      (cons "from" from)
      (cons "to" to)
      (cons "date" date)))))

(defun maildir-index/match-msg-test ()
  (let ((example
         '(((:id "UE04KOVM.2872911@commerceone.com"
             :match t
             :excluded nil
             :filename "/home/nicferrier/mymaildir/var/maildir/nferrier/cache/1425641905.V902Ib2ffe0M195109.Ubuntu-1110-oneiric-64-minimal"
             :timestamp 1425639510 :date_relative "March 06" :tags ("inbox" "unread")
             :headers
             (:Subject "You have received a new secure message from BankLine"
                       :From "Bankline <secure.message@business.natwest.com>"
                       :To "administrator@ferrier.me.uk" :Date "Fri, 06 Mar 2015 11:58:30 +0100"))
            nil))))
    (maildir-index/match-msg example)))

(defun maildir-index/show (term form)
  "Display a notmuch index FORM in a maildir buffer."
  (with-current-buffer (get-buffer-create (format "*maildir-index-%s*" term))
    (erase-buffer)
    (->> form
      (-map 'maildir-index/match-msg)
      (--map (kva "filename" it))
      (-filter 'identity)
      (-map 'maildir/index-header)
      (-filter 'maildir-verify-header)
      (-map 'maildir/hdr->summary)
      (--map (concat it "\n"))
      (apply 'concat)
      (insert))
    (maildir-mode)
    (pop-to-buffer (current-buffer))))

(defun maildir-index/notmuch-read (buf)
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (save-match-data ; this was an attempt to filter errors... we use 2> /dev/null now
        (while (re-search-forward "^Error .*\n" nil t)
          (goto-char (match-beginning 0))
          (delete-region (point) (+ 1 (line-end-position)))))
      (goto-char (point-min))
      (read (current-buffer)))))

(defun maildir-notmuch-lookup (maildir term)
  "Lookup TERM in MAILDIR using notmuch and display it."
  (interactive
   (list
    maildir-mail-dir
    (read-from-minibuffer "Index term: ")))
  (with-current-buffer (get-buffer-create "*maildir-index*") (erase-buffer))
  (let ((default-directory (expand-file-name maildir))
        (proc (start-process-shell-command
               "maildir-index" "*maildir-index*"
               (format "notmuch show --format=sexp --body=false %s 2> /dev/null"
                       term))))
    (set-process-sentinel
     proc
     (lambda (proc evt)
       (message "evt is %s" evt)
       (cond
         (t ;(equal evt "finished\n")
          (maildir-index/show
           term
           (maildir-index/notmuch-read (process-buffer proc)))))))
    (format "Indexing maildir for %s" term)))

(provide 'maildir-notmuch)

;;; index.el ends here
