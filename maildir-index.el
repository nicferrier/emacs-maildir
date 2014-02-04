;;; maildir-index.el --- indexing for maildir  -*- lexical-binding: t -*-

;; Copyright (C) 2014  Nic Ferrier

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

;; indexing a maildir is a good idea imo

;; we can remember the indexing term and rebuild the index

;;; Code:

(require 'pipe)

;; copy of the function in maildir.el
(defun maildir-index/2 (mail-dir &optional field-symbols)
  "Make an index of the specified MAIL-DIR.

Optionally use FIELD-SYMBOLS as the list of header field symbols
to produce the index for.  By default this is
`maildir-default-index-field-syms'."
  (loop
     with collect-file = nil
     for file in (directory-files (maildir/home mail-dir "cur") 't "^[^.]+")
     do
       (setq
        collect-file
        (condition-case err
            (let ((indexed (maildir/index-header file)))
              (if (and (assq 'date indexed)
                       (assq 'to indexed)
                       (assq 'from indexed))
                  indexed
                  ;; Else
                  (message "maildir-index: parsing %s expectation error" file)
                  (delete-file file)
                  nil))
          (error
           (message "maildir-index: parsing %s error %S" file err)
           (delete-file file)
           nil)))
     if collect-file
     collect collect-file))

(defun maildir-index/find (maildir-cache-dir days text-filter)
  ;; if we preserve the ctime of the mails then we have a good way to
  ;; filter  with find
  (format
   (concat
    "find %s "
    "-type f -ctime -%d "
    "-exec grep -qi '%s' \{\} \\; "
    ;; prints the c-time of the file followed by the filename
    "-printf \"%%p\\n\"") ; I was using "%%C@" to print the ctime  
   maildir-cache-dir
   days
   text-filter))

(defun* maildir-index-make (folder-name term &key (days 10))
  "Make FOLDER-NAME an index of TERM in `maildir-mail-dir'. 

DAYS old maildir files are searched for TERM.

If FOLDER-NAME exists just pull new files into it."
  ;; TODO we need a refresh option? to delete all the files in the folder
  (interactive
   (list (read-from-minibuffer "index mail-dir name: ")
         (read-from-minibuffer "index search term: ")))
  (maildir-make-new folder-name)
  (let ((cmd (maildir-index/find
              (maildir/home maildir-mail-dir "cache") days term))
        collected-files)
    (pipe cmd
      (catch :eof
        (apply 'maildir-link
             `(,maildir-mail-dir ,folder-name ,@(pipe-read)))))))

(provide 'maildir-index)

;;; maildir-index.el ends here
