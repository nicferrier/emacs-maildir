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

;;;###autoload
(defun* maildir-index-make (folder-name term &key (days 10))
  "Make FOLDER-NAME an index of TERM in `maildir-mail-dir'. 

DAYS old maildir files are searched for TERM.

If FOLDER-NAME exists just pull new files into it."
  ;; TODO we need a refresh option? to delete all the files in the folder
  (interactive
   (let ((reads 
          (list (read-from-minibuffer "index mail-dir name: ")
                (read-from-minibuffer "index search term: "))))
     (if current-prefix-arg
         (append reads (list :days (string-to-number
                                    (read-from-minibuffer "days to index: "))))
         reads)))
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