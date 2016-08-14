;;; maildir-xml.el --- convert html mail to text

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

;; this came about from looking at a joseph turner email

;;; Code:

(require 's)
(require 'kv)

(defun maildir-xml-l (l)
  (--keep
   (cond
     ((stringp it) it)
     ((eq (car it) 'style))
     (t (maildir-xml-e it)))
   (cddr l)))

(defun maildir-xml->str (l)
  (mapconcat
   'identity
   (-flatten
    (-keep
     (lambda (it)
       (cond
         ((stringp it)
          (unless (equal it "    ")
            (mapconcat 's-trim (split-string it "\n") " ")))))
     (-flatten l))) "\n"))

(defun maildir-xml-e (e)
  (when (symbolp (car e))
    (case (car e)
      (html (nic-xml-e (assoc 'body (cddr e))))
      ((body table tr td tbody) (maildir-xml-l e))
      (a (format "[%s|%s]"
                 (kva 'href (cadr e))
                 (maildir-xml->str (maildir-xml-l e))))
      (img (format "{{%s}}" (kva 'src (cadr e))))
      (strong (s-trim (elt e 2)))
      (p (maildir-xml->str (maildir-xml-l e)))
      (span (maildir-xml->str (maildir-xml-l e))))))

(defun maildir-xml/xml->text (buffer)
  (interactive
   (list
    (when (re-search-forward "<html>" nil t)
      (current-buffer))))
  (let ((sxml
         (with-current-buffer buffer
           (libxml-parse-html-region (point) (point-max)))))
    (with-current-buffer (get-buffer-create "*htmlmail*")
      (erase-buffer)
      (princ (maildir-xml->str (maildir-xml-e sxml)) (current-buffer))
      (goto-char (point-min))
      (switch-to-buffer-other-window (current-buffer)))))

(provide 'maildir-xml)

;;; maildir-xml.el ends here
