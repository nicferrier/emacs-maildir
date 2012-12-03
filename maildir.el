;;; maildir.el --- a full maildir handling tool

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Maintainer: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: mail, files
;; Version: 0.0.3

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

;; Handling maildirs with Emacs.

;;; Code:

(require 'mailheader)
(require 'mm-decode)
(require 'rfc2047)
(require 'ietf-drums)
(require 'cl)

(defgroup maildir nil
  "The Maildir mail user agent application."
  :group 'applications)

(defcustom maildir-default-index-field-syms
  '(to from date subject)
  "The default list of field symbols for the indexer."
  :group 'maildir
  :type 'sexp)

(defcustom maildir-mail-dir ""
  "The location of the Maildir you want to use."
  :group 'maildir
  :type 'directory)

(defcustom maildir-remote-host ""
  "The host to connect to to find emails."
  :group 'maildir
  :type 'string)

(defcustom maildir-remote-maildir ""
  "The host to connect to to find emails."
  :group 'maildir
  :type 'string)

(defcustom maildir-remote-days 5
  "The number of days to check on the remote, for new emails."
  :group 'maildir
  :type 'integer)

(defun maildir-log (data)
  (with-current-buffer (get-buffer-create "*maildir-log*")
    (save-excursion
      (goto-char (point-max))
      (cond
        ((listp data)
         (insert (format "%S" data)))
        (t
         (insert data))))))

;; Message stuff

(require 'qp)

;; The keymap for the message view mode
(defvar maildir-message/keymap-initializedp nil
  "Is the mode map initialized yet?

If you want to debug the mode map you can set this back to nil
and it should get reinitialized next time you make the mode.")

;; Hooks for the message mode
(defvar maildir-message-mode-hook nil
  "The message mode hooks.")

(defun maildir-message-fill ()
  "Allow filling of a paragraph even when read only.

MDMUA message buffers are read only but paragraphs are sometimes
not formatted properly so we provide this command to allow you to
fill them.

Also causes the buffer to be marked not modified."
  (interactive)
  (let ((buffer-read-only nil))
    (fill-paragraph)
    (set-buffer-modified-p nil)))

(define-derived-mode maildir-message-mode message-mode ;; parent
  "Maildir Message"  ; name
  "Maildir Msg \\{maildir-message-mode-map}" ; docstring
  (unless maildir-message/keymap-initializedp
    (define-key maildir-message-mode-map "\C-ca" 'message-reply)
    (define-key maildir-message-mode-map "\C-cw" 'message-wide-reply)
    (define-key maildir-message-mode-map "F" 'maildir-message-fill)
    (define-key maildir-message-mode-map "q" 'kill-buffer)
    ;;(define-key maildir-message-mode-map "p" 'mdmua-message-open-part)
    (setq maildir-message/keymap-initializedp t))
  ;;set the mode as a non-editor mode
  (put 'maildir-message-mode 'mode-class 'special)
  ;;ensure that paragraphs are considered to be whole mailing lists
  (make-local-variable 'paragraph-start)
  (setq paragraph-start paragraph-separate)
  ;;setup the buffer to be read only
  ;; (make-local-variable 'buffer-read-only)
  (setq buffer-read-only 't)
  (set-buffer-modified-p nil)
  ;;run the mode hooks
  (run-hooks 'maildir-message-mode-hook))


;; Maildir parsing stuff

(defun maildir/index-header-parse (header-name header-value)
  "Parse the HEADER-PAIR.

Often does nothing but for some header fields (such as Date) it
changes the value in some way."
  (cons
   header-name
   (cond
     ((eq 'date header-name)
      (format-time-string
       "%Y%m%d%H%M%S"
       (apply
        'encode-time
        (parse-time-string header-value))))
     ((memq header-name '(from to))
      (let* ((addr (condition-case err
                       (ietf-drums-parse-address header-value)
                     (error (cons "" ""))))
             (address (or (car addr) ""))
             (name (or (cdr addr) "")))
        (list (list (cons 'address address) (cons 'name name)))))
     (t header-value))))

(defun maildir/home (mail-dir &optional sub)
  "Return the properly expanded dir."
  (file-name-as-directory
   (expand-file-name
    (concat
     (file-name-as-directory mail-dir)
     (if sub sub "")))))

(defun maildir/file-name->mail (filename &optional do-info extra-info)
  "See http://cr.yp.to/proto/maildir.html"
  (let ((fname (file-name-nondirectory filename)))
    (unless (string-match
             "\\([^.]+\\)\\.\\([^.]+\\)\\.\\(.*\\)\\(:[12]\\(.*\\)\\)*"
             fname)
      (error "filename '%s' is broken" fname))
    (format
     "%s.%s.%s%s"
     (match-string-no-properties 1 fname) ; time
     (match-string-no-properties 2 fname) ; delivery identifier
     (match-string-no-properties 3 fname) ; delivery hostname
     (let ((info (match-string-no-properties 4 fname)))
       (case (or do-info :nothing)
         (:nothing "")
         ;; Add new info to the existing
         (:add (apply
                'concat
                ":2,"
                (match-string-no-properties 5 fname)
                (when extra-info (list extra-info))))
         (t (if info info "")))))))

(defun maildir/exists-p (filename maildir)
  (let ((md-name (maildir/file-name->mail filename t)))
    (file-exists-p
     (concat
      (file-name-as-directory maildir)
      (file-name-as-directory "cache")
      md-name))))

(defun maildir-import-new (mail-dir)
  "Scan the maildir/new directory and import stuff."
  (interactive (list maildir-mail-dir))
  (let ((new (maildir/home mail-dir "new"))
        (cache (maildir/home mail-dir "cache"))
        (cur (maildir/home mail-dir "cur")))
    ;; Make the dirs if we need them
    (unless (file-exists-p cur) (mkdir cur t))
    (unless (file-exists-p cache) (mkdir cache t))
    (loop
       for filename
       in (directory-files new 't "^[^.]+")
       collect
         (let* ((base (maildir/file-name->mail filename t))
                (cur-base (maildir/file-name->mail filename :add))
                (cache-file (concat cache base))
                (cur-file (concat cur cur-base)))
           ;; First move the new file to the cache
           (rename-file filename cache-file)
           ;; then symlink it into cur
           (make-symbolic-link cache-file cur-file)
           cur-file))))

;;;###autoload
(defun maildir-pull ()
  "Just memorize what we did for now."
  (interactive)
  (let* ((maildir maildir-mail-dir)
         (filelist (split-string
                    ;; Would prefer to do this async
                    (shell-command-to-string
                     ;; FIXME doesn't find just new - needs to.
                     (format "ssh %s find %s  -type f -ctime -%d"
                      maildir-remote-host
                      (file-name-as-directory maildir-remote-maildir)
                      maildir-remote-days))
                    "\n"))
         (new-mails
          ;; Produces the list of files we don't have in the maildir's cache
          (loop
             for file in filelist
             if (string-match ".+" file)
             unless (maildir/exists-p file maildir)
             collect file)))
    ;; What are they?
    (maildir-log new-mails)
    ;; Make a filelist of them
    (with-current-buffer (find-file-noselect "/tmp/maildircp")
      (erase-buffer)
      (loop
         for file in new-mails
         do (progn (insert file) (insert "\n")))
      (save-buffer))
    ;; Rsync the filelist to the maildir/new - Would prefer to do this async
    (shell-command-to-string
     (concat
      "rsync -av --files-from=/tmp/maildircp "
      "-e ssh " maildir-remote-host ":/ " ; the filelist identifies the files
      ;; FIXME - very wrong!!! this needs to be the maildir somehow
      "~/mymaildir"))
    ;; Returns the list of new files
    (maildir-import-new maildir)))

(defun maildir/file->header (message-file)
  "Read the MESSAGE-FILE and return it's header.

Disposes of any created buffer."
  (with-current-buffer (find-file-noselect file)
    (unwind-protect
         (mail-header-extract)
      (kill-buffer (current-buffer)))))

(defun maildir/index-header (file)
  "Convert MESSAGE-FILE to an index list."
  (append
   (list (cons 'file file))
   (loop for (hdr-name . hdr-value) in (maildir/file->header file)
      if (memq hdr-name maildir-default-index-field-syms)
      collect (maildir/index-header-parse hdr-name hdr-value))))

(defun maildir-index (mail-dir &optional field-symbols)
  "Make an index of the specified MAIL-DIR.

Optionally use FIELD-SYMBOLS as the list of header field symbols
to produce the index for.  By default this is
`maildir-default-index-field-syms'."
  (loop
     with collect-file = nil
     for file in (directory-files (maildir/home mail-dir "cur") 't "^[^.]+")
     do
       (setq collect-file (maildir/index-header file))
     if collect-file
     if (and (assq 'date collect-file)
             (assq 'to collect-file)
             (assq 'from collect-file))
     collect collect-file))

(defun maildir-lisp ()
  (let ((buf (get-buffer-create "*maildir*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert
       (mapconcat
        (lambda (hdr-alist)
          (json-encode hdr-alist))
        (maildir-index "~/testmaildir/maildir/nic")
        "\n")))
    (switch-to-buffer buf)))

(defun maildir/parse-date (date-str)
  "Return a list of the following elements:

  year month day hour min seconds+anything else

Each value is a number."
  (let ((date-string date-str))
    (string-match
     (concat
      "\\([0-9]\\{4\\}\\)" ; year
      "\\([0-9]\\{2\\}\\)" ; month
      "\\([0-9]\\{2\\}\\)" ; day
      "\\([0-9]\\{2\\}\\)" ; hour
      "\\([0-9]\\{2\\}\\)" ; minute
      "\\([0-9]*\\)") ; rest
     date-string)
    (loop for i from 1 to 6
       collect
         (string-to-number
          (match-string-no-properties i date-string)))))

(defun maildir/format-date (date-list)
  (apply 'format "%4d-%02d-%02d %02d:%02d:%02d" date-list))

(defun maildir/hdr->summary (hdr-alist)
  (propertize
   (format
    "%s  %35s  %s"
    (maildir/format-date (maildir/parse-date (cdr (assq 'date hdr-alist))))
    (let ((from (cadr (assq 'from hdr-alist))))
      (rfc2047-decode-string (aget from 'address)))
    (let ((subject (cdr (assq 'subject hdr-alist))))
      (if subject
          (rfc2047-decode-string subject)
          "")))
   :filename (aget hdr-alist 'file)))

(defun maildir-rm (filename)
  "Remove the specified FILENAME."
  (interactive
   (list
    (plist-get (text-properties-at (point)) :filename)))
  (let ((p (point))
        (l (line-beginning-position)))
    (save-excursion
      (goto-char l)
      (let ((kill-whole-line t)
            (buffer-read-only nil))
        (kill-line)))
    (let ((new-point (+ (point) (- p l))))
      (if (> new-point (line-end-position))
          (goto-char (line-end-position))
          (goto-char new-point)))
    (delete-file filename)))


;; Multipart buffer locals

(defvar maildir-message-mm-parent-buffer-name nil
  "Buffer local cache of the parent buffer name.")

(defvar maildir-message-mm-part-number nil
  "Buffer local cache of the current buffer's part number.")

(defvar maildir-message-mm-parts nil
  "Buffer local cache of multipart parts.")

;; Multipart ui

(defun maildir/message-open-part (parent-buffer-name part-number)
  (with-current-buffer (get-buffer parent-buffer-name)
    (let* ((header-end
            (save-excursion
              (goto-char (point-min))
              (re-search-forward "\n\n" nil t)))
           (header-text (buffer-substring (point-min) header-end))
           (parts maildir-message-mm-parts)
           (part (if (< part-number (length parts))
                     (elt parts part-number)
                     (error "maildir-message: No more parts!"))))
      (with-current-buffer
          (get-buffer-create
           (format "%s[%s]" parent-buffer-name part-number))
        (let ((buffer-read-only nil))
          (erase-buffer)
          ;; Insert the message
          (insert header-text)
          (let ((end-of-header (point)))
            (insert
             (with-current-buffer (car part) ; the part buffer
               (buffer-substring-no-properties (point-min) (point-max))))
            (when (eq (elt part 2) 'quoted-printable)
              (quoted-printable-decode-region end-of-header (point-max)))
            (maildir-message-mode)
            (local-set-key ">" (lambda ()
                                 (interactive)
                                 (maildir-message-open-another-part 1)))
            (local-set-key "<" (lambda ()
                                 (interactive)
                                 (maildir-message-open-another-part -1)))
            (switch-to-buffer (current-buffer))
            ;; Make the local var to link us back and to other parts
            (make-local-variable 'maildir-message-mm-parent-buffer-name)
            (setq maildir-message-mm-parent-buffer-name parent-buffer-name)
            (make-local-variable 'maildir-message-mm-part-number)
            (setq maildir-message-mm-part-number part-number)
            (goto-char (point-min))))))))

(defun maildir-message-open-another-part (&optional which)
  "Open a different part than this one.

You must be focused on a part buffer, see `maildir/message-open-part'.

WHICH can be `next' or `previous' or a number to indicate a
specific part.  The default is `next'."
  (interactive)
  (unless which (setq which 'next))
  (maildir/message-open-part
   maildir-message-mm-parent-buffer-name
   (cond
     ((eq which 'next)
      (+ 1 maildir-message-mm-part-number))
     ((eq which 'previous)
      (- maildir-message-mm-part-number 1))
     ((numberp which)
      (+ maildir-message-mm-part-number which))
     (t
      (+ 1 maildir-message-mm-part-number)))))

(defun maildir-open (filename)
  "Open the specified FILENAME."
  (interactive
   (list
    (plist-get (text-properties-at (point)) :filename)))
  (with-current-buffer (find-file-noselect filename)
    (let* ((header
             (save-excursion
               (goto-char (point-min))
               (mail-header-extract)))
           (content-type (mail-header-parse-content-type
                          (aget header 'content-type)))
           (end-of-header
            (save-excursion
              (re-search-forward "\n\n" nil t))))
      ;; Decide what to do based on type
      (if (string-match "multipart/.*" (car content-type))
          (let ((parts (mm-dissect-buffer))
                (parent-buffer-name (buffer-name)))
            (make-local-variable 'maildir-message-mm-parts)
            (setq maildir-message-mm-parts parts)
            (maildir/message-open-part parent-buffer-name 1))
          ;; Else it's a normal mail
          (when (equal
                 (aget header 'content-transfer-encoding)
                 "quoted-printable")
            (quoted-printable-decode-region end-of-header (point-max)))
          (switch-to-buffer (current-buffer))
          (maildir-message-mode)))))

(defun maildir-quit ()
  "Quit the current maildir."
  (interactive)
  (kill-buffer))

(defun maildir-refresh ()
  "Refresh the maildir."
  (interactive)
  (message "maildir refreshing...")
  (maildir-pull)
  (maildir-list t))

(defvar maildir-mode/keymap-initialized-p nil
  "Whether or not the keymap has been initialized.")

(define-derived-mode maildir-mode nil "Maildir"
  "Major mode for using maildirs.

\\<maildir-mode-map>
"
  :group 'maildir
  (setq buffer-read-only t)
  (unless maildir-mode/keymap-initialized-p
    (define-key maildir-mode-map "\r" 'maildir-open)
    (define-key maildir-mode-map "d" 'maildir-rm)
    (define-key maildir-mode-map "q" 'maildir-quit)
    (define-key maildir-mode-map "r" 'maildir-refresh)
    (setq maildir-mode/keymap-initialized-p t)))

;;;###autoload
(defun maildir-list (&optional clear)
  (interactive)
  (let ((clear t)
        (buf (get-buffer-create "*maildir*")))
    (with-current-buffer buf
      (let ((buffer-read-only nil))
        (when clear (erase-buffer))
        (let ((index-list (maildir-index maildir-mail-dir)))
          (insert
           (mapconcat 'maildir/hdr->summary index-list "\n")
           "\n"))
        (sort-lines t (point-min) (point-max)))
      (switch-to-buffer buf)
      (maildir-mode))))

(provide 'maildir)

;;; maildir.el ends here
