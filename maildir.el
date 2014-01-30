;;; maildir.el --- a full maildir handling tool -*- lexical-binding: t -*-

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Maintainer: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: mail, files
;; Url: http://github.com/nicferrier/emacs-maildir

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
(require 'mm-view)
(require 'rfc2047)
(require 'ietf-drums)
(require 'cl)
(require 'assoc)
(require 'json)
(require 'kv)
(require 'noflet)
(require 's)
(require 'dash)

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

(defcustom maildir-downloads-dir "~/Downloads/"
  "The location of the directory to download files to.

Mail parts that cannot be directly inlined in a buffer are saved
to this directory."
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
(defcustom maildir-message-mode-hook '()
  "The message mode hooks."
  :type 'hook
  :group 'maildir)


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
      (let* ((addr
              (condition-case err
                  (ietf-drums-parse-address header-value)
                (error
                 (progn
                   (message "maildir/index-header-parse %s" err)
                   (cons "" "")))))
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

(defun maildir/pull ()
  "Base function for pulling maildir."
  (let* ((maildir maildir-mail-dir)
         (filelist
          (loop for filename in
               (split-string
                ;; Would prefer to do this async
                (shell-command-to-string
                 ;; FIXME doesn't find just new - needs to.
                 (format "ssh %s find %s -type f -ctime -%d"
                         maildir-remote-host
                         (file-name-as-directory maildir-remote-maildir)
                         maildir-remote-days))
                "\n")
             when (string-match
                   (format "^%s\\(/.*\\)" maildir-remote-maildir)
                   filename)
             collect (match-string 1 filename)))
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
     (format
      "rsync -av --files-from=/tmp/maildircp -e ssh %s:%s %s"
      maildir-remote-host
      maildir-remote-maildir
      maildir-mail-dir))
    ;; Returns the list of new files
    (maildir-import-new maildir)))

;;;###autoload
(defun maildir-pull ()
  "Use ssh and rsync to pull mail from the remote."
  (interactive)
  (maildir/pull))


(defun maildir/split-string (sep-rx str &optional match-group)
  "Split STR with SEP-RX optionally narrowing to MATCH-GROUP.

Eg:

  (split-string \"\\(\n\\)[^ ]\" line 1)

might split on line ending not followed by space, but the
resulting lines will still have whatever character matched the
not-space."
  (let ((to-match (or match-group 0)))
    (noflet ((match-beginning (x)
               (if (equal x 0)
                   (funcall this-fn to-match)
                   (funcall this-fn x)))
             (match-end (x)
               (if (equal x 0)
                   (funcall this-fn to-match)
                   (funcall this-fn x))))
      (split-string str sep-rx))))

(defun maildir/parse-header (buffer)
  "Parse a header in the BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let* ((eoh (save-excursion
                    (re-search-forward "\n\n" nil t)))
             (lines 
              (maildir/split-string
               "\\(\n\\)\\(?:[^\r\t ]\\)"
               (buffer-substring (point) eoh) 1)))
        (-keep (lambda (s)
                 (save-match-data
                   (when (string-match "\\([^:]+\\):\\(.*\\)" s)
                     (cons (intern (downcase (match-string 1 s)))
                           (s-trim (or (match-string 2 s) ""))))))
               lines)))))

(defun maildir/file->header (message-file)
  "Read the MESSAGE-FILE and return it's header.

Disposes of any created buffer."
  (with-current-buffer (find-file-noselect message-file)
    (unwind-protect
         ;;(mail-header-extract)
         (maildir/parse-header (current-buffer))
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

(defvar maildir/from-colors (make-hash-table :test 'equal)
  "Hash of emails to colors.")

(defun maildir/hdr->summary (hdr-alist)
  (propertize
   (format
    "%s  %35s  %s"
    (propertize
     (maildir/format-date (maildir/parse-date (cdr (assq 'date hdr-alist))))
     'face 'message-header-other)
    (let* ((from (cadr (assq 'from hdr-alist)))
           (addr (aget from 'address))
           (existing-color (gethash (downcase addr) maildir/from-colors))
           (color (if existing-color
                      existing-color
                      (let ((new-color (format "#%x" (random #xffffff))))
                        (puthash (downcase addr) new-color maildir/from-colors)
                         new-color))))
      (propertize
       (rfc2047-decode-string (aget from 'address))
       'face `(:foreground ,color)))
    (let ((subject (cdr (assq 'subject hdr-alist))))
      (if subject
          (propertize
           (rfc2047-decode-string subject)
           'face 'message-header-subject)
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


;; Mail viewing

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


(defvar maildir-message-mm-parts nil
  "List of parts of the message in this buffer.")
(make-local-variable 'maildir-message-mm-parts)

(defun maildir-message-part-info ()
  "Tell the user about parts in the current bufffer."
  (interactive)
  (if (not (boundp 'maildir-message-mm-parts))
    (message "there are no parts in this message")
    (message
     "message parts are: %s"
     (let ((i 0))
       (mapconcat
        'identity
        (loop for part in maildir-message-mm-parts
           collect
             (format "[%d] %s" i (if (listp part) (cadr part) part))
           do (setq i (+ 1 i))) "\n")))))

(defvar maildir-message-header-end nil
  "Buffer local end of header marker.")

(define-derived-mode maildir-message-mode message-mode ;; parent
  "Maildir Message"  ; name
  "Maildir Msg \\{maildir-message-mode-map}" ; docstring
  (unless maildir-message/keymap-initializedp
    (define-key maildir-message-mode-map "\C-cr" 'message-reply)
    (define-key maildir-message-mode-map "\C-cw" 'message-wide-reply)
    (define-key maildir-message-mode-map "F" 'maildir-message-fill)
    (define-key maildir-message-mode-map "O" 'maildir-part-open)
    (define-key maildir-message-mode-map "o" 'maildir-message-open-another-part)
    (define-key maildir-message-mode-map "q" 'kill-buffer)
    (define-key maildir-message-mode-map "i" 'maildir-message-part-info)
    ;;(define-key maildir-message-mode-map "p" 'mdmua-message-open-part)
    (setq maildir-message/keymap-initializedp t))
  ;;set the mode as a non-editor mode
  (put 'maildir-message-mode 'mode-class 'special)
  ;;ensure that paragraphs are considered to be whole mailing lists
  (make-local-variable 'paragraph-start)
  (setq paragraph-start paragraph-separate)
  ;; make the local variable for the end of the header
  (make-local-variable 'maildir-message-header-end)
  ;;setup the buffer to be read only
  ;; (make-local-variable 'buffer-read-only)
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  ;;run the mode hooks
  (run-hooks 'maildir-message-mode-hook))


;; Multipart buffer locals

(defvar maildir-message-mm-parent-buffer-name nil
  "Buffer local cache of the parent buffer name.")

(defvar maildir-message-mm-part-number nil
  "Buffer local cache of the current buffer's part number.")

(defvar maildir-message-mm-parts nil
  "Buffer local cache of multipart parts.")

;; Multipart ui

(defun maildir/formatted-header (header-string)
  "Format the header, mostly so we can the drums stuff."
  (loop
     for header-line
     in (split-string header-string "\n")
     concat (if (string-match "^From: \\(.*\\)" header-line)
                (concat
                 "From: "
                 (car (ietf-drums-parse-address
                       (match-string 1 header-line))) "\n")
                ;; Else just the header-line
                (concat header-line "\n"))))

(defun maildir-message-part-next  ()
  (interactive)
  (maildir-message-open-another-part 'next))

(defun maildir-message-part-prev  ()
  (interactive)
  (maildir-message-open-another-part 'previous))

(defun maildir/message-open-external-part (part &optional how)
  "Open PART that cannot be inlined.

HOW, if present, should be a string viewer, from mailcap say.
The HOW, if present, is treated as a shell command and executed."
  (let* ((filename
          (or
           (mail-content-type-get
            (mm-handle-disposition part) 'filename)
           (mail-content-type-get
            (mm-handle-type part) 'name)))
         (path (concat maildir-downloads-dir
                       (file-name-nondirectory filename))))
    (with-temp-buffer 
      (mm-insert-part part)
      (write-file path))
    (if how
        (let ((resolved-file (expand-file-name path)))
          (start-process-shell-command
           (format "*maildir-open-command-%s*" resolved-file)
           (format "*maildir-open-command-%s*" resolved-file)
           (format how resolved-file)))
        ;; Else there was no how, just open it
        (find-file path))))

(defun maildir/linkize (buffer)
  "Convert links inside BUFFER into clickable buttons."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "http[s]*://[^ \t\n]+" nil t)
        (let ((url (match-string 0))
              (beg (match-beginning 0))
              (end (match-end 0)))
          (make-text-button
           beg end
           'label url
           'action (lambda (x) (browse-url url))
           'follow-link t))))))

(defun maildir/message-open-inlineable-part (parent-buffer-name
                                             parts
                                             part-number
                                             header-text)
  "If a part is inlineable this is how it's opened."
  (let ((part (elt parts part-number))
        (part-buffer-name (format
                           "%s[%s]"
                           parent-buffer-name
                           part-number)))
    (with-current-buffer (get-buffer-create part-buffer-name)
      ;; Copy the parent buffer's parts list
      (setq maildir-message-mm-parts parts)
      ;; Insert the content
      (let (end-of-header)
        (let ((buffer-read-only nil))
          (erase-buffer)
          ;; Insert the message
          (insert (maildir/formatted-header header-text))
          (setq end-of-header (point))
          (setq maildir-message-header-end end-of-header)
          (mm-display-part part)
          (maildir/linkize (current-buffer)))
        ;; Now stuff that needs to happen with the ability to set buffer-read-only
        (maildir-message-mode)
        (add-hook
         'kill-buffer-hook
         (lambda ()
           (condition-case err 
               (kill-buffer parent-buffer-name)
             (error nil)))
         nil t)
        (local-set-key ">" 'maildir-message-part-next)
        (local-set-key "<" 'maildir-message-part-prev)
        (switch-to-buffer (current-buffer))
        ;; Make the local var to link us back and to other parts
        (make-local-variable 'maildir-message-mm-parent-buffer-name)
        (setq maildir-message-mm-parent-buffer-name parent-buffer-name)
        (make-local-variable 'maildir-message-mm-part-number)
        (setq maildir-message-mm-part-number part-number)
        (goto-char end-of-header)))))

(defun maildir/message-open-part (parent-buffer-name part-number
                                  &optional how)
  "Open the specified PART-NUMBER from the PARENT-BUFFER-NAME.

The parts are referenced by the buffer local variable
`maildir-message-mm-parts' in the specified PARENT-BUFFER-NAME.

HOW optionally specifies how to open the part and may be a
mailcap reference."
  (with-current-buffer (get-buffer parent-buffer-name)
    (let* ((header-end
            (save-excursion
              (goto-char (point-min))
              (re-search-forward "\n\n" nil t)))
           (header-text (buffer-substring (point-min) header-end))
           (parts maildir-message-mm-parts)
           (gnus-inhibit-images nil) ; hack for a gnus problem
           (part (if (< part-number (length parts))
                     (elt parts part-number)
                     (error "maildir-message: No more parts!"))))
      (if (not (mm-inlinable-p part))
          (maildir/message-open-external-part part)
          (maildir/message-open-inlineable-part
           parent-buffer-name parts part-number header-text)))))

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

(defun maildir/simplify-part-list (part-list)
  "Make the part list simple enough to present the user."
  (let ((simple-1
         (loop for (buffer . part) in part-list
            collect
              (cons
               buffer
               (format "%s" (car part))))))
    (mapcar
     (lambda (p) (cons (cdr p) (car p)))
     simple-1)))

(defun maildir/part-desc->part-idx (part-desc parts)
  (loop for e in (kvalist->keys parts)
     with count = 0
     do (incf count)
     if (equal e part-desc)
     return (- count 1)))

(defun maildir/list-mailcap-viewers (parts idx)
  (let* ((mte (cadr (elt parts idx)))
         (mt (split-string (car mte) "/"))
         (info (kva (car mt) mailcap-mime-data))
         (viewers (mailcap-possible-viewers info (cadr mt))))
    viewers))

(defun maildir-part-open (part-num &optional how)
  "Open the specified PART-NUM.

When used interactively `completing-read's the part list to allow
selection of a part."
  (interactive
   (let* ((c-list
           (maildir/simplify-part-list
            maildir-message-mm-parts))
          (part (completing-read "Part: " c-list))
          (idx (maildir/part-desc->part-idx part c-list)))
     (list
      idx
      (when current-prefix-arg
        (completing-read
         "How (select a mailcap viewer): "
         (mapcar
          (lambda (l)
            (cons  (kva 'viewer l) l))
          (maildir/list-mailcap-viewers
           maildir-message-mm-parts idx)))))))
  (if how
      (maildir/message-open-external-part
       (elt maildir-message-mm-parts part-num) how)
      ;; Else no specific how so work it out
      (maildir/message-open-part
       maildir-message-mm-parent-buffer-name
       part-num)))

(defun maildir/msg-header-fix (end-of-header-pt)
  "Fix `mail-header-extract'.

The function has a bug where it won't read headers with no value.
This is probably bad but we should still read them."
  (save-excursion
    (goto-char (point-min))
    (let ((buffer-read-only nil))
    (while (re-search-forward "^\\([A-Za-z0-9_-]+\\):\n" end-of-header-pt t)
      (backward-char)
      (insert " dummyvalue")
      (save-buffer)))))

(defun maildir/display-inline (header content-type end-of-header-point buffer)
  "Display inline part."
  ;; FIXME - replace this with some emacs function?
  (with-current-buffer buffer
    (when (equal
           (aget header 'content-transfer-encoding)
           "quoted-printable")
      (quoted-printable-decode-region
       end-of-header-point (point-max)))
    (let ((encoding (cdr (cadr content-type)))
          (buffer-read-only nil))
      ;; Not sure whether to mark the region encoded so we don't have
      ;; to do it again
      (when encoding
        (condition-case err
            (decode-coding-region
             end-of-header-point (point-max)
             (intern (downcase encoding)))
          (error (message "maildir/display-inline encode error -- %S" err))))
      (maildir/linkize (current-buffer)))))

(defun maildir/flatten-parts (part)
  (let ((sign (car part)))
    (if (and (stringp sign)
             (string-match-p "multipart/.*" sign))
        (loop for p in (cdr part)
           append (maildir/flatten-parts p))
        ;; else
        (list part))))

(defun maildir/mimetype-index (parts mime-type-regex)
  "Find the index of the specified MIME-TYPE-REGEX.

PARTS is a part list as returned by `mm-disect-buffer'.  Each
part is either a string mime-type or a list describing a part
where the cadr is the mime-type."
  (let ((i 0))
    (loop for part in parts
       if (string-match-p mime-type-regex (caadr part))
       return i
       do (setq i (+ i 1)))))

(defun maildir-open (filename)
  "Open the specified FILENAME."
  (interactive
   (list
    (plist-get (text-properties-at (point)) :filename)))
  (with-current-buffer (find-file-noselect filename)
    (let* ((end-of-header
            (save-excursion
              (re-search-forward "\n\n" nil t)))
           (header
             (save-excursion
               (goto-char (point-min))
               (maildir/msg-header-fix end-of-header)
               (mail-header-extract)))
           (content-type (mail-header-parse-content-type
                          (or (aget header 'content-type)
                              "text/plain"))))
      ;; Decide what to do based on type
      (if (string-match-p "multipart/.*" (car content-type))
          (let* ((parts (mm-dissect-buffer))
                 (pl (maildir/flatten-parts parts))
                 (parent-buffer-name (buffer-name)))
            (setq maildir-message-mm-parts pl)
            (maildir/message-open-part
             parent-buffer-name (maildir/mimetype-index pl "text/.*")))
          ;; Else it's a normal mail
          ;;
          ;; FIXME why can't we find an mm- func that does what this does?
          ;; - how do you dissect a non-multipart message to get a handle?
          (maildir/display-inline
           header content-type end-of-header (current-buffer))
          (switch-to-buffer (current-buffer))
          (maildir-message-mode)
          (setq maildir-message-header-end end-of-header)
          (goto-char maildir-message-header-end)))))

;; Maildir list stuff

(defvar maildir/buffer-mail-dir nil
  "Buffer-local used for references to a buffer's maildir.")

(defun maildir-quit ()
  "Quit the current maildir."
  (interactive)
  (kill-buffer))

;;;###autoload
(defun maildir-refresh ()
  "Refresh the maildir."
  (interactive)
  (message "maildir refreshing...")
  (maildir-pull)
  (maildir-list (or maildir/buffer-mail-dir maildir-mail-dir) t))

(defun maildir-mode-next-field ()
  (interactive)
  (re-search-forward "[^ ] "))


(defvar maildir-isearch-overlays nil
  "Set by isearch extensions to the list of overlays.")

(defun maildir/isearch-caller (to-call)
  "Return a function to call TO-CALL within an isearch.

When TO-CALL is called the variable `maildir-isearch-overlays' is
set to the list of overlays that isearch found."
  (lambda ()
    (interactive)
    (let ((maildir-isearch-overlays
           (loop for o in (overlays-in (point-min) (point-max))
              if (eq (overlay-get o 'face) 'lazy-highlight)
              collect o)))
      (call-interactively to-call))))

(defvar maildir/isearch-keymap nil
  "The keymap containing extra things we enable during isearch.")

(defun maildir/isearch-hook-jack-in ()
  "To be called by the isearch hook to connect our keymap."
  (unless maildir/isearch-keymap
    (setq maildir/isearch-keymap (make-sparse-keymap))
    (set-keymap-parent maildir/isearch-keymap isearch-mode-map)
    (define-key maildir/isearch-keymap (kbd "M-m")
      (maildir/isearch-caller 'maildir-move)))
  (setq overriding-terminal-local-map maildir/isearch-keymap))

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
    (define-key maildir-mode-map "F" 'maildir-find-file)
    (define-key maildir-mode-map "n" 'next-line)
    (define-key maildir-mode-map "p" 'previous-line)
    (define-key maildir-mode-map "\t" 'maildir-mode-next-field)
    (define-key maildir-mode-map "d" 'maildir-rm)
    (define-key maildir-mode-map "q" 'maildir-quit)
    (define-key maildir-mode-map "r" 'maildir-refresh)
    (define-key maildir-mode-map "g" 'maildir-refresh)
    (define-key maildir-mode-map "m" 'maildir-move)
    (define-key maildir-mode-map "+" 'maildir-make-new)
    (setq maildir-mode/keymap-initialized-p t)))

(defun maildir-find-file ()
  "Find the message file at point."
  (interactive)
  (find-file (get-text-property (point) :filename)))

;;;###autoload
(defun maildir-list (mail-dir &optional clear)
  "List the maildir."
  ;; TODO how to resolve the location of a specified mail-dir?
  (interactive
   (list maildir-mail-dir))
  (let ((clear t)
        (buf (get-buffer-create
              (if (equal mail-dir maildir-mail-dir)
                  "*maildir*"
                  (format "*maildir-%s*" mail-dir)))))
    (with-current-buffer buf
      (let ((buffer-read-only nil))
        (when clear (erase-buffer))
        (let ((index-list (maildir-index mail-dir)))
          (insert
           (mapconcat 'maildir/hdr->summary index-list "\n")
           "\n"))
        ;; For display of maildir folders this (point-max) will have
        ;; to be the start of the folder list
        (sort-lines t (point-min) (point-max)))
      (switch-to-buffer buf)
      (maildir-mode)
      ;; Now set the buffer local maildir pointer
      (make-local-variable 'maildir/buffer-mail-dir)
      (setq maildir/buffer-mail-dir mail-dir)
      (add-hook 'isearch-mode-hook 'maildir/isearch-hook-jack-in t t)
      (goto-char (point-min)))))

(defun maildir/new-maildir (name &optional base-maildir)
  "Make a new maildir NAME.

BASE-MAILDIR is optional and specifies the base maildir to create
the new maildir in.  `maildir-mail-dir' is used by default."
  (let ((base (if (file-exists-p base-maildir) base-maildir maildir-mail-dir)))
    (make-directory (format "%s/.%s/cur" base name) t)
    (make-directory (format "%s/.%s/new" base name) t)))

(defun maildir-make-new (name)
  "Make a new maildir."
  (interactive "Mnew maildir name: ")
  ;; FIXME we could make mail-dir buffer local and then we could find
  ;; this in the buffer.
  (maildir/new-maildir name maildir-mail-dir)
  (message "maildir: created %s" name))

(defun maildir/directory-p (dir)
  (and (file-directory-p dir) dir))

(defun maildir/list-maildirs (&optional base-maildir)
  "List the maildirs under a particular BASE-MAILDIR.

By default list `maildir-mail-dir'."
  (let ((base (if (file-exists-p base-maildir) base-maildir maildir-mail-dir)))
    (mapcar 'maildir/directory-p
            (directory-files base t "\\.[A-Za-z0-9-_]+"))))

(defun maildir/directory->pair (directory)
  "Convert a directory to a cons: basename . directory"
  (cons
   (file-name-nondirectory directory)
   directory))

(defvar maildir/move-history nil
  "History of maildir move folders.")

(defun maildir/complete-folder ()
  (let ((pairs
         (mapcar
          'maildir/directory->pair
          (maildir/list-maildirs maildir/buffer-mail-dir))))
    (aget pairs
          (completing-read
           "move to folder: "
           pairs nil t
           (car maildir/move-history) 'maildir/move-history))))

(defun maildir-move (to-folder &rest filename-list)
  "Move the files in FILENAME-LIST to TO-FOLDER.

When called interactively this moves the message at `point'.

When called interactively from inside an incremental search this
moves all the messages the search is highlighting."
  (interactive
   (if maildir-isearch-overlays
       (cons (maildir/complete-folder)
             (loop for o in maildir-isearch-overlays
                collect (get-text-property (overlay-start o) :filename)))
       ;; Else it's probably just one, in a buffer
       (list
        (maildir/complete-folder)
        (get-text-property (point) :filename))))
  ;; Do we need to kill the current line?
  (loop for filename in filename-list
     do
       (condition-case err
           (let ((pos (text-property-any
                       (point-min) (point-max)
                       :filename filename)))
             (rename-file
              filename
              (format
               "%s/cur/%s"
               to-folder (file-name-nondirectory filename)))
             (when pos
               (let (buffer-read-only
                     (kill-whole-line t))
                 (save-excursion
                   (goto-char (line-beginning-position))
                   (kill-line)))))
         (error (message
                 "maildir-move %S while moving %S"
                 (car err) filename)))))

;;;###autoload
(defun maildir-cache-find (text maildir-root &optional find-spec)
  "Use `find-dired' to find TEXT in the MAILDIR-ROOT"
  (interactive
   (list
    (read-from-minibuffer "search maildir cache for: ")
    (or maildir/buffer-mail-dir maildir-mail-dir)
    (let ((find-spec "-type f -ctime -2 -exec grep -qi %s \\{\\} \\;"))
      (if current-prefix-arg
          (read-from-minibuffer "search maildir with: " find-spec)
          find-spec))))
  (find-dired (format "%s/cache" maildir-root)
              (format find-spec text)))

(provide 'maildir)

;;; maildir.el ends here
