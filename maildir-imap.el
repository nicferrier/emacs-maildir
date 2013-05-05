;;; maildir-imap.el - imap sync for maildir

(require 'imap)

(defcustom maildir/imap-source nil
  "An IMAP source for pulling mails from."
  :type '(choice
          (const :tag "Default" nil)
          (string :tag "Url" "imap.gmail.com"))
  :group 'maildir)

(defun maildir/new-filename (maildir)
  "Makes a new filename for MAILDIR.

MAILDIR is a maildir location.

It returns a list of a new file created on the MAILDIR and the
maildir unique filename with the expectation that you will rename
the created file to the new filename.

We do this to preserve the i-node of the created file."
  (destructuring-bind (bigtime littletime)
      (split-string (format "%f" (float-time)) "\\.")
    (let ((name (make-temp-name (file-name-directory maildir))))
      (with-temp-file name)
      (list
       name
       (format
        "%s.I%sM%s.%s"
        bigtime
        (elt (file-attributes name) 10)
        littletime (system-name))))))

(defun maildir/new-file (maildir filename)
  "Make a new file for MAILDIR and FILENAME."
  (expand-file-name
   (format "%snew/%s"
           (file-name-as-directory maildir)
           filename)))

(defvar maildir/imap-connection nil
  "IMAP connection for syncing.")

(defun maildir/imap-month-ago ()
  "Make an IMAP string of the date 35 days ago."
  (upcase
   (format-time-string
    "%d-%h-%Y"
    (time-subtract
     (current-time)
     (seconds-to-time
      (* 35 (* 24 (* 60 60))))))))

(defun maildir/imap-check-connect ()
  "Check and connect to IMAP if necessary."
  (unless (eq 'run
              (condition-case nil
                  (process-status
                   (get-buffer-process
                    maildir/imap-connection))
                (error nil)))
    (setq maildir/imap-connection
          (imap-open
           maildir/imap-source
           ;; TODO: How to get this dependant on the maildir/imap-source?
           993 'ssl 'login))
    (imap-login-auth maildir/imap-connection)
    (imap-mailbox-select "INBOX" t maildir/imap-connection)
    (imap-mailbox-status "INBOX" 'messages maildir/imap-connection)))

(defun maildir/imap-search ()
  "Do the IMAP search for maildir.

Presumes `maildir/imap-connection' is made."
  (imap-search
   (format "UNSEEN SINCE %s" (maildir/imap-month-ago))
   maildir/imap-connection))

(defvar maildir/imap-message-doit nil
  "Whether to actually do message creation or not.")

(defvar maildir/imap-log nil
  "The log buffer used for when we're not doing actual pulling.")

(defun maildir/imap-message (msg)
  "Do a single IMAP message."
  (with-temp-buffer ; (get-buffer-create "*imapscratch*")
    (destructuring-bind (file filename)
        (maildir/new-filename maildir-mail-dir)
      (let ((new-filename
             (maildir/new-file maildir-mail-dir filename))
            (coding-system-for-write 'no-conversion)
            (msg-uid (imap-fetch
                      msg "(RFC822 UID FLAGS)" 'UID nil
                      maildir/imap-connection)))
        (unless (member "\\Seen"
                        (imap-message-get
                         msg-uid 'FLAGS maildir/imap-connection))
          (insert (imap-message-get
                   msg-uid 'RFC822 maildir/imap-connection))
          ;; And mark it seen
          (imap-message-flags-add (number-to-string msg-uid)
                                  "SEEN" t maildir/imap-connection)
          (if (not maildir/imap-message-doit) ; debug mode, quite useful
              (let* ((hdr (progn
                            (goto-char (point-min))
                            (mail-header-extract)))
                     (log-msg 
                      (format "%s from %s with %s\n"
                              (aget hdr 'subject)
                              (aget hdr 'from)
                              (imap-message-get msg-uid 'FLAGS maildir/imap-connection))))
                (with-current-buffer maildir/imap-log
                  (insert log-msg)))
              ;; Else really do it
              (rename-file file new-filename)
              (write-file new-filename)))))))

(defun maildir/imap-sync (&optional doit)
  "Sync `maildir/imap-source' with `maildir-mail-dir'.

Note that if you want to use other maildir's than the standard
one you should wrap this with a function to let-bind
`maildir-mail-dir'.

If DOIT is specified actually do the creation of maildir entries
in `maildir-mail-dir', otherwise just create a log of what would
be done and display it."
  (interactive
   (list current-prefix-arg))
  (if (not maildir/imap-source)
      (error "no `maildir/imap-source' is configured, configure one to sync.")
      ;; Reconnect to imap if necessary
      (maildir/imap-check-connect)
      (let ((maildir/imap-log (get-buffer-create "*maildir-imap-log*"))
            (maildir/imap-message-doit doit))
        (mapcar 'maildir/imap-message (maildir/imap-search)))))

;;(maildir/imap-sync "~/mymaildir/var/maildir/nferrier")

;;; maildir-imap.el ends here
