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

(defun maildir/imap-sync (maildir)
  (interactive)
  (if (not maildir/imap-source)
      (error "no `maildir/imap-source' is configured, configure one to sync.")
      ;; Reconnect to imap if necessary
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
        (imap-mailbox-status "INBOX" 'messages maildir/imap-connection))
      (mapcar
       (lambda (msg)
         (with-temp-buffer
           (destructuring-bind (file filename)
               (maildir/new-filename maildir)
             (let ((new-filename
                    (expand-file-name
                     (format "%snew/%s"
                             (file-name-as-directory maildir) filename)))
                   (coding-system-for-write 'no-conversion))
               (insert (imap-fetch
                        msg "(RFC822)" 'RFC822 nil
                        maildir/imap-connection))
               (rename-file file new-filename)
               (write-file new-filename)))))
       (imap-search
        (format "SINCE %s" (maildir/imap-month-ago))
        maildir/imap-connection))))

;;(maildir/imap-sync "~/mymaildir/var/maildir/nferrier")

;;; maildir-imap.el ends here
