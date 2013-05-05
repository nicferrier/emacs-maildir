;;; maildir-message-handlers.el - things to do when messages open

(require 'maildir)

(defcustom maildir-outlook-meetings-diary-file nil
  "The diary file to use for outlook meetings."
  :type '(choice (const :tag "Default" nil)
		 (file :tag "File"))
  :group 'maildir)

(defun maildir/calendar-month-name->number (month-name)
  "Convert the specified MONTH-NAME to the 1th based month number.

Eg: January is 1.

Also works with the abbreviated forms.  See
`calendar-month-name-array' for how month names are matched and
`calendar-month-abbrev-array' for abbrevations."
  ;; This should go into diary or calendar and be fixed in diary.el
  (let ((month (catch 'found
                 (dotimes (i (length calendar-month-name-array))
                   (if (or
                        (string-equal
                         (downcase
                          (aref calendar-month-name-array i))
                         (downcase month-name))
                        (string-equal
                         (downcase (aref calendar-month-abbrev-array i))
                         (downcase month-name)))
                       (throw 'found (1+ i))))
           nil))) month))

;; Another Outlook date format
;;
;;   ("^When: \\([0-9]\\{1,2\\}\\) \\(\\w+\\) \\([0-9]\\{4\\}\\) \\([0-9:]\\{5\\}\\)-\\([0-9:]\\{5\\}\\) \\(.*\\)"
;;      . diary-outlook-format-2))))
;;
;; Needs to go into Emacs
(defun diary-outlook-format-2 (body)
  (let* ((day (match-string 1 body))
         (month-name (match-string 2 body))
         (month (maildir/calendar-month-name->number month-name))
         (year (match-string 3 body))
         (time-start (match-string 4 body))
         (time-stop (match-string 5 body))
         (rest (match-string 6 body)))
    (concat
     (cond ((eq calendar-date-style 'iso) "\\3 \\2 \\1") ; YMD
           ((eq calendar-date-style 'european) "\\1 \\2 \\3") ; DMY
           (t "\\2 \\1 \\3")) ; MDY
     "\n \\4 %s, \\6\n")))

(defun maildir-invitation-to-diary (buffer)
  "Detect and note Outlook invitations."
  (when maildir-outlook-meetings-diary-file
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (diary-from-outlook-internal 
         (mail-fetch-field "Subject")
         (buffer-substring-no-properties
          maildir-message-header-end
          (point-max)))
        (with-current-buffer (get-buffer "diary")
          (save-buffer))))))

;;; maildir-message-handlers.el ends here
