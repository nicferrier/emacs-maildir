;;; tests for maildir

(require 'maildir)

(ert-deftest maildir/split-string ()
  (should
   (equal
    (maildir/split-string "\\(\n\\)[^ ]" "blah\nblah" 1)
    (list "blah" "blah"))))

(ert-deftest maildir--index-header-parse-date()
  "Test the index header parsing of dates."
  (should
   (equal
    '(date . "20110726135116")
    (maildir--index-header-parse
     (cons 'date "Tue, 26 Jul 2011 15:51:16 +0300")))))

(ert-deftest maildir--index-header-parse-to()
  "Test the index header parsing of the to address."
  (should
   (equal
    `(to . ((address . "nferrier@ferrier.me.uk")
            (name . "Nic Ferrier")))
    (maildir--index-header-parse
     (cons 'to "\"Nic Ferrier\" <nferrier@ferrier.me.uk>")))))

(ert-deftest maildir/calendar-month-name->number ()
  "Test the calendar conversion stuff."
  (should
   (equal 
    (maildir/calendar-month-name->number "Jan")
    1))
  (should
   (equal 
    (maildir/calendar-month-name->number "January")
    1))
  (should-not
   (maildir/calendar-month-name->number "Doh")))

;;; tests.el ends here
