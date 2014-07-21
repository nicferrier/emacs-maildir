


;; gets the messages from the cache for a day ago
(let ((adayago (time-subtract (current-time) (seconds-to-time (* 24 (* 60 60))))))
  (-filter
   (lambda (it)
     (time-less-p 
      adayago
      (if (elt (file-attributes it) 6)
          (elt (file-attributes it) 5)
          adayago)))
   (directory-files
    (expand-file-name "~/mymaildir/var/maildir/nferrier/cache")
    t "^[^.].*[^~]$")))
