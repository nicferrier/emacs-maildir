;;; maildir-index-test.el --- tests for maildir-index functions



(insert "X-Original-To: nferrier@ferrier.me.uk
Delivered-To: nferrier@ferrier.me.uk
Received: from mail-pb0-f54.google.com (mail-pb0-f54.google.com [209.85.160.54])
	by po1.ferrier.me.uk (Postfix) with ESMTPS id C3DDDAC0334
	for <nferrier@ferrier.me.uk>; Thu, 23 Jan 2014 12:40:34 +0100 (CET)
Received: by mail-pb0-f54.google.com with SMTP id uo5so1727197pbc.41
        for <nferrier@ferrier.me.uk>; Thu, 23 Jan 2014 03:40:10 -0800 (PST)
DKIM-Signature: v=1; a=rsa-sha256; c=relaxed/relaxed;
        d=aiesec.co.uk; s=google;
        h=mime-version:in-reply-to:references:date:message-id:subject:from:to
         :cc:content-type;
        bh=8KNbvURKilGGyqtaj46KJarM0sgeAPgXm1HyH8bGWJQ=;
        b=RAeaZe6RPEpmI3BRpA+R7gPm3c4KYIkBmVGs+ufK+DzKY2Q3aUqK2PJkLoGP5I3PCZ
         O2jFwdJZaA+5ai+66N7QgQb/KX/W2WmoObgEFep2NpratMRQHLx3+NZGC9rm1ZlqMkn6
         qgsV3IyDTUk+GOolw7ece781eeoHn5Y3FtbXA=
X-Google-DKIM-Signature: v=1; a=rsa-sha256; c=relaxed/relaxed;
        d=1e100.net; s=20130820;
        h=x-gm-message-state:mime-version:in-reply-to:references:date
         :message-id:subject:from:to:cc:content-type;
        bh=8KNbvURKilGGyqtaj46KJarM0sgeAPgXm1HyH8bGWJQ=;
        b=kh4ZTw60mnkORLqmSZYJ3i+gXn2Sxu0OwE1DsobRGuLey0b/hq89uN9AlXBFkdpjWI
         cyDYUfPWK69E4hDQEA109phSA6adhhC6bEz3wuK6tLsHFYrS1ja3bGf8uXqrfQwb32Io
         LwwMhQecHZOUPWl3vLXTry0iP+06CVweJkbYNhkeOn3TpBQ5XZ3e3q+D2PnoUse0eiaI
         S9f686AsEfoiN3O9MXpAAA+5IQWEUUgqjVPaIeBXhG3NpMIzzP91VjXjhLpyV+qoHnEg
         QjQTRaHOJYKdMKrsasY3ChgarchB5UzG1FJSJ4punyziF9tdM5hHEld5WhXkhL5bTdYe
         jolw==
X-Gm-Message-State: ALoCoQlb7YLA4QjmV7fZmqFnXhNdCb37YHR2QJHgJMSBFnPXMVc1E7rt9A+4wOS7htAjS7BDAvZs
MIME-Version: 1.0
X-Received: by 10.68.34.37 with SMTP id w5mr7411835pbi.159.1390477210529; Thu,
 23 Jan 2014 03:40:10 -0800 (PST)
Received: by 10.70.25.225 with HTTP; Thu, 23 Jan 2014 03:40:10 -0800 (PST)
In-Reply-To: <CA+3c+qqhyT_pG8tOd_Pd9NzHMRw9UWJkn4v6xAN=oA8Dk1TWnw@mail.gmail.com>
References: <87a9enga3a.fsf@ferrier.me.uk>
	<CA+3c+qqhyT_pG8tOd_Pd9NzHMRw9UWJkn4v6xAN=oA8Dk1TWnw@mail.gmail.com>
Date: Thu, 23 Jan 2014 11:40:10 +0000
Message-ID: <CAHictWYCwFjVke8=w49wZ_-=hkGdCwi6buu=U-MciD0+=oPtMQ@mail.gmail.com>
Subject: Re: conference in feb
From: nicola@aiesec.co.uk
To: Reuben Ayley <reuben@aiesec.co.uk>
Cc: Nic Ferrier <nferrier@ferrier.me.uk>
Content-Type: multipart/alternative; boundary=bcaec520ef7b33ca0f04f0a1b4f1

")

(with-current-buffer
    (get-buffer
     (find-file-noselect
      (concat
       "~/mymaildir/var/maildir/nferrier/cur/"
       "1391096025.V902Ibe1b1aM155292.Ubuntu-1110-oneiric-64-minimal:2,"))))

(with-temp-buffer
  (insert "X-Original-To: nferrier@ferrier.me.uk
Received: from mail-pb0-f54.google.com (mail-pb0-f54.google.com [209.85.160.54])
	by po1.ferrier.me.uk (Postfix) with ESMTPS id C3DDDAC0334
	for <nferrier@ferrier.me.uk>; Thu, 23 Jan 2014 12:40:34 +0100 (CET)
MIME-Version: 1.0
Date: Thu, 23 Jan 2014 11:40:10 +0000
Subject: Re: conference in feb
From: nicola@aiesec.co.uk
To: Reuben Ayley <reuben@aiesec.co.uk>
Content-Type: multipart/alternative; boundary=bcaec520ef7b33ca0f04f0a1b4f1

")
  (nic-parse-header (current-buffer)))



(provide 'maildir-index-test)
;;; maildir-index-test.el ends here
