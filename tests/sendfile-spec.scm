(load "spec-helper.scm")

(define (megabytes n)
  (* n 1024 1024))

(define (transmits-correctly)
  (let ((server-side-checksum #f)
        (expected-checksum #f))
    (matcher
     (check (subject)
            (with-running-server
             (lambda ()
               (call-with-temporary-file/checksum (force subject)
                                                  (lambda (file-path checksum)
                                                    (set! expected-checksum checksum)
                                                    (set! server-side-checksum (stream-file file-path (string-length (force subject))))
                                                    (string=? server-side-checksum checksum))))))
     (message (form subject negate)
              (with-output-to-string
                (lambda ()
                  (printf "Checksum missmatch. Expected ~A but got ~A" expected-checksum server-side-checksum)))))))



   
  (context "Sendfile"
    (it "can send files greater than 1 mb" (meta ((bug . 727)))
        (expect (generate-buffer (megabytes 2)) (transmits-correctly))))



