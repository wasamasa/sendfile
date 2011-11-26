;; 
;; %%HEADER%%
;; 

(use test)
(load "test-helper")
(use sendfile)


(with-running-server

 (let* ((mb-buffer (generate-buffer (mebibytes 1)))
        (mb-checksum (buffer-checksum mb-buffer)))


   (define (stream-mb-buffer)
     (call-with-temporary-file/checksum
      mb-buffer
      (lambda (temp-file _)
        (stream-file temp-file sendfile))))
 
   (test-group "sendfile main interface"
               (test "sendfile"
                     mb-checksum
                     (stream-mb-buffer)))


   (test-group "forcing implementation"
               (parameterize ((force-implementation 'read-write))
                 (test "read-write"
                       mb-checksum
                       (stream-mb-buffer))))

   (test-group "bugs"               
               (call-with-buffer/checksum
                (kibibytes 1)
                (lambda (buffer checksum)
                  (test "custom input port without fd [bug #542]"
                        checksum
                        (call-with-connection-to-server
                         (lambda (server-in server-out)
                           (write-content-size server-out (kibibytes 1))
                           (sendfile (open-input-string buffer) server-out)
                           (read-checksum server-in))))))
   
    
               (call-with-temporary-file/checksum
                (generate-buffer (mebibytes 2))
                (lambda (temp-file expected-checksum)
                  (test "send files > 1 mibibyte"
                        expected-checksum
                        (stream-file temp-file sendfile)))))))

