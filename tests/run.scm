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
               (test "sendfile" mb-checksum (stream-mb-buffer)))

   (test-group "forcing implementation"
               
               (parameterize ((force-implementation 'read-write))
                 (test "read-write" mb-checksum (stream-mb-buffer)))

               (if sendfile-available
                   (parameterize ((force-implementation 'sendfile))
                     (test "sendfile(2)" mb-checksum (stream-mb-buffer))))

               (if mmap-available
                   (parameterize ((force-implementation 'mmapped))
                     (test "mmap(2)" mb-checksum (stream-mb-buffer))))

               (parameterize ((force-implementation 'read-write-port))
                 (test "read-write-port" mb-checksum (stream-mb-buffer))))


   (test-group "read-write variations"
               
               (call-with-temporary-file/checksum
                (generate-buffer (mebibytes 1))
                (lambda (temp-file expected-checksum)
                  (test "ports only"
                        expected-checksum
                        (call-with-connection-to-server
                         (lambda (server-in server-out)
                           (write-content-size server-out (mebibytes 1))
                           (call-with-input-file temp-file
                             (lambda (file-in)
                               (sendfile file-in server-out)))
                           (close-output-port server-out)
                           (read-checksum server-in)))))))


   (test-group "content chunking"
               
      (let* ((head   (generate-buffer 20 #\a))
             (middle (generate-buffer 20 #\b))
             (tail   (generate-buffer 20 #\c))
             (buffer (string-append head middle tail)))         

        (define (chunking-test proc) 
         (call-with-connection-to-server
          (lambda (server-in server-out)
            (let ((input (open-input-string buffer)))
              (write-content-size server-out -1)
              (proc input server-out)
              (close-output-port server-out)
              (read-checksum server-in)))))

        (define-syntax test-chunking
          (syntax-rules ()
            ((_ implementation)
             (test-group implementation
               (parameterize ((force-implementation (string->symbol implementation)))          
                 (test "offsets"
                       (buffer-checksum tail)
                       (chunking-test
                        (lambda (input output)
                          (sendfile input output offset: 40))))
                 (test "size"
                       (buffer-checksum head)
                       (chunking-test
                        (lambda (input output)
                          (sendfile input output bytes: 20))))
             
                 (test "size and offset"
                       (buffer-checksum middle)
                       (chunking-test
                        (lambda (input output)
                          (sendfile input output offset: 20 bytes: 20)))))))))
        
        (when sendfile-available
          (test-chunking "sendfile"))

        (when mmap-available
          (test-chunking "mmapped"))

        (test-chunking "read-write")
        
        (test-chunking "read-write-port")))

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
                          (close-output-port server-out)
                          (read-checksum server-in))))))
  
   
              (call-with-temporary-file/checksum
               (generate-buffer (mebibytes 2))
               (lambda (temp-file expected-checksum)
                 (test "send files > 1 mebibyte"
                       expected-checksum
                       (stream-file temp-file sendfile)))))))


(unless (zero? (test-failure-count)) (exit 1))

