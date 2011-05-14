(use test tcp posix (srfi 4))
(load "test-utils")
(use sendfile)
(import sendfile)

(define the-server (setup))

(test-begin "Chunking")

(when sendfile-available
  (test-group "sendfile"
              (test "giving offsets"
                    "ent (unofficial) maintainer of Scheme->C\n"
                    (begin
                      (with-prepared-environment
                       offset-test-file
                       (lambda (in out)
                         (parameterize ((force-implementation 'sendfile))
                           (sendfile in out offset: 1400))) #t)
                      (file-contents test-file-out)))
            
              (test "giving size"
                    " Scheme occupies a unique niche." 
                    (begin
                      (with-prepared-environment
                       offset-test-file
                       (lambda (in out)
                         (parameterize ((force-implementation 'sendfile))
                           (sendfile in out bytes: 32))) #t)
                      (file-contents test-file-out)))
            
              (test "giving size and offset"
                    "It is a mindset" 
                    (begin
                      (with-prepared-environment
                       offset-test-file
                       (lambda (in out)
                         (parameterize ((force-implementation 'sendfile))
                           (sendfile in out offset: 213 bytes: 15))) #t)
                      (file-contents test-file-out)))

            
              ;; (test "giving too high offset" #t #f)
              ;; (test "giving too low offset" #t #f)
              ;; (test "giving too high size" #t #f)
              ;; (test "giving too low size" #t #f)
              ))


(when mmap-available
 (test-group "mmap"
             (test "giving offsets"
                   "ent (unofficial) maintainer of Scheme->C\n"
                   (begin
                     (with-prepared-environment
                      offset-test-file
                      (lambda (in out)
                        (parameterize ((force-implementation 'mmapped))
                          (sendfile in out offset: 1400))) #t)
                     (file-contents test-file-out)))
             (test "giving size"
                   " Scheme occupies a unique niche." 
                   (begin
                     (with-prepared-environment
                      offset-test-file
                      (lambda (in out)
                        (parameterize ((force-implementation 'mmapped))
                          (sendfile in out bytes: 32))) #t)
                     (file-contents test-file-out)))
            
             (test "giving size and offset"
                   "It is a mindset" 
                   (begin
                     (with-prepared-environment
                      offset-test-file
                      (lambda (in out)
                        (parameterize ((force-implementation 'mmapped))
                          (sendfile in out offset: 213 bytes: 15))) #t)
                     (file-contents test-file-out)))

             ;; (test "giving offset > page-size")
             ;; (test "giving offset > page-size and size > page-size)
            
             ;; (test "giving too high offset" #t #f)
             ;; (test "giving too low offset" #t #f)
             ;; (test "giving too high size" #t #f)
             ;; (test "giving too low size" #t #f)
             ))

(test-group "read-write-loop/fd"
            (test "giving offsets"
                  "ent (unofficial) maintainer of Scheme->C\n"
                  (begin
                    (with-prepared-environment
                     offset-test-file
                     (lambda (in out)
                       (parameterize ((force-implementation 'read-write))
                         (sendfile in out offset: 1400))) #t)
                    (file-contents test-file-out)))

            (test "giving size"
                  " Scheme occupies a unique niche." 
                  (begin
                    (with-prepared-environment
                     offset-test-file
                     (lambda (in out)
                       (parameterize ((force-implementation 'read-write))
                         (sendfile in out bytes: 32))) #t)
                    (file-contents test-file-out)))

            (test "giving size and offset"
                  "It is a mindset" 
                  (begin
                    (with-prepared-environment
                     offset-test-file
                     (lambda (in out)
                       (parameterize ((force-implementation 'read-write))
                         (sendfile in out offset: 213 bytes: 15))) #t)
                    (file-contents test-file-out)))

            ;; test non-page-aligned offsets
            ;; test bigger chunks
            
            ;; (test "giving too high offset" #t #f)
            ;; (test "giving too low offset" #t #f)
            ;; (test "giving too high size" #t #f)
            ;; (test "giving too low size" #t #f)
            
            )


(test-group "read-write-loop/port"
            (test "giving offsets"
                  "ent (unofficial) maintainer of Scheme->C\n"
                  (begin
                    (with-prepared-environment
                     offset-test-file
                     (lambda (in out)
                       (parameterize ((force-implementation 'read-write-port))
                         (sendfile in out offset: 1400))) #t #f)
                    (file-contents test-file-out)))

            (test "giving size"
                  " Scheme occupies a unique niche." 
                  (begin
                    (with-prepared-environment
                     offset-test-file
                     (lambda (in out)
                       (parameterize ((force-implementation 'read-write-port))
                         (sendfile in out bytes: 32))) #t #f)
                    (file-contents test-file-out)))

            (test "giving size and offset"
                  "It is a mindset" 
                  (begin
                    (with-prepared-environment
                     offset-test-file
                     (lambda (in out)
                       (parameterize ((force-implementation 'read-write-port))
                         (sendfile in out offset: 213 bytes: 15))) #t #f)
                    (file-contents test-file-out)))

            
            
            ;; (test "giving too high offset" #t #f)
            ;; (test "giving too low offset" #t #f)
            ;; (test "giving too high size" #t #f)
            ;; (test "giving too low size" #t #f)

            )

(test-end "Chunking")

(test-group "read-write-loop"
      (test "send"
       test-file-size
       (with-prepared-environment test-file
        (lambda (in out)
          (impl:read-write-loop/fd in out 0 test-file-size)) #f #f))
      (sleep 1)
      (test "verify"
       test-file-checksum
       (compute-file-checksum test-file-out)))

(test-group "read-write-loop (ports-only)"
      (test "send"
        test-file-size
        (with-prepared-environment test-file
         (lambda (in out)
             (impl:read-write-loop/port in out 0 test-file-size)) #t #f))
      (sleep 1)
      (test "verify"
       test-file-checksum
       (compute-file-checksum test-file-out)))


(if sendfile-available
    (test-group "sendfile-impl"
                (test "send"
                      test-file-size 
                      (with-prepared-environment test-file
                       (lambda (in out)
                         (impl:sendfile in out 0 test-file-size)) #f #f))
                 (sleep 1)
                 (test "verify"
                       test-file-checksum
                       (compute-file-checksum test-file-out))))

(if mmap-available
    (test-group "mmapped io"

                (test "send"
                      test-file-size 
                      (with-prepared-environment test-file
                       (lambda (in out)
                         (impl:mmapped in out 0 test-file-size)) #f #f))
                (sleep 1)
                (test "verify"
                      test-file-checksum
                      (compute-file-checksum test-file-out))))
                          



(test-begin "interface")

(test-group "sendfile"
      (test "send"
            test-file-size
            (with-prepared-environment test-file
             (lambda (in out)
               (sendfile in out)) #t #f))
      (sleep 1)
      (test "verify"
            test-file-checksum
            (compute-file-checksum test-file-out)))




(test-group "custom input port without fd [bug #542]"
            (let ((test-content "I'm content from a custom port"))
              (test "send"
                    (string-length test-content)
                    (with-prepared-environment test-file
                                               (lambda (ignored out)
                                                 (sendfile
                                                  (open-input-string test-content)
                                                  out)) #t #t))))



(test-end "interface")

(test-begin "forcing implementation")

(test "read-write-loop"
      'read-write-loop
      (with-prepared-environment test-file
                                 (lambda (in out)
                                   (parameterize ((force-implementation 'read-write))
                                     (sendfile in out)
                                     *last-selected-implementation*)) #t))

(if sendfile-available
    (test "sendfile"
          'sendfile
          (with-prepared-environment test-file
                                     (lambda (in out)
                                       (parameterize ((force-implementation 'sendfile))
                                         (sendfile in out)
                                         *last-selected-implementation*)) #t)))
(if mmap-available
    (test "mmapped"
          'mmapped
          (with-prepared-environment test-file
                                     (lambda (in out)
                                       (parameterize ((force-implementation 'mmapped))
                                         (sendfile in out)
                                         *last-selected-implementation*)) #t)))

(test-end "forcing implementation")



(tear-down the-server)
  
(unless (zero? (test-failure-count)) (exit 1))
