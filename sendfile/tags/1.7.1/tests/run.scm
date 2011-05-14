(use test tcp posix (srfi 4))
(load "test-utils")
(use sendfile)
(import sendfile)

(define the-server (setup))

(test-group "read-write-loop"
      (test "send"
       test-file-size
       (with-prepared-environment test-file
        (lambda (in out)
          (impl:read-write-loop/fd in out test-file-size))))
      (sleep 1)
      (test "verify"
       test-file-checksum
       (compute-file-checksum test-file-out)))

(test-group "read-write-loop (ports-only)"
      (test "send"
        test-file-size
        (with-prepared-environment test-file
         (lambda (in out)
             (impl:read-write-loop/port in out test-file-size)) #t))
      (sleep 1)
      (test "verify"
       test-file-checksum
       (compute-file-checksum test-file-out)))


(if os-dep:sendfile-available?
    (test-group "sendfile-impl"
                (test "send"
                      test-file-size 
                      (with-prepared-environment test-file
                       (lambda (in out)
                         (impl:sendfile in out test-file-size))))
                 (sleep 1)
                 (test "verify"
                       test-file-checksum
                       (compute-file-checksum test-file-out))))

(if os-dep:mmap-available?
    (test-group "mmapped io"

                (test "send"
                      test-file-size 
                      (with-prepared-environment test-file
                       (lambda (in out)
                         (impl:mmapped in out test-file-size))))
                (sleep 1)
                (test "verify"
                      test-file-checksum
                      (compute-file-checksum test-file-out))
                 ;; Disabled until offset works as expected. It makes the tests stall.
                 ;;  
                 ;;(test "offset"
                 ;;      #t
                 ;;      (with-prepared-environment offset-test-file
                 ;;        (lambda (in out)
                 ;;          (impl:mmapped in out offset-test-file-size offset: offset-file-offset)
                 ;;          (equal? (file-contents test-file-out) (file-content-chunk offset-test-file offset-file-offset offset-test-file-size)))))



                ))
                          



(test-begin "interface")

(test-group "sendfile"
      (test "send"
            test-file-size
            (with-prepared-environment test-file
             (lambda (in out)
               (sendfile in out)) #t))
      (sleep 1)
      (test "verify"
            test-file-checksum
            (compute-file-checksum test-file-out)))

(test-end "interface")

(test-begin "forcing implementation")

(test "read-write-loop"
      'read-write-loop
      (with-prepared-environment test-file
                                 (lambda (in out)
                                   (parameterize ((force-implementation 'read-write))
                                     (sendfile in out)
                                     *last-selected-implementation*)) #t))

(if os-dep:sendfile-available?
    (test "sendfile"
          'sendfile
          (with-prepared-environment test-file
                                     (lambda (in out)
                                       (parameterize ((force-implementation 'sendfile))
                                         (sendfile in out)
                                         *last-selected-implementation*)) #t)))
(if os-dep:mmap-available?
    (test "mmapped"
          'mmapped
          (with-prepared-environment test-file
                                     (lambda (in out)
                                       (parameterize ((force-implementation 'mmapped))
                                         (sendfile in out)
                                         *last-selected-implementation*)) #t)))

(test-end "forcing implementation")

(tear-down the-server)
  
