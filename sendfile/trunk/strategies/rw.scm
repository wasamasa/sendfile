(define sys-seek (foreign-lambda int "lseek" integer integer int))
(define-foreign-variable seek-set int "SEEK_SET")

(define (impl:read-write-loop/port src dst offset bytes)
  (set!  *last-selected-implementation* 'read-write-loop)
  
  (let* ((buffsize (read-write-buffer-size))
         (buffer (make-string buffsize))
         (seek (foreign-lambda int "lseek" integer integer int)))
    
    (when (positive? offset)
      (sys-seek src offset seek-set))
    
    (let loop ((bytes-left bytes) (bytes-read 0))
      (if (not (positive? bytes-left))
          bytes-read
          (let* ((to-read (fxmin buffsize (inexact->exact bytes-left)))
                 (read-bytes (cadr (file-read src to-read buffer))))
            (display (substring buffer 0 read-bytes) dst)
            (loop (- bytes-left read-bytes) (+ bytes-read read-bytes)))))))




(define (impl:read-write-loop/fd src dst offset bytes)
  (set!  *last-selected-implementation* 'read-write-loop)
  
  (let* ((buffsize (read-write-buffer-size))
         (buffer (make-string buffsize))
         (write-timeout (write-timeout))
         (write/offset (foreign-lambda* int ((int dst) (nonnull-scheme-pointer buff) (unsigned-integer write_offset) (unsigned-integer bytes))
                         "C_return(write(dst,buff + write_offset,bytes));"))
         (write-bytes (lambda (size)
                        (let loop ((bytes-left size) (write_offset 0))
                          (when (positive? bytes-left)
                            (let ((written-bytes (write/offset dst buffer write_offset bytes-left)))
                              (cond
                               ((zero? bytes-left) #t)
                               ((and (negative? written-bytes) (= errno/again (##sys#update-errno)))
                                (when write-timeout
                                  (##sys#thread-block-for-timeout!
                                   ##sys#current-thread
                                   (+ (current-milliseconds) write-timeout)))
                                (##sys#thread-block-for-i/o! ##sys#current-thread dst #:output)
                                (%yield)
                                (when (##sys#slot ##sys#current-thread 13)
                                  (complain #f "write operation timed out"))
                                (loop bytes-left write_offset))
                               ((negative? written-bytes)
                                (complain #t "write failed"))
                               (else (loop (fx- bytes-left written-bytes) (fx+ write_offset written-bytes))))))))))
    (when (positive? offset)
      (sys-seek src offset seek-set))
    
    (let loop ((bytes-left bytes) (bytes-read 0))
      (if (not (positive? bytes-left))
          bytes-read
          (let* ((to-read (fxmin buffsize (inexact->exact bytes-left)))
                 (read-bytes (cadr (file-read src to-read buffer))))
            (write-bytes read-bytes)
            (loop (- bytes-left read-bytes) (+ bytes-read read-bytes)))))))
