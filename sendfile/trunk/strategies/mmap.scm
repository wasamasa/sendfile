(define write-timeout (make-parameter #f))

(cond-expand
  (madvise
   (foreign-declare "#include<sys/mman.h>")
   (define-foreign-variable %madvise-will-need int "MADV_WILLNEED")
   (define (%madvise buff len behav)
     ((foreign-lambda int "madvise" (c-pointer char) unsigned-integer int) buff len behav)))
  (else
   (define %madvise-will-need #f)
   (define (%madvise . args) #t)))


(define-foreign-variable sc-page-size int "_SC_PAGE_SIZE")
(define sys:page-size ((foreign-lambda long "sysconf" int) sc-page-size))
(define sys:write (foreign-lambda integer "write" integer c-pointer unsigned-integer))

(define (impl:mmapped src dst offset bytes)
  (set!  *last-selected-implementation* 'mmapped)
  (chunk-for-each (cut send-chunk dst <> <> <>) src offset bytes))

;; map the bytes starting at offset and ending at offset+bytes
;; into memory, by mapping %current-chunk-size bytes at a time
(define (chunk-for-each proc src offset bytes)
  (let ((page-size sys:page-size)
        (mmap-offset offset)
        (ptr-offset #f)
        (offset offset)
        (target-offset (+ offset bytes))
        (write-timeout (write-timeout)))
    ;(printf "~%1 mmap-offset: ~A ptr-offset: ~A offset: ~A target-offset: ~A ~%" mmap-offset ptr-offset offset target-offset)
    
    ;; ensure page-alignment
    (when (positive? offset)
      (cond
       ((> offset page-size)
        (let ((mod (modulo offset page-size)))
          (unless (zero? mod)
            (set! mmap-offset (quotient offset page-size))
            (set! ptr-offset mod))))
       (else
        (set! ptr-offset offset)
        (set! mmap-offset 0))))
    
    ;(printf "2 mmap-offset: ~A ptr-offset: ~A offset: ~A target-offset: ~A ~%" mmap-offset ptr-offset offset target-offset)

    (let loop ((offset offset) (bytes-written 0) (mmap-offset mmap-offset) (ptr-offset ptr-offset))
      (if (= offset target-offset)
          bytes-written
          ;;now map chunks until we have mapped the data we wanted
          (let* ((chunk-size (next-chunk-size (or ptr-offset offset) target-offset))
                 (mem-file   (map-file-to-memory #f chunk-size prot/read map/shared src (or mmap-offset offset)))
                 (pointer    (memory-mapped-file-pointer mem-file)))
            (if ptr-offset
                (proc (pointer-offset pointer ptr-offset) chunk-size write-timeout)
                (proc pointer chunk-size write-timeout))
            (unmap-file-from-memory mem-file)
            (loop (+ offset chunk-size) (+ bytes-written chunk-size) #f #f))))))

(define (send-chunk dst ptr size write-timeout)
  ;;don't bother advices for data smaller than 64k
  (when (>= size (kilobytes 64)) (%madvise ptr size %madvise-will-need))
  ;(printf "Shall writ: ~A bytes starting at: ~A" size ptr )
  (let loop ((bytes-left size) (work-ptr (pointer-offset ptr 0)))
    (if (zero? bytes-left)
        #t
        (let ((result (sys:write dst work-ptr bytes-left)))
          (cond
           ((and (negative? result) (= errno/again (##sys#update-errno)))
            (when write-timeout
              (##sys#thread-block-for-timeout!
               ##sys#current-thread
               (+ (current-milliseconds) write-timeout)))
            (##sys#thread-block-for-i/o! ##sys#current-thread dst #:output)
            (%yield)
            (when (##sys#slot ##sys#current-thread 13)
              (complain #f "write operation timed out"))
            (loop bytes-left work-ptr)) ;retry
           ((negative? result)
            (complain #f "write failed"))
           (else
            (loop (- bytes-left result) (pointer-offset work-ptr result))))))))

