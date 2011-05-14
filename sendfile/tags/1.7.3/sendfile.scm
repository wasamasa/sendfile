;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sendfile Egg for the Chicken Scheme system.
;;              
;; This eggs provides a capability to utilize
;; the sendfile system-call. However it is
;; not a mere wrapper to call this function if
;; available, but rather its semantics may be stated as:
;;
;; "Send the file as fast as possible to its destination."
;;
;; Please report bugs to <http://trac.callcc.org/>
;;
;; Copyright (c) 2007 David Krentzlin 
;;
;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;sendfile attempts to send a file from the sourcefile out to a tcp-socket
;;as fast as possible. On systems that provide sendfile(2) syscalls
;;this syscall will be used if apropriate on other systems memory-mapped write
;;will be used to emulate this. And if even this fails a normal system-write
;;will be used to send the data


;;TODO

(require-library lolevel posix extras srfi-4)
(foreign-declare "#include \"os-dep.h\"\n")

(module sendfile
  (force-implementation
   *last-selected-implementation*
   read-write-buffer-size
   implementation-selector
   impl:mmapped
   impl:sendfile
   impl:read-write-loop/fd
   impl:read-write-loop/port
   os-dep:sendfile-available?
   os-dep:mmap-available?
   sendfile)
  (import scheme chicken posix srfi-4 (only lolevel pointer-offset) foreign)
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;we need to know the wordsize to calculate the
  ;;biggest representable fixnum. This is used as the
  ;;size to be passed to sendfile
  (define-foreign-variable %word-size int "C_WORD_SIZE")

  ;;is the sendfile(2) avilable?
  (define-foreign-variable %have-native-sendfile int "HAVE_SENDFILE")
  (define os-dep:sendfile-available? (= %have-native-sendfile 1))

  ;;is mmap nativly available?
  (define-foreign-variable %have-mmap int "HAVE_MMAP")
  (define os-dep:mmap-available? (= %have-mmap 1))

  
  ;;system-specific defines that are used to fine-tune somethings if
  ;;they are available

  (define-foreign-variable %madvise-sequential int "MADV_SEQUENTIAL")
  (define-foreign-variable %madvise-will-need int "MADV_WILLNEED")

  
  (define-foreign-variable %bufsize int "BUFSIZ")

  
  ;;the buffer used in read write loops
  ;;the client may adjust this to meet its need
  (define read-write-buffer-size (make-parameter %bufsize))

  ;;advise the kernel for a specific buffer
  ;;this is used in  mmapped io if supported
  (define (%madvise buff len behav)
    ((foreign-lambda int "madvise" (c-pointer char) unsigned-integer int) buff len behav))

  
  ;;the sendfile implementation
  ;;note that we pass a flonum instead of an integer as we need
  ;;to be able to represent abritrary sizes on non-64bit systems as well
  ;;also (file-size) currently returns a float for big-files
  (define %sendfile-implementation
    (foreign-lambda double "sendfile_implementation" int int double unsigned-integer))



  ;;some helpers that make things cleaner
  (define (kilobytes num) (* num 1024))
  (define (megabytes num)  (* (kilobytes num) 1024))

  ;;the current chunk-size specifies how big the slices are that
  ;;we read/write in the three scenarios. This is parameterized
  ;;because different methods to send the file may work better with
  ;;differently sized chunks
  (define %current-chunk-size (make-parameter (kilobytes 512)))

  ;;compute the next chunk to send out of offset and the length
  ;;of the remaining buffer. This is really just a convenience-procedure
  ;;that uses (the possibly parameterized) (chunk-zize)
  (define (%next-chunk-size len offset)
    (if (> (- len offset) (%current-chunk-size))
        (%current-chunk-size)
        (- len offset)))

  (define *last-selected-implementation* #f)

  (define write-timeout (make-parameter #f))

  ;;copied from tcp
  (define (%yield)
    (##sys#call-with-current-continuation
     (lambda (return)
       (let ((ct ##sys#current-thread))
         (##sys#setslot ct 1 (lambda () (return (##core#undefined))))
         (##sys#schedule) ) ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Conditions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define strerror (foreign-lambda c-string "strerror" int))

  (define (make-exn-condition location message arguments)
    (apply make-property-condition
           'exn
           (append
            (if location (list 'location location) '())
            (if message (list 'message message) '())
            (if (and arguments (not (null? arguments))) (list 'arguments arguments) '()))) )

  (define (make-sendfile-condition location message arguments)
    (make-composite-condition
     (make-exn-condition location message arguments)
     (make-property-condition 'sendfile)) )

  (define (errno-argument)
    (let ((err (errno)))
      (if (zero? err)
          '()
          (let ((str (strerror err)))
            (if (or (not str) (zero? (string-length str)))
                (list (number->string err))
                (list str) ) ) ) ) )

  (define (%error msg . args)
    (abort (make-sendfile-condition #f msg (append (errno-argument) args))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The three implementations follow now.
;; sendfile implements three distinct strategies to send the file over the wire.
;; Which method is used depends on the system's capabilities and the size of the file
;; 1) it uses sendfile(2)
;; 2) it uses mmapped-io. This means chunks of the file are mmapped into the process-memory
;;    and written to the socket
;; 3) it uses read-writes repeatetly. This is the simplest (NOT the slowest in all cases) strategy.
;;    It simply reads a chunk of the file and writes it out to the socket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;MMAPPED-SEND
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (impl:mmapped src dst len #!key (offset 0.0))
    (set!  *last-selected-implementation* 'mmapped)
    
    (unless os-dep:mmap-available?
      (%error  "mmap is not available on this system"))
    
    (define sys:write (foreign-lambda integer "write" integer c-pointer unsigned-integer))

    (define (send-chunk ptr size write-timeout)
      ;;don't bother adivices for data smaller than 64k
      (when (> size (kilobytes 64)) (%madvise ptr size %madvise-will-need))
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
                  (%error "write operation timed out"))
                (loop bytes-left work-ptr)) ;retry
               ((negative? result) #f)
               (else
                (loop (- bytes-left result) (pointer-offset work-ptr result))))))))
    
    (parameterize ((%current-chunk-size (kilobytes 512)))
      (let loop ((offset offset))
        (cond
         ((= offset len) len)
         (else
          (let* ((next-chunk (%next-chunk-size len offset))
                 (mem-file (map-file-to-memory #f next-chunk prot/read map/shared src offset))
                 (write-timeout (write-timeout)))
            (unless (send-chunk (memory-mapped-file-pointer mem-file) next-chunk write-timeout)
              (unmap-file-from-memory mem-file)
              (##sys#update-errno)
              (%error "write-failed"))
            (unmap-file-from-memory mem-file)
            (loop (+ offset next-chunk))))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SENDFILE(2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (impl:sendfile src dst len )
    (set!  *last-selected-implementation* 'sendfile)
    
    (unless os-dep:sendfile-available?
      (%error "sendfile is not available on this system"))
    
    (parameterize ((%current-chunk-size (inexact->exact (- (expt 2 (- %word-size 3)) 1))))
      (let loop ((offset 0.0))
        (cond
         ((= offset len)  len)
         (else
          (let* ((next-chunk (%next-chunk-size len offset))
                 (new-offset (%sendfile-implementation src dst offset next-chunk)))
            (cond
             ((eqv? -2.0 new-offset)  ; EAGAIN/EINTR
              (when (write-timeout)
                (##sys#thread-block-for-timeout!
                 ##sys#current-thread
                 (+ (current-milliseconds) (write-timeout))))
              (##sys#thread-block-for-i/o! ##sys#current-thread dst #:output)
              (%yield)
              (when (##sys#slot ##sys#current-thread 13)
                (%error "write operation timed out"))
              (loop offset))
             ((negative? new-offset)
              (##sys#update-errno)
              (%error "sendfile failed"))
             (else
              (loop new-offset)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;READ-WRITE-LOOP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  (define (impl:read-write-loop/port src dst len)
    (set!  *last-selected-implementation* 'read-write-loop)
    
    (let* ((buffsize (read-write-buffer-size))
           (buffer (make-string buffsize)))
      (let loop ((n len))
        (if (not (positive? n))
            len
            (let* ((to-read (fxmin buffsize (inexact->exact n)))
                   (read-bytes (cadr (file-read src to-read buffer))))
              ;(display (substring buffer 0 (sub1 read-bytes)))
              (display (substring buffer 0 read-bytes) dst)
              (loop (- n read-bytes)))))))

  
  (define (impl:read-write-loop/fd src dst len)
    (set!  *last-selected-implementation* 'read-write-loop)
    
    (let* ((buffsize (read-write-buffer-size))
           (buffer (make-string buffsize))
           (write-timeout (write-timeout))
           (write/offset (foreign-lambda* int ((int dst) (nonnull-scheme-pointer buff) (unsigned-integer offset) (unsigned-integer bytes))
                                          "C_return(write(dst,buff + offset,bytes));"))
           (write-bytes (lambda (size)
                          (let loop ((left size) (offset 0))
                            (let ((written-bytes (write/offset dst buffer offset left)))
                              (cond
                               ((zero? left) #t)
                               ((and (negative? written-bytes) (= errno/again (##sys#update-errno)))
                                (when write-timeout
                                  (##sys#thread-block-for-timeout!
                                   ##sys#current-thread
                                   (+ (current-milliseconds) write-timeout)))
                                (##sys#thread-block-for-i/o! ##sys#current-thread dst #:output)
                                (%yield)
                                (when (##sys#slot ##sys#current-thread 13)
                                  (%error "write operation timed out"))
                                (loop left offset))
                               ((negative? written-bytes)
                                (##sys#update-errno)
                                (%error "write failed"))
                               (else (loop (fx- left written-bytes) (fx+ offset written-bytes)))))))))
      (let loop ((n len))
        (if (not (positive? n))
            len
            (let* ((to-read (fxmin buffsize (inexact->exact n)))
                   (read-bytes (cadr (file-read src to-read buffer))))
              (write-bytes read-bytes)
              (loop (- n read-bytes)))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The single interface procedure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (default-selector len)
    (cond
     ((< len (megabytes 1)) impl:read-write-loop/fd)
     (os-dep:sendfile-available? impl:sendfile)
     (os-dep:mmap-available? impl:mmapped)
     (else impl:read-write-loop/fd)))
  
  
  (define (port-has-fd? obj)
    (unless (port? obj)
      (%error "supplied argument is not a port"))
    (handle-exceptions exn #f (port->fileno obj) #t))

  (define (->fileno obj)
    (cond
     ((fixnum? obj) obj)
     ((port? obj) (port->fileno obj))
     (else (%error "supplied argument is neither port nor descriptor"))))
  
  ;;set to either 'sendfile 'mmapped 'read-write or 'nothing
  (define force-implementation (make-parameter 'nothing))

  (define implementation-selector (make-parameter default-selector))

  (define (sendfile src dst)
    (let* ((src (->fileno src))
           (size (file-size src)))
      (if (and (port? dst) (not (port-has-fd? dst)))
          (impl:read-write-loop/port src dst size)
          (begin
            (flush-output dst) ; Implementations below use non-buffered I/O
            (let ((dst (->fileno dst)))
              (case (force-implementation)
                ((sendfile)   (impl:sendfile src dst size))
                ((mmapped)    (impl:mmapped src dst size))
                ((read-write) (impl:read-write-loop/fd src dst size))
                ((nothing)
                 (let ((impl ((implementation-selector) size)))
                   (impl src dst size)))
                (else
                 (%error "invalid implementation forced. Allowed values are (sendfile mmapped read-write nothing)"))))))))
;;module
  
)
