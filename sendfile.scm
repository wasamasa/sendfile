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

(module sendfile
(force-implementation *last-selected-implementation* read-write-buffer-size
 implementation-selector impl:mmapped impl:sendfile impl:read-write-loop/fd
 impl:read-write-loop/port mmap-available sendfile-available sendfile)
(import chicken scheme)
(require-library posix lolevel srfi-4)
(import extras posix srfi-4 foreign lolevel ports)

(include "backward-compatibility.scm")

(define (kilobytes num)  (* num 1024))
(define (megabytes num)  (* (kilobytes num) 1024))

(define-foreign-variable %bufsize int "BUFSIZ")

;;set to either 'sendfile 'mmapped 'read-write or 'nothing
(define force-implementation (make-parameter 'nothing))

(define write-timeout (make-parameter #f))


;;the buffer used in read write loops
;;the client may adjust this to meet its need
(define read-write-buffer-size (make-parameter %bufsize))

;;the current chunk-size specifies how big the slices are that
;;we read/write in the three scenarios. This is parameterized
;;because different methods to send the file may work better with
;;differently sized chunks.
;; We've chosen 64k for two reasons:
;; 1) as chicken does not have native threads, a smaller chunksize
;;    means a shorter period of time that the thread blocks. 
;; 2) it is half the typical max readahead size in Linux 2.6, giving the
;;    kernel some time to populate the page cache in between
;;    subsequent sendfile() calls.
(define %current-chunk-size (make-parameter (kilobytes 64)))

;;compute the next chunk to send out of offset and the length
;;of the remaining buffer. This is really just a convenience-procedure
;;that uses (the possibly parameterized) (chunk-size)

(define (next-chunk-size current-offset target-offset)
  (let ((distance (- target-offset current-offset)))
    (if (> distance (%current-chunk-size)) (%current-chunk-size) distance)))


;; yield control to other threads so that
;; we don't block them
(define (%yield)
    (##sys#call-with-current-continuation
     (lambda (return)
       (let ((ct ##sys#current-thread))
         (##sys#setslot ct 1 (lambda () (return (##core#undefined))))
         (##sys#schedule) ) ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions
;;
;; Todo:
;; Add conditions for
;; * invalid implementation forced
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(define (complain with-errno? msg . args)
  (abort (make-sendfile-condition #f msg (if with-errno?
                                             (begin
                                               (##sys#update-errno)
                                               (append (errno-argument) args))
                                             args))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strategies / Implementations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this global is set by the implementations used
;; so that the client programmer knows what was going on
;; under the hood
(define *last-selected-implementation* #f)

(define mmap-available (cond-expand
                         (mmap #t)
                         (else #f)))

(define sendfile-available (cond-expand
                             (sendfile #t)
                             (else #f)))

;;mmap
(cond-expand
  (mmap (include "strategies/mmap.scm"))
  (else
   (define (impl:mmapped . args)
     (complain #f "Mmap is not available on your system"))))

;;sendfile
(cond-expand
  (sendfile
   (cond-expand
     (linux
      (include "strategies/sendfile.linux.scm"))
     ((or netbsd openbsd freebsd)
      (include "strategies/sendfile.bsd.scm"))
     (macosx
      (include "strategies/sendfile.macos.scm"))
     (sun
      (include "strategies/sendfile.solaris.scm"))
     (else)))
  (else))

(cond-expand
  (sendfile
   (define (impl:sendfile src dst offset bytes)
     (set!  *last-selected-implementation* 'sendfile)
     
     (let loop ((offset offset) (target-offset (+ offset bytes)))
       (if (= offset  target-offset)
           bytes
           (let* ((next-chunk (next-chunk-size offset (+ offset bytes)))
                  (new-offset (%sendfile-implementation src dst offset next-chunk)))
             (cond
              ((eqv? -2.0 new-offset)   ; EAGAIN/EINTR
               (when (write-timeout)
                 (##sys#thread-block-for-timeout!
                  ##sys#current-thread
                  (+ (current-milliseconds) (write-timeout))))
               (##sys#thread-block-for-i/o! ##sys#current-thread dst #:output)
               (%yield)
               (when (##sys#slot ##sys#current-thread 13)
                 (complain #f "write operation timed out"))
               (loop offset target-offset))
              ((negative? new-offset)
               (complain #t "sendfile failed"))
              (else
               (loop new-offset target-offset))))))))
  (else
   (define (impl:sendfile . args)
     (complain #f "Sendfile is not available on your system"))))

;; read-write-loop
(include "strategies/rw.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlevel Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Select the implementation based on the amount of bytes to send
;; It tries to select the apropriate implementation but is not fool-proof
(define (default-selector len)
  (cond
   ((> len (megabytes 1) impl:read-write-loop/fd ))
   (else
    (cond-expand
      (sendfile impl:sendfile)
      (mmap     impl:mmapped)
      (else     impl:read-write-loop/fd)))))

(define implementation-selector (make-parameter default-selector))

(define (port-has-fd? obj)
  (unless (port? obj)
    (complain #f "supplied argument is not a port"))
  (handle-exceptions exn #f (port->fileno obj) #t))

(define (->fileno obj)
  (cond
   ((fixnum? obj) obj)
   ((port? obj) (port->fileno obj))
   (else (complain #f "supplied argument is neither port nor descriptor"))))


(define (ensure-sane-offset/bytes size offset bytes)
  (cond
   ((negative? offset)                    (complain #f "Offset must be >= 0 but was" offset))
   ((and bytes (negative? bytes))         (complain #f "Bytes must be >= 0 but was " bytes))
   ((and bytes (> (+ offset bytes) size)) (complain #f "Bytes + offset exceeds filesize" ))
   (else #t)))

(define (sendfile source target #!key (offset 0) (bytes #f))
  (cond
   ((ports? source target)
    (sendfile/ports source target offset bytes))
   (else (sendfile/best-strategy source target offset  bytes))))

(define (port-without-fd? port)
  (and (port? port) (not (port-has-fd? port))))

(define (ports? source target)
  (or
   (and (eq? (force-implementation) 'read-write-port) (port? target))
   (port-without-fd? source)
   (port-without-fd? target)))

(define (sendfile/ports source target offset bytes-to-send)
  (if (port-without-fd? source)
      (impl:read-write-loop/port-both source target offset  bytes-to-send)
      (let* ((source  (->fileno source))
             (size (file-size source))
             (len (or bytes-to-send (- size offset))))
        
        (ensure-sane-offset/bytes size offset bytes-to-send)
        (impl:read-write-loop/port source target offset len))))

(define (sendfile/best-strategy source target offset bytes-to-send)
    (let* ((source (->fileno source))
           (size (file-size source))
           (len  (or bytes-to-send (- size offset))))

      (ensure-sane-offset/bytes size offset bytes-to-send)
      (flush-output target)
      (let ((target (->fileno target)))
        (case (force-implementation)
          ((sendfile)
           (if sendfile-available
               (impl:sendfile source target offset len)
               (complain #f "implementation sendfile was forced but is not available")))
          ((mmapped)
           (if mmap-available
               (impl:mmapped source target offset len)
               (complain #f "implementation mmap was forced but is not available")))
          ((read-write)      (impl:read-write-loop/fd source target offset len))
          ((nothing)
           (let ((impl ((implementation-selector) size)))
             (impl source target offset len)))
          (else
           (complain #f "invalid implementation forced. Allowed values are (sendfile mmapped read-write read-write-port nothing)"))))))
)


