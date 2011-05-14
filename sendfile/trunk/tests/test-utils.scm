(use srfi-69)

(define (notify fmt . args)
  (apply printf fmt args)
  (flush-output))

;; the file that is send over the wire
(define test-file "/tmp/sendfile-test.data")

;; the server will write all data it receives to this file
(define test-file-out "/tmp/sendfile-test.data.out")

;; the size of the file to transfer in bytes
(define wanted-test-file-size (* 1024 1024))

;; this may differ slightly from wanted-test-file-size
;; it is computed using (file-size)
;; The reason for the difference is that we use a fixed-size
;; fill-vector and this can lead to bytes being cut of as the
;; remainder of the division if the wanted size is not a multiple of
;; fill-chunk-size or less than fill-chunk-size
(define test-file-size 0)

(define fill-chunk-size 1024)

;; the checksum of the file will be set during generation
(define test-file-checksum #f)

;; the server is started before the test-suite is run
;; it listens on 5555 and writes anything it recieves to a fixed location
(define (server)
  (let ((listener (tcp-listen 5555)))
    (let loop ()
      (receive (i o) (tcp-accept listener)
        (let* ((file (open-output-file test-file-out #:binary))
               (lock (file-lock/blocking file))
               (vec (read-u8vector #f i)))
          (file-write (port->fileno file) (u8vector->blob vec))
          (file-unlock lock)
          (close-output-port file)
          (close-input-port i)
          (close-output-port o)))
      (loop))))

;; in order to see if the files have changed during transfer we
;; compute a checksum of the file
(define (compute-file-checksum file)
  (let* ((inp (open-input-file file #:binary))
         (vec (read-u8vector #f inp)))
    (close-input-port inp)
    (hash vec)))


;; this file is needed for a special test for a bug
(define offset-test-file "offset-test.txt")


(define (fill-test-file port)
  (let ((fill (make-u8vector fill-chunk-size))
        (rounds (if (> wanted-test-file-size fill-chunk-size) (inexact->exact (round (/ wanted-test-file-size fill-chunk-size))) 1)))
    (do ((i 1 (+ i 1)))
        ((= rounds i) i) (write-u8vector fill port))))

(define (generate-test-files)
  (newline)
  (notify "Generating files")
  (flush-output)
  (call-with-output-file test-file fill-test-file #:binary)
  (set! test-file-size (file-size test-file))
  (set! test-file-checksum (compute-file-checksum test-file)))

(define (destroy-test-files)
  (if (file-exists? test-file) (delete-file test-file)))


(define (with-prepared-environment file proc #!optional (ports? #f) (return-output #f))
  (parameterize ((tcp-read-timeout 3))
    (let ((in (file-open file (bitwise-ior open/rdonly open/binary)))
          (size (file-size file)))
      ;; Touch or truncate file
      (unless (file-exists? file) (with-output-to-file file void))
      (receive (i o) (tcp-connect "localhost" 5555)
        (dynamic-wind
            void
            (lambda () (proc in (if ports? o (port->fileno o))))
            (lambda ()
              (close-input-port i)
              (close-output-port o)
              (file-close in)))))))


;; tests if server is allready up
;; thanks to Peter Bex
(define (can-connect?)
  (handle-exceptions exn #f
    (receive (in out)
        (tcp-connect "localhost" 5555)
      (close-input-port in)
      (close-output-port out)
      #t)))

(define (wait-for-server times)
  (if (zero? times)
      #f
      (begin (sleep 1) (or (can-connect?) (wait-for-server (sub1 times))))))

(define (file-contents file)
  (let* ((in (open-input-file file))
         (lock (file-lock/blocking in))
         (contents (read-string #f in)))
    (file-unlock lock)
    (close-input-port in)
    contents))

(define (file-contents-chunk file offset length)
  (let* ((content (file-contents file))
         (offset-cnt (string-drop content offset)))
    (string-take offset-cnt length)))


(define (start-server)
  (newline)
  (notify "starting server on port 5555")
  (let ((pid (process-fork server)))
    (unless (wait-for-server 3)
      (notify "could not start server!!!")
      (destroy-test-files)
      (exit 0))
    (newline)
    (notify "standby")
    (flush-output)
    (sleep 4)
    pid))

(define (stop-server pid)
  (notify "shutting down")
  (process-signal pid)
  (newline)
  (notify "sent SIGTERM to server. Please make sure the server isn't running anymore!"))

(define (setup)
  (generate-test-files)
  (start-server))

(define (tear-down pid)
  (destroy-test-files)
  (stop-server pid))        
      
