(use srfi-69)

(define (compute-file-checksum file)
  (let* ((inp (open-input-file file #:binary))
         (vec (read-u8vector #f inp)))
    (close-input-port inp)
    (hash vec)))


(define test-file-out "outfile.data")
(define big-test-file "sicp.pdf")
(define test-file "testfile.txt")
(define test-file-size (file-size test-file))
(define big-test-file-size (file-size big-test-file))
(define test-file-checksum (compute-file-checksum test-file))
(define big-test-file-checksum (compute-file-checksum big-test-file))

(define (with-prepared-environment file proc #!optional (ports? #f))
  (let ((in (file-open file (bitwise-ior open/rdonly open/binary)))
        (size (file-size file)))
    (receive (i o) (tcp-connect "localhost" 5555)
      (if (file-exists? test-file-out) (delete-file test-file-out))
      (let ((res (proc in (if ports? o (port->fileno o)))))
        (close-input-port i)
        (close-output-port o)
        (file-close in)
        res))))


(define (server)
  (let ((listener (tcp-listen 5555)))
    (let loop ()
      (receive (i o) (tcp-accept listener)
        (let ((vec (read-u8vector #f i))
              (file (open-output-file test-file-out #:binary)))
          (file-write (port->fileno file) (u8vector->blob vec))
          (close-output-port file)
          (close-input-port i)
          (close-output-port o)))
      (loop))))


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
      
