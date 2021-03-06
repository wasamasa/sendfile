;;
;; %%HEADER%%
;;
(use simple-sha1 tcp-server srfi-69 posix srfi-4)

(define (notify fmt . args)
  (apply printf fmt args)
  (flush-output))

(define (file-checksum path)
  (sha1sum path))

(define (buffer-checksum buffer)
  (string->sha1sum buffer))

(define (server-port)
  (let ((p (get-environment-variable "SENDFILE_TEST_PORT")))
    (or (and p (string->number p)) 5555)))

(define (server)
  (let ((listener (tcp-listen (server-port))))
    (let loop ()
      (receive (input output) (tcp-accept listener)
          (let ((finish-request (lambda (_)
                                  (close-input-port input)
                                  (close-output-port output)
                                  (exit 0))))
            (set-signal-handler! signal/term finish-request)
            (handle-request input output)
            (close-input-port input)
            (close-output-port output))
          (loop)))))

(define (extract-error-message exn)
  (string-trim-both (with-output-to-string (lambda () (print-error-message exn))) #\newline))

;; the handler reads the input and writes back the checksum of
;; the data received.
;; Important: the implementation expected the bytes to receive to be
;; the very first line of input. If this size is -1 then we just read
;; until we reach eof-object

;; Use it like so:
;; (call-with-connection-to-server (lambda (i o) (display "4" o) (newline
;; o) (display "aaaa" o) (read-line i)))

(define (handle-request input output)
  (handle-exceptions exn
                     (let ((error-message (extract-error-message exn)))
                       (fprintf (current-error-port) "~a~%" error-message)
                       (fprintf output "~a~%" error-message)
                       (flush-output output))
     (let* ((header (read-line input)))
        ;(notify "SERVER: new connection~%")
        (unless (eof-object? header)
          ;(notify "SERVER: received size-header: ~A bytes ~%" header)
          (let* ((bytes-following (string->number header))
                 (content (read-string (and (positive? bytes-following) bytes-following) input))
                 (checksum (buffer-checksum content)))
      ;      (notify "SERVER: read content successfully~%")
            (fprintf output "~A~%" checksum)
      ;      (notify "SERVER: answered with checksum: ~A~%" checksum)
)))))



(define (start-server #!key (fork #t))
  (if fork (fork-server) (server)))

(define (fork-server)
  (let ((pid (process-fork server)))
    (unless (wait-for-server 3)
      (notify "could not start server!!!")
      (exit 0))
    (flush-output)
    (sleep 4)
    pid))

(define (wait-for-server times)
  (if (zero? times)
      #f
      (begin (sleep 1) (or (can-connect?) (wait-for-server (sub1 times))))))

(define (can-connect?)
  (handle-exceptions exn #f
    (receive (in out)
        (tcp-connect "localhost" (server-port))
      (close-input-port in)
      (close-output-port out)
      #t)))

(define (stop-server pid)
  (process-signal pid))

(define (call-with-running-server thunk)
  (let ((pid (start-server)))
    (thunk)
    (stop-server pid)))

(define-syntax with-running-server
  (syntax-rules ()
    ((_ code more-code ...)
     (call-with-running-server
      (lambda () code more-code ...)))))

;; access the running server
(define (call-with-connection-to-server proc)
  (parameterize ((tcp-read-timeout 30000))
    (receive (input output) (tcp-connect "localhost" (server-port))
      (let ((result (proc input output)))
        ;; if an error accured the server might have gone down
        (handle-exceptions exn
                           (fprintf (current-error-port "~a~%" (extract-error-message exn)))
         (close-input-port input)
         (close-output-port output))
        result))))


(define (stream-file path streamer)
  (let ((size (file-size path))
        (file-port (file-open path (bitwise-ior open/rdonly open/binary))))
    (call-with-connection-to-server
     (lambda (server-input server-output)
       (write-content-size server-output size)
       ;(notify "STREAM-FILE: Wrote size-header: ~A bytes ~%" size)
       (streamer file-port server-output)
       ;(notify "STREAM-FILE: Streamed data~%")
       (close-output-port server-output)
       ;(notify "STREAM-FILE: Reading checksum from server ~%")
       (let ((chksm (read-checksum server-input)))
       ;  (notify "STREAM-FILE: Received checksum ~A~%" chksm)
         chksm)))))

(define (write-content-size port size)
  (display size port)
  (newline port)
  (flush-output port))

(define (read-checksum port)
  (read-line port))

;generate a string of bytes bytes
(define (call-with-buffer bytes proc #!optional (char #\a))
  (proc (make-string bytes char)))

(define (call-with-buffer/checksum buffer-size proc)
  (call-with-buffer
   buffer-size
   (lambda (buffer)
     (proc buffer (buffer-checksum buffer)))))

;; generate files
(define (call-with-temporary-file content proc)
  (let ((path (create-temporary-file)))
    (with-output-to-file path (lambda () (display content)))
    (let ((result (proc path)))
      (delete-file path)
      result)))

(define (call-with-temporary-file/checksum content proc)
  (call-with-temporary-file content
    (lambda (tempfile-path)
      (proc tempfile-path (buffer-checksum content)))))

(define (mebibytes amount)
  (* amount (kibibytes 1024)))

(define (kibibytes amount)
  (* amount 1024))

(define (generate-buffer bytes #!optional (char #\a))
  (call-with-buffer bytes identity char))
