(cond-expand
  (chicken-4
   (define-constant +buf-size+ 1024)

   (define copy-port
     (let ((read-char read-char)
           (write-char write-char))
       (define (read-buf port writer)
         (let ((buf (make-string +buf-size+)))
           (let loop ()
             (let ((n (read-string! +buf-size+ buf port)))
               (unless (eq? n 0)
                 (writer buf n)
                 (loop))))))
       (define (write-buf buf n port writer)
         (do ((i 0 (fx+ i 1)))
             ((fx>= i n))
           (writer (integer->char (##sys#byte buf i)) port)))
       (define (read-and-write reader writer)
         (let loop ()
           (let ((x (reader)))
             (unless (eof-object? x)
               (writer x)
               (loop)))))
       (define (read-and-write-buf src dest reader)
         (let ((buf (make-string +buf-size+)))
           (let loop ((n 0))
             (when (fx>= n +buf-size+)
               (write-string buf +buf-size+ dest)
               (set! n 0))
             (let ((c (reader src)))
               (cond ((eof-object? c)
                      (when (fx>= n 0)
                        (write-string buf n dest)))
                     (else
                      (##sys#setbyte buf n (char->integer c))
                      (loop (fx+ n 1))))))))
       (lambda (src dest #!optional (read read-char) (write write-char))
         ;; does not check port args intentionally
         (cond ((eq? read read-char)
                (read-buf
                 src
                 (if (eq? write write-char)
                     (lambda (buf n) (write-string buf n dest))
                     (lambda (buf n) (write-buf buf n dest write)))))
               ((eq? write write-char)
                (read-and-write-buf src dest read))
               (else
                (read-and-write
                 (lambda () (read src))
                 (lambda (x) (write x dest)))))))))
  ;; CHICKEN 5 has copy-port, for sure
  (else))
