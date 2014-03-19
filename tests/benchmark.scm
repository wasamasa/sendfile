(use test)

(load "test-helper")

;; dd if=/dev/zero of=/tmp/test-file.100m count=100 bs=1M
;; dd if=/dev/zero of=/tmp/test-file.500m count=500 bs=1M
;; dd if=/dev/zero of=/tmp/test-file.700m count=700 bs=1M
;; dd if=/dev/zero of=/tmp/test-file.1g count=1024 bs=1M
;; start server with  socat TCP-LISTEN:5555,fork file:/dev/null

(use micro-benchmark sendfile)

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
       (void)))))

(let ((runs 5)
      (files '("test-file.100m" "test-file.500m" "test-file.700m" "test-file.1g")))

  (define (stream-file* file method)
    (benchmark-run runs (stream-file file method)))

  (define (run-bench/method title file)
    (let ((result (stream-file* file sendfile)))
      (print "Runtimes(" title "): \t" result)))

  (define (run-bench file)
    (run-bench/method "sendfile" file)

    (parameterize ((force-implementation 'read-write))
      (run-bench/method "  rw" file))

    (parameterize ((force-implementation 'read-write-port))
      (run-bench/method " rwp" file))

    (parameterize ((force-implementation 'mmapped))
      (run-bench/method "mmap" file)))

  (for-each (lambda (file)
              (print "\nFile: " file)
              (print "======================================")
              (run-bench (conc "/tmp/" file)))
            files))
