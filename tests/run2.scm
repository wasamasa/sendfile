;; 
;; %%HEADER%%
;; 

(use test)
(load "spec-helper")
(use sendfile)

(with-running-server
 
 (let ((kb-buffer (generate-buffer (kibibytes 1)))
       (mb-buffer (generate-buffer (mibibytes 1))))
 
   (test-group "sendfile main interface"
               (call-with-temporary-file/checksum
                mb-buffer
                (lambda (temp-file-path content-checksum)
                  (let ((file-input (file-open temp-file-path (bitwise-ior open/rdonly open/binary))))
                    (test content-checksum
                          (call-with-connection-to-server
                           (lambda (server-in server-out)
                             (display (mibibytes 1) server-out)
                             (newline server-out)
                             (sendfile file-input server-out)
                             (flush-output server-out)
                             (read-line server-in)))))))))

  (test-group "regression")

 )

