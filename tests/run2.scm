;; 
;; %%HEADER%%
;; 

(use test)
(load "test-helper")
(use sendfile)


(with-running-server
 
 (test-group "sendfile main interface"

             (call-with-temporary-file/checksum
              (generate-buffer (mibibytes 1))
              (lambda (temp-file expected-checksum)

                (test "sendfile"
                      expected-checksum
                      (stream-file temp-file sendfile)))))

 (test-group "regression")

 )

