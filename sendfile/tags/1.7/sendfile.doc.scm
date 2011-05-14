(use eggdoc)
;(use eggdoc-zb)

(define examples `( (pre #<<EOF
(use sendfile)

;;in all the examples
;;we use a generic procedure with-prepared-environment
;;which we assume provides us with the input and outputports
;;needed. Most of the time the output-port will be a socket
;;and the input-port may be connected to a file
;;the size of the input-file was gathered as well


;; use the standard interface and let the system decide what to do
(with-prepared-environment
 (lambda (in out len)
   (sendfile in out)))

;; force a specific method to use: Part I

;;read-write
;;notice that you can force a specific transmission method
;;via the srfi parameter sendfile:force-implementation
;;there are four possible values: 'sendfile,'read-write-loop,'mmapped,'nothing
;;'nothing is the default, if this is set, sendfile will decide which implementation to use
;;based on the systems capabilities and the filesize
(with-prepared-environment
 (lambda (in out len)
   (parameterize ((sendfile:force-implementation 'read-write))
     (sendfile in out))))

;;force a specific method to use: Part II

;;sometimes you may want to decide which method to
;;use based on the size of the file.
;;there is an srfi-parameter called sendfile:implementation-selector
;;which does just that. See documentation for details
(with-prepared-environment
 (lambda (in out)
   (paramaterize ((sendfile:implementation-selector) (lambda (len)
                                                       (if (> len 1024)
                                                           sendfile:sendfile
                                                           sendfile:read-write-loop)))
                 (sendfile in out))))

EOF
)))

(define doc
  `((eggdoc:begin
    (name "sendfile")
    (description "faster filetransfer over the network")
    (author "David Krentzlin")
    (history
     (version "1.7" "Actually allow input ports for source; they still have to have an underlying file descriptor currently")
     (version "1.6.3" "Use c-pointer instead of treating the buffer as a string. Thanks to Felix")
     (version "1.6.2" "Flush output port before sending to ensure output is sent in order.")
     (version "1.5.0" "The 'force parameter has been removed from (sendfile), its a parameter now instead. Bugfix for the read-write-loop, that causes corrupted files to be sent.")
     (version "1.4.3" "MacOS X 10.5 support, fix of BSD & default 'sendfile_implementation' argument order, better error information")
     (version "1.3.0" "make it compile on windows")
     (version "1.2.0" "allmost complete rewrite")
     (version "1.1.0" "Enhanced portability")
     (version "1.0.0" "Initial release"))
    (usage)
    (download "sendfile.egg")
    (documentation
     (p "This extension provides a way to do filetransfers with zero-copy-semantics if applicable. It provides an interface to the sendfile syscall on
 systems that support it. On other systems it emulates it with the help of memory-mapped IO. Sendfile is very configurable and tries to do the best based
 on the actual file. See below to learn what it provides.")
     
     (p "NOTE: theoretically read-write-loop and mmapped-io can be used for file-to-file-transfers, where sendfile can only be used for file-network-transfers.")
     
     (subsection "sending a file. the main interface"
       (group            
        (procedure "(sendfile source destination)"
          (p "Tries to send the file identified by `source` to `destination` as fast as possible. Unless a specific technique is forced it will decide what method to use from the systems capabilities and the filesize.")

          (p "source ... can be either a port to the inputfile or a filedescriptor of an already opened file."

          (p "destination ... can be either a port to the outputfile (socket) or a filedescriptor (socketdesciptor) of an already opened file (socket).  When it is a port, any buffered output is flushed via {{flush-output}} prior to sending the file.")))))
     
     (subsection "sending a file with the sendfile-syscall"
      (group            
        (procedure "(sendfile:sendfile source destination len)"
          (p "If it is available this is the interface to the sendfile-implementation of your operating system")

          (p "source ... is the filedescriptor of an already opened file")
          (p "destination ... is the filedescriptor of an already opened file (MUST be a socket)")
          (p "len ... is the size of the file in bytes as e.g. retrieved by (file-size)")
          (p "This procedure returns the amount of bytes successfully written."))))

     (subsection "sending a file with memory-mapped-io"
      (group            
        (procedure "(sendfile:mmapped source destination len)"
          (p "Sends a file by mapping it into the memory and do repeated writes.")
          (p "source ... is the filedescriptor of an already opened file")
          (p "destination ... is the filedescriptor of an already opened file (can be a socket)")
          (p "len ... is the size of the file in bytes as e.g. retrieved by (file-size)")
          (p "This procedure returns the amount of bytes successfully written."))))

     (subsection "sending a file with a read-write-loop"
      (group            
        (procedure "(sendfile:read-write-loop source destination len)"
          (p "Sends a file by performing repeated reads and writes.")

          (p "source ... is the filedescriptor of an already opened file")
          (p "destination ... is the filedescriptor of an already opened file (can be a socket)")
          (p "len ... is the size of the file in bytes as e.g. retrieved by (file-size)")
          (p "This procedure returns the amount of bytes successfully written."))))

     (subsection "test if sendfile is natively available"
      (group            
        (definition "sendfile:os-dep:sendfile-available?"
          (p "Is set to #t if the sendfile-syscall is available and #f if not"))))

     (subsection "test if mmap() is available"
      (group            
        (definition "sendfile:os-dep:mmap-available?"
          (p "Is set to #t if the mmap() is available and #f if not"))))

     (subsection "Parameters"
      (group            
        (parameter "sendfile:read-write-buffer-size"
          (p "The size of the buffer that is used in sendfile:read-write-loop"))
        (parameter "sendfile:force-implementation"
          (p "Causes sendfile to allways use the transmission-method specified by this parameter.
             Possible values are 'sendfile,'mmapped,'read-write and 'nothing.
            It defaults to 'nothing, where (sendfile) will decide which method to use based on the system's capabilities and sendfile:implementation-selector"))
        (parameter "sendfile:implementation-selector"
          (p "A one-argument procedure that gets the size of the file in question passed and is expected to return a procedure to use (either of sendfile:mmapped,sendfile:sendfile,sendfile:read-write-loop)"))))
          
                   

     (examples ,examples)))))

(eggdoc->html doc)
