(foreign-declare "#include<sys/sendfile.h>")
(define %sendfile-implementation
  (foreign-lambda* double ((integer src) (integer dst) (double offset) (unsigned-integer to_send))
    "sendfilevec_t fvec;"
    "size_t written;"
    "fvec.sfv_fd = src;"
    "fvec.sfv_flag = 0;"
    "fvec.sfv_len = to_send;"
    "fvec.sfv_off = (off_t)offset;"
    "if(sendfilev(dst,&fvec,1,&written) == -1){"
    "  C_return(errno == EAGAIN ? -2 : -1);"
    "}"
    "C_return((double)(offset + written));"

    ))
