(foreign-declare "
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <errno.h>")
(define %sendfile-implementation
  (foreign-lambda* double ((integer src) (integer dst) (double offset) (unsigned-integer to_send))
    "off_t res = (off_t)to_send;"
    "if(sendfile(src,dst,(off_t)offset,&res,NULL/*&hdtr*/,0) < 0){"
    "  C_return(errno == EAGAIN ? -2 : -1);"
    "}"
    "C_return((double)(offset + res));"))
