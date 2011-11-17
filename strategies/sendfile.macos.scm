(foreign-declare "
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <errno.h>")

;; EAGAIN may be signaled even when partial data is sent, but the caller expects EAGAIN
;; to mean zero bytes sent, so we return the number of bytes sent when non-zero.
(define %sendfile-implementation
  (foreign-lambda* double ((integer src) (integer dst) (double offset) (unsigned-integer to_send))
    "off_t res = (off_t)to_send;"
    "if(sendfile(src,dst,(off_t)offset,&res,NULL/*&hdtr*/,0) < 0){"
    "  if(errno == EAGAIN)"
    "    C_return(res == 0 ? -2 : (double)(offset + res));"
    "  else C_return(-1);"
    "}"
    "C_return((double)(offset + res));"))
