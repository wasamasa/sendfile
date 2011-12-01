(foreign-declare "
#include<sys/socket.h>
#include<sys/types.h>
#include <sys/uio.h>")

(cond-expand
  (posix-fadvise
   (foreign-declare "#define READ_AHEAD (4 * 1024 * 1024)")
   (define %sendfile-implementation
     (foreign-lambda* double ((integer src) (integer dst) (double offset) (unsigned-integer to_send))
       "size_t res = 0;"
       "off_t tmpoffset = (off_t)offset;"
       "if((posix_fadvise(src,0,0,POSIX_FADV_SEQUENTIAL) < 0) && errno != ENOSYS){"
       "  C_return(-1);"
       "}"
       "if(sendfile(src,dst,tmpoffset,to_send,NULL,&res,0) < 0){"
       "  if(errno == EAGAIN){"
       "    C_return(res == 0 ? -2 : (double)(tmpoffset + res));"
       "  }else{"
       "    C_return(-1);"
       "  }"
       "}"
       " if(((off_t)offset & ~(READ_AHEAD - 1)) != (((off_t)offset + res) & ~(READ_AHEAD - 1))){"
       "  if(posix_fadvise(src,((off_t)offset + res) & ~(READ_AHEAD - 1),READ_AHEAD,POSIX_FADV_NOREUSE) < 0){"
       "    C_return(-1)"
       "  }"
       "}"
       "C_return((double)(tmpoffset + res));")))
  (else
   (define %sendfile-implementation
     (foreign-lambda*  double ((integer src) (integer dst) (double offset) (unsigned-integer to_send))
       "size_t res = 0;"
       "off_t tmpoffset = (off_t)offset;"
       "if(sendfile(src,dst,tmpoffset,to_send,NULL,&res,0) < 0){"
       "  if(errno == EAGAIN){"
       "    C_return(res == 0 ? -2 : (double)(tmpoffset + res));"
       "  }else{"
       "    C_return(-1);"
       "  }"
       "}"
       "C_return((double)(tmpoffset + res));"))))
