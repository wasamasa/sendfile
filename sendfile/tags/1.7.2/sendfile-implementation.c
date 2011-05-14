#include <unistd.h>
#include <errno.h>
#include "os-dep.h"


/* gnu linux sendfile syscall */
/* changes mostly inspired by lighttpd (a very nice http server) 
   keep up the good work folks. */

#ifdef HAVE_POSIX_FADVISE
#define READ_AHEAD (4 * 1024 * 1024)
#endif




#if defined(USE_LINUX_IMPL)

double sendfile_implementation(int src,int dst,double offset,size_t to_send){
  int res;
  off_t curoffset = (off_t)offset;


#ifdef HAVE_POSIX_FADVISE
  if((posix_fadvise(src,0,0,POSIX_FADV_SEQUENTIAL) < 0) && errno != ENOSYS)
    return -1;
#endif

  res = sendfile(dst,src,&curoffset,to_send);
  
	if(res < 0){
		if(errno == EAGAIN || errno == EINTR)
			 return -2;
		return -1;
	}  
  
#ifdef HAVE_POSIX_FADVISE
  if(((off_t)offset & ~(READ_AHEAD - 1)) != (((off_t)offset + res) & ~(READ_AHEAD - 1)))
    if(posix_fadvise(src,((off_t)offset + res) & ~(READ_AHEAD - 1),READ_AHEAD,POSIX_FADV_NOREUSE) < 0)
      return -1;
#endif
  return (double)curoffset;
}

#elif defined(USE_BSD_IMPL)

/* FreeBSD,DragonFly BSD */
double sendfile_implementation(int src,int dst,double offset,size_t to_send){
  size_t res = 0;
  off_t tmpoffset = (off_t)offset;

#ifdef HAVE_POSIX_FADVISE
  if((posix_fadvise(src,0,0,POSIX_FADV_SEQUENTIAL) < 0) && errno != ENOSYS)
    return -1;
#endif


  if(sendfile(src,dst,offset,to_send,NULL,&res,0) < 0){
    if(errno == EAGAIN)
			return -2;
		return -1;
  }

#ifdef HAVE_POSIX_FADVISE
  if(((off_t)offset & ~(READ_AHEAD - 1)) != (((off_t)offset + res) & ~(READ_AHEAD - 1)))
    if(posix_fadvise(src,((off_t)offset + res) & ~(READ_AHEAD - 1),READ_AHEAD,POSIX_FADV_NOREUSE) < 0)
      return -1;
#endif

  return (double)(offset + res);
}

#elif defined(USE_DARWIN_IMPL)

/* Darwin 9.0+ */
double sendfile_implementation(int src,int dst,double offset,size_t to_send){
  off_t res = to_send;
  //struct sf_hdtr hdtr;
  
  //memset(&hdtr,0,sizeof(hdtr));
  if(sendfile(src,dst,(off_t)offset,&res,NULL/*&hdtr*/,0) < 0){
    if(errno == EAGAIN)
			return -2;
		return -1;
  }

  return (double)(offset + res);
}

#elif defined(USE_SOLARIS_IMPL)

double sendfile_implementation(int src,int dst,double offset,size_t to_send){
  sendfilevec_t fvec;
  size_t written;
  
  fvec.sfv_fd = src;
  fvec.sfv_flag = 0;
  fvec.sfv_len = to_send;
  fvec.sfv_off = (off_t)offset;
    
  if(sendfilev(dst,&fvec,1,&written) == -1){
    if(errno == EAGAIN)
			return -2;
		return -1;
  }

  return (double)(offset + written);
}


#else

double sendfile_implementation(int src,int dst,double offset,size_t to_send){
  return 0;
}


#endif




