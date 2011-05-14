

#include <chicken.h>

#ifdef __linux

 #include <sys/types.h>
 #include <unistd.h>
 #include <sys/sendfile.h>
 #include <sys/mman.h>
 #include <linux/version.h>
 #include <fcntl.h>

 double sendfile_implementation(int src,int dst,double offset,size_t to_send);

 #define HAVE_SENDFILE 1
 #define HAVE_MMAP 1
 #define USE_LINUX_IMPL
 #define _XOPEN_SOURCE 600
 #if defined(POSIX_FADV_SEQUENTIAL) && defined(POSIX_FADV_NOREUSE)
  #define HAVE_POSIX_FADVISE 
 #endif

#elif defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__) || defined(__DragonFly__)

 #include <sys/socket.h>
 #include <sys/types.h>
 #include <sys/mman.h>

 double sendfile_implementation(int src,int dst,double offset,size_t to_send);
 #define USE_BSD_IMPL
 #define HAVE_MMAP 1
 #if (defined(__FreeBSD__) && (__FreeBSD__ >= 3)) || defined(__DragonFly__)
  #define HAVE_SENDFILE 1
  #include <sys/uio.h>
 #else
  #define HAVE_SENDFILE 0
 #endif

#elif (defined(__APPLE__) && defined(__MACH__))

 #include <sys/types.h>
 #include <sys/socket.h>
 #include <sys/uio.h>
 #include <sys/mman.h>

 double sendfile_implementation(int src,int dst,double offset,size_t to_send);
 #define HAVE_MMAP 1

 /* Not foolproof but does stop compilation when < MacOS 10.5
  * Should also state that the MIN_ALLOWED is 10.5 but that requires
  * compiler option setting.
  */
 #include <AvailabilityMacros.h>
 #if MAC_OS_X_VERSION_MAX_ALLOWED > MAC_OS_X_VERSION_10_4
   #define HAVE_SENDFILE 1
   #define USE_DARWIN_IMPL
 #else
   #define HAVE_SENDFILE 0
 #endif

#elif defined(sun) || defined(__sun)

 double sendfile_implementation(int src,int dst,double offset,size_t to_send);
 #ifdef __SunOS_5_8
   #define HAVE_SENDFILE 1
   #define HAVE_MMAP 1
   #define USE_SOLARIS_IMPL
   #include <sys/sendfile.h>
   #include <sys/mman.h>
 #else
   #define HAVE_SENDFILE 0
   #define HAVE_MMAP 0
 #endif

#elif defined(_WIN32) || defined(_MSC_VER) || defined(__MINGW32__)

 #define MADV_SEQUENTIAL 0
 #define HAVE_SENDFILE 0
 #define HAVE_MMAP 0
 double sendfile_implementation(int src,int dst,double offset,int to_send);

#else

 #define MADV_SEQUENTIAL 0
 #define HAVE_SENDFILE 0
 #define HAVE_MMAP 0
 double sendfile_implementation(int src,int dst,double offset,size_t to_send);

#endif


#if defined(__CYGWIN__) || !defined(MADV_SEQUENTIAL)
static int madvise(char* a, unsigned int b, int c){
  return 0;
}
#define MADV_SEQUENTIAL 0
#define MADV_WILLNEED 0
#define HAVE_MADVISE 0
#endif


