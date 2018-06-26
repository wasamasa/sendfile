#define _XOPEN_SOURCE 600
#include <stdlib.h>
#include <fcntl.h>
int main(){
  int fake_fd = 0;
  posix_fadvise(fake_fd,0,0,POSIX_FADV_SEQUENTIAL);
  return posix_fadvise(fake_fd,0,0,POSIX_FADV_NOREUSE);
}
