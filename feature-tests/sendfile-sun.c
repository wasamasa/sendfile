#include <stdlib.h>
#include <sys/sendfile.h>
int main(){
  int dst;
  sendfilevec_t fvec;
  size_t written;
  return sendfile(dst,&fvec,1,&written);
}
