#include <stdlib.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/uio.h>
int main(){
  int src,dst;
  off_t offset,res;
  return sendfile(src,dst,offset,&res,0,0);
}
