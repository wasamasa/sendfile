#include <stdlib.h>
#include <sys/sendfile.h>
int main(){
  int in,out;
  off_t offset;
  return sendfile(in,out,&offset,0);
}
