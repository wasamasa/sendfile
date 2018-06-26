#include <stdlib.h>
#include "sys/mman.h"
int main (){
  char *buff;
  madvise(buff,0,MADV_WILLNEED);
  return 0;
}
