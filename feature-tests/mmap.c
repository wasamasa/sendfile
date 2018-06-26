#include <stdlib.h>
#include <sys/mman.h>
int main(){
   int src;
   return mmap(0, 100, PROT_READ | PROT_WRITE, MAP_SHARED,src, 0);
}
