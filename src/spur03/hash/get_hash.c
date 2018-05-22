#include <stdio.h>
#include "fnv.h"

unsigned int get_hash_fnv_32(char* str,int bound)
{
  Fnv32_t hval;

printf("%s %d\n",str,bound) ;
  hval = fnv_32_str(str, hval);
  int ret = hval%(unsigned int)bound+1;
  return ret ;
}
