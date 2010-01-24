/* from public domain code in libiberty (GNU binutils) - ANSIfied */

#include <stddef.h>

int
memcmp(const void* str1, const void* str2, size_t count) 
{
  register unsigned char *s1 = (unsigned char*)str1;
  register unsigned char *s2 = (unsigned char*)str2;

  while (count-- > 0) {
    if (*s1++ != *s2++)
      return s1[-1] < s2[-1] ? -1 : 1;
  }
  return 0;
}
