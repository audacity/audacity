#include <stdlib.h>
#include <string.h>
#include "audio.h"

namespace _sbsms_ {

audio *make_audio_buf(long n) {
  return (audio*)calloc(n,sizeof(audio));
}

void free_audio_buf(audio *buf) {
  free(buf);
}

long copy_audio_buf(audio *to, long off1, audio *from, long off2, long n)
{
  memcpy(to+off1,from+off2,n*sizeof(audio));
  return n;
}

long audio_convert_from(float *to, long off1, audio *from, long off2, long n)
{
  for(int k=0;k<n;k++) {
    int k2 = (k+off1)<<1;
    to[k2] = (float)from[k+off2][0];
    to[k2+1] = (float)from[k+off2][1];
  }
  return n;
}

long audio_convert_to(audio *to, long off1, float *from, long off2, long n)
{
  for(int k=0;k<n;k++) {
    int k2 = (k+off2)<<1;
    to[k+off1][0] = (real)from[k2];
    to[k+off1][1] = (real)from[k2+1];
  }
  return n;
}

}