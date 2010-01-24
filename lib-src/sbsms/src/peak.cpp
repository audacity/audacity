#include "peak.h"
#include <stdlib.h>
#include <string.h>

namespace _sbsms_ {

PeakAllocator :: PeakAllocator()
{
  size = 0;
  count = 0;
  peaks = NULL;
}

PeakAllocator :: ~PeakAllocator()
{
  if(peaks) free(peaks);
}

peak *PeakAllocator :: create() 
{
  if(count >= size) {
    if(size == 0)
      size = 1024;
    else
      size *= 2;
    
    peak *newpeaks = (peak*)malloc(size*sizeof(peak));
    if(peaks) {
      memcpy(newpeaks,peaks,count*sizeof(peak));
      free(peaks);
    }
    peaks = newpeaks;
  }
  return peaks+(count++);
}

void PeakAllocator :: destroy(peak *p)
{
  count--;
}

void PeakAllocator :: destroyAll()
{
  count = 0;
}

}
