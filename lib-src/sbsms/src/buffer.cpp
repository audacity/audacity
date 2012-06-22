#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "buffer.h"
#include "sbsms.h"
#include "utils.h"
#include <algorithm>
using namespace std;

namespace _sbsms_ {

template<>
void SampleBuf :: write(grain *g, int h)
{
  grow(N);
  g->synthesize();
  float f = 2.6666666666666666666666666f/(float)(N/h);
  for(int c=0;c<2;c++) {
    int j = 0;
    int kend = writePos + N;
    for(int k=writePos; k<kend; k++) {
      buf[k][c] += g->x[j++][c] * f;
    }
  }
  writePos += h;
}

GrainBuf :: GrainBuf(int N, int h, int N2, int type) :
  grainAllocator(N,N2,type)
{
  this->length = initGrainBufLength;
  this->buf = (grain**) calloc(2*length,sizeof(grain*));
  this->iBuf = (audio*) calloc(N2,sizeof(audio));
  this->N2 = N2;
  this->h = h;
  this->overlap = N2 - h;
  this->xOffset = (N-N2)>>1;
  this->iBufWritePos = 0;
  this->readPos = 0;
  this->writePos = 0;
}

GrainBuf :: ~GrainBuf() 
{
  for(int k=readPos;k<writePos;k++) {
    grainAllocator.forget(buf[k]);
  }
  free(buf);
  free(iBuf);
}

audio *GrainBuf :: getWindowFFT()
{
  return grainAllocator.W;
}

long GrainBuf :: write(audio *buf2, long n)
{
  if(n==0) {
    return 0;
  }
  long ng = 0;
  long bufReadPos = 0;
  long nToCopy;
  while(bufReadPos<n) {
    nToCopy = min((n-bufReadPos),N2-iBufWritePos);
    if(nToCopy+iBufWritePos == N2) {
      if(buf2) {
        memmove(iBuf+iBufWritePos, buf2+bufReadPos, nToCopy*sizeof(audio));
      } else {
        memset(iBuf+iBufWritePos, 0, nToCopy*sizeof(audio));
      }
      grain *g = grainAllocator.create();
      memmove(g->x+xOffset,iBuf,N2*sizeof(audio));
      write(g);
      ng++;
      memmove(iBuf,iBuf+h,overlap*sizeof(audio));
      iBufWritePos = overlap;
      bufReadPos += nToCopy;
    } else break;
  }
  nToCopy = min((n-bufReadPos),N2-iBufWritePos);
  if(buf2) {
    memmove(iBuf+iBufWritePos, buf2+bufReadPos, nToCopy*sizeof(audio));
  } else {
    memset(iBuf+iBufWritePos, 0, nToCopy*sizeof(audio));
  }
  iBufWritePos += nToCopy;
    
  return ng;
}

void GrainBuf :: advance(long n)
{
  assert(readPos+n <= writePos);
  for(int k=readPos;k<readPos+n;k++) {
    grainAllocator.forget(buf[k]);
  }
  readPos += n;
  if(readPos >= length) {
    memmove(buf,buf+readPos,(writePos-readPos)*sizeof(grain*));
    writePos = writePos - readPos;
    readPos = 0;
  }
}

grain* GrainBuf :: read(long k) 
{
  return buf[k];
}

long GrainBuf :: nReadable()
{
  return writePos - readPos;
}

void GrainBuf :: write(grain *g) 
{
  if(writePos >= length<<1) {
    length <<= 1;
    grain **newBuf = (grain**)calloc((length<<1),sizeof(grain*));
    memmove(newBuf,buf+readPos,(writePos-readPos)*sizeof(grain*));
    free(buf);
    buf = newBuf;
    writePos -= readPos;
    readPos = 0;
  }
  grainAllocator.reference(g);
  buf[writePos++] = g;
}

void GrainBuf :: reference(grain *g)
{
  grainAllocator.reference(g);
}

void GrainBuf :: forget(grain *g)
{
  grainAllocator.forget(g);
}

void GrainBuf :: clear()
{
  memset(iBuf,0,N2*sizeof(audio));
  iBufWritePos = 0;
  advance(writePos-readPos);
}

Mixer :: Mixer(SampleBufBase *b1, SampleBuf *b2)
{
  this->b1 = b1;
  this->b2 = b2;
}

long Mixer :: read(audio *outBuf, long n)
{
  if(n==0) return 0;
  n = min(n,b2->nReadable());
  n = b1->read(outBuf,n);
  audio *buf2 = b2->getReadBuf();
  for(int k=0;k<n;k++) {
    for(int c=0;c<2;c++) 
      outBuf[k][c] += buf2[k][c];
  }
  b2->advance(n);
  return n;
}

}
