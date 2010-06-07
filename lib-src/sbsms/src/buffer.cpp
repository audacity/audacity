#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "buffer.h"
#include "sbsms.h"
#include "utils.h"
#include <algorithm>
using namespace std;

namespace _sbsms_ {

#define INIT_SAMPLEBUF_LENGTH 8192
#define INIT_GRAINBUF_LENGTH 128
#define INIT_TPLBUF_LENGTH 128

/****************

 SampleBuf

****************/

SampleBuf :: SampleBuf(int N) 
{
  init(N,0);
}

SampleBuf :: SampleBuf(int N, long delay) 
{
  init(N,delay);
}

void SampleBuf :: init(int N, long delay)
{
  this->delay = delay;
  this->N = N;
  this->length = INIT_SAMPLEBUF_LENGTH;
  this->buf = make_audio_buf(2*length);
  this->readPos = 0;
  this->writePos = 0;
}

SampleBuf :: ~SampleBuf() 
{
  free_audio_buf(buf);
}

long SampleBuf :: write(audio *in, long n)
{
  if(n==0) {
    return 0;
  }
  grow(n);
  memcpy(buf+writePos,in,n*sizeof(audio));
  writePos += n;
  return n;
}

void SampleBuf :: grow(long n)
{
  long pos = writePos+n;
  while(pos >= 2*length) {
    length *= 2;
    audio *newBuf = make_audio_buf(2*length);
    memcpy(newBuf,buf+readPos,(length-readPos)*sizeof(audio));
    free_audio_buf(buf);
    buf = newBuf;
    writePos -= readPos;
    pos -= readPos;
    readPos = 0;
  }
}

long SampleBuf :: write(grain *g, int h)
{
  grow(g->N);
  g->synthesize();
  real f = 2.0/(real)(N/h) * 4.0/3.0;
  
  for(int c=0;c<2;c++) {
    int j = 0;
    for(int k=writePos; k<writePos+g->N; k++) {
      buf[k][c] += g->x[j++][c] * f;
    }
  }
  writePos += h;
  return h;
}

long SampleBuf :: read(audio *outBuf, long n)
{
  if(n==0)
    return 0;
  assert(writePos < 2*length);
  n = min(n,n_readable());
  n = max(n,(long)0);
  memcpy(outBuf,buf+readPos,n*sizeof(audio));
  return n;
}

long SampleBuf :: n_readable()
{
  return max(writePos-delay - readPos,(long)0);
}

void SampleBuf :: advance(long n) {
  assert(readPos+n <= writePos);
  memset(buf+readPos,0,n*sizeof(audio));
  readPos += n;
  if(readPos >= length) {
    long endPos;
    if(N==0)
      endPos = 2*length;
    else 
      endPos = min(2*length,writePos+N);
    memcpy(buf,buf+readPos,(endPos-readPos)*sizeof(audio));
    memset(buf+readPos,0,(endPos-readPos)*sizeof(audio));
    writePos -= readPos;
    readPos = 0;
  }
}

audio *SampleBuf :: getReadBuf() 
{
  return (buf+readPos);
}

void SampleBuf :: clear()
{
  advance(writePos-readPos);
}

/****************

 GrainBuf

****************/

GrainBuf :: GrainBuf(int N, int h) 
{
  init(N,h,1);
}

GrainBuf :: GrainBuf(int N, int h, real pad) 
{
  init(N,h,pad);
}

void GrainBuf :: init(int N, int h, real pad)
{
  this->length = INIT_GRAINBUF_LENGTH;
  this->buf = (grain**) calloc(2*length,sizeof(grain*));
  this->iBuf = (audio*) calloc(N,sizeof(audio));
  
  this->pad = pad;
  this->N = N;
  this->h = h;
  this->iBufWritePos = 0;
  this->readPos = 0;
  this->writePos = 0;
  
}

GrainBuf :: ~GrainBuf() 
{
  clear();
  free(buf);
  free_audio_buf(iBuf);
}

long GrainBuf :: write(audio *buf2, long n)
{
  if(n==0) {
    return 0;
  }
  long ng = 0;
  int overlap = N - h;
  long bufReadPos = 0;

  while(bufReadPos<n) {
    long nToCopy = min((n-bufReadPos),N-iBufWritePos);
    memcpy(iBuf+iBufWritePos, buf2+bufReadPos, nToCopy*sizeof(audio));

    if(nToCopy+iBufWritePos == N) {
      convert(iBuf);
      ng++;
      memcpy(iBuf,iBuf+h,overlap*sizeof(audio));
      iBufWritePos = overlap;
      bufReadPos += nToCopy;
    } else break;
  }

  // copy the remainder to the iBuf
  long nToCopy = min((n-bufReadPos),N-iBufWritePos);
  memcpy(iBuf+iBufWritePos, buf2+bufReadPos, nToCopy*sizeof(audio));
  iBufWritePos += nToCopy;
    
  return ng;
}

void GrainBuf :: convert(audio *timebuf) 
{
  grain *g = grain::create(N,pad);
  memcpy(g->x,timebuf,N*sizeof(audio));
  g->analyze();
  g->h = h;
  write(g);
}

void GrainBuf :: advance(long n)
{
  assert(readPos+n <= writePos);
  for(int k=readPos;k<readPos+n;k++) {
    grain :: forget(buf[k]);
  }
  readPos += n;

  //grain::count-=n;
  if(readPos >= length) {
    memcpy(buf,buf+readPos,(writePos-readPos)*sizeof(grain*));
    writePos = writePos - readPos;
    readPos = 0;
  }
}

grain* GrainBuf :: read(long k) 
{
  return buf[k];
}

void GrainBuf :: write(grain *g) 
{
  if(writePos >= 2*length) {
    length *= 2;
    grain **newBuf = (grain**)calloc(2*length,sizeof(grain*));
    memcpy(newBuf,buf+readPos,(writePos-readPos)*sizeof(grain*));
    free(buf);
    buf = newBuf;
    writePos -= readPos;
    readPos = 0;
  }

  grain :: referenced(g);
  buf[writePos++] = g;
}

void GrainBuf :: clear()
{
  memset(iBuf,0,N*sizeof(audio));
  iBufWritePos = 0;
  advance(n_readable());
}

/****************

 Mixer

****************/

Mixer :: Mixer(SampleBufBase *b1, SampleBuf *b2)
{
  this->b1 = b1;
  this->b2 = b2;
}

long Mixer :: read(audio *outBuf, long n)
{
  if(n==0)
    return 0;
  n = min(n,n_readable());

  b1->read(outBuf,n);
  audio *buf2 = b2->getReadBuf();

  for(int k=0;k<n;k++) {
    for(int c=0;c<2;c++) 
      outBuf[k][c] += buf2[k][c];
  }
  return n;
}

void Mixer::advance(long n)
{
  b1->advance(n);
  b2->advance(n);
}

long Mixer::n_readable()
{
  return min(b1->n_readable(), b2->n_readable());
}

/********************
 TrackPointListBuffer
********************/

TrackPointListBuffer :: TrackPointListBuffer() {
  length = INIT_TPLBUF_LENGTH;
  buf = (tplist**) calloc(2*length,sizeof(tplist*));
  readPos = 0;
  writePos = 0;
}

TrackPointListBuffer :: ~TrackPointListBuffer() {
  for(int k=readPos;k<writePos;k++)
    delete buf[k];
  free(buf);
}

long TrackPointListBuffer :: write(tplist *tpl) 
{
  if(writePos >= 2*length) {
    length *= 2;
    tplist **newBuf = (tplist**) calloc(2*length,sizeof(tplist*));
    memcpy(newBuf,buf+readPos,(writePos-readPos)*sizeof(tplist*));
    free(buf);
    buf = newBuf;
    writePos -= readPos;
    readPos = 0;
  }

  buf[writePos++] = tpl;
  return 1;
}

long TrackPointListBuffer :: n_readable()
{
  return writePos-readPos;
}

tplist *TrackPointListBuffer :: read(long k) 
{
  return buf[k];
}

void TrackPointListBuffer :: advance(long n)
{
  assert(readPos+n <= writePos);
  for(int k=readPos;k<readPos+n;k++) {
    delete buf[k];
  }
  readPos += n;
  if(readPos >= length) {
    memcpy(buf,buf+readPos,(writePos-readPos)*sizeof(tplist*));
    writePos = writePos - readPos;
    readPos = 0;
  }
}

}
