// -*- mode: c++ -*-
#ifndef BUFFER_H
#define BUFFER_H

#include "sbsms.h"
#include "grain.h"
#include <stdlib.h>
#include <list>
using namespace std;

namespace _sbsms_ {

enum {
  initSampleBufLength = 8192,
  initGrainBufLength = 256,
  initRingBufferLength = 64
};

template <class T>
class RingBuffer {
 public:
  RingBuffer();
  ~RingBuffer();
  long write(T a);
  T read(long k);
  T read();
  long nReadable();
  void advance(long n);
  void clear();
  long readPos;
  long writePos;
 protected:
  T *buf;
  long length;
};

/********************
 RingBuffer
********************/

template <class T>
RingBuffer<T> :: RingBuffer()
{
  length = initRingBufferLength;
  buf = (T*) calloc(2*length,sizeof(T));
  readPos = 0;
  writePos = 0;
}

template <class T>
RingBuffer<T> :: ~RingBuffer()
{
  free(buf);
}
  
template <class T>
long RingBuffer<T> :: write(T a)
{
  if(writePos >= 2*length) {
    length *= 2;
    T *newBuf = (T*) calloc(2*length,sizeof(T));
    memmove(newBuf,buf+readPos,(writePos-readPos)*sizeof(T));
    free(buf);
    buf = newBuf;
    writePos -= readPos;
    readPos = 0;
  }
  buf[writePos++] = a;
  return 1;
}

template <class T>
T RingBuffer<T> :: read(long k)
{
  return buf[k];
}

template <class T>
T RingBuffer<T> :: read()
{
  return buf[readPos];
}

template <class T>
long RingBuffer<T> :: nReadable()
{
  return writePos-readPos;
}

template <class T>
void RingBuffer<T> :: advance(long n)
{
  readPos += n;
  if(readPos >= length) {
    memmove(buf,buf+readPos,(writePos-readPos)*sizeof(T));
    writePos = writePos - readPos;
    readPos = 0;
  }
}

template <class T>
void RingBuffer<T> :: clear()
{
  readPos = 0;
  writePos = 0;
}

class SampleBufBase {
 public:
  SampleBufBase() {};
  virtual ~SampleBufBase() {};
  virtual long read(audio *buf, long n)=0;
};

/****************

SampleBuf

****************/

class grain;

template<class T>
class ArrayRingBuffer
{
 public:
  ArrayRingBuffer(int N);
  virtual ~ArrayRingBuffer();
  void clear();
  void grow(long pos);
  void write(T *buf, long n);
  void write(grain *g, int h);
  void read(T *buf, long n);
  void advance(long n);
  long nReadable();
  T *getReadBuf();
  long readPos, writePos;
  int N;
  long length;
  T *buf;
};


template<class T>
ArrayRingBuffer<T> :: ArrayRingBuffer(int N) 
{
  this->N = N;
  this->length = initSampleBufLength;
  this->buf = (T*)calloc(2*length,sizeof(T));
  this->readPos = 0;
  this->writePos = 0;
}

template<class T>
ArrayRingBuffer<T> :: ~ArrayRingBuffer() 
{
  free(buf);
}

template<class T>
void ArrayRingBuffer<T> :: write(T *in, long n)
{
  grow(n);
  if(in) memmove(buf+writePos,in,n*sizeof(T));
  writePos += n;
}

template<class T>
void ArrayRingBuffer<T> :: grow(long n)
{
  long pos = writePos+n;
  while(pos >= 2*length) {
    length *= 2;
    T *newBuf = (T*)calloc(2*length,sizeof(T));
    memmove(newBuf,buf+readPos,(length-readPos)*sizeof(T));
    free(buf);
    buf = newBuf;
    writePos -= readPos;
    pos -= readPos;
    readPos = 0;
  }
}

template<class T>
void ArrayRingBuffer<T> :: read(T *outBuf, long n)
{
  n = max(0L,min(n,nReadable()));
  memmove(outBuf,buf+readPos,n*sizeof(T));
  advance(n);
}

template<class T>
long ArrayRingBuffer<T> :: nReadable()
{
  return max(0L,writePos-readPos);
}

template<class T>
void ArrayRingBuffer<T> :: advance(long n) {
  grow(N);
  memset(buf+readPos,0,n*sizeof(T));
  readPos += n;
  if(readPos >= length) {
    long endPos;
    endPos = writePos+N;
    memmove(buf,buf+readPos,(endPos-readPos)*sizeof(T));
    memset(buf+readPos,0,((length<<1)-readPos)*sizeof(T));
    writePos -= readPos;
    readPos = 0;
  }
}

template<class T>
T *ArrayRingBuffer<T> :: getReadBuf() 
{
  return (buf+readPos);
}

template<class T>
void ArrayRingBuffer<T> :: clear()
{
  advance(writePos-readPos);
}

typedef ArrayRingBuffer<audio> SampleBuf;

class GrainBuf {
 public:
  GrainBuf(int N, int h, int N2, int type);
  ~GrainBuf();
  long write(audio *buf, long n);
  void write(grain *g);
  void advance(long n);
  long nReadable();
  void clear();
  grain* read(long k);
  void reference(grain *g);
  void forget(grain *g);
  audio *getWindowFFT();
  long readPos;
  long writePos;
 protected:
  audio *iBuf;
  grain **buf;
  long length;
  long N2;
  long h;
  long overlap;
  long xOffset;
  long iBufWritePos;
  GrainAllocator grainAllocator;
};

class Mixer : public SampleBufBase {
 public:
  Mixer(SampleBufBase *, SampleBuf *);
  ~Mixer() {}
  virtual long read(audio *buf, long n);
protected:
  SampleBufBase *b1;
  SampleBuf *b2; 
};

}

#endif
