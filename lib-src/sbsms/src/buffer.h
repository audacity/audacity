#ifndef BUFFER_H
#define BUFFER_H

#include <assert.h>
#include <cstring>
#include <cstdlib>
#include "sbsms.h"
#include "grain.h"
#include "trackpoint.h"
#include <list>
using namespace std;

namespace _sbsms_ {

typedef list<trackpoint*> tplist;

template <class T>
class RingBuffer {
 public:
  RingBuffer();
  ~RingBuffer();
  
  long write(T a);
  T read(long k);
  long n_readable();
  void advance(long n);
  void clear();

  long readPos;
  long writePos;
 protected:
  T *buf;
  long length;
};

#define INIT_RINGBUF_LENGTH 128

/********************
 RingBuffer
********************/

template <class T>
RingBuffer<T> :: RingBuffer()
{
  length = INIT_RINGBUF_LENGTH;
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
    memcpy(newBuf,buf+readPos,(writePos-readPos)*sizeof(T));
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
long RingBuffer<T> :: n_readable()
{
  return writePos-readPos;
}

template <class T>
void RingBuffer<T> :: advance(long n)
{
  assert(readPos+n <= writePos);
  readPos += n;
  if(readPos >= length) {
    memcpy(buf,buf+readPos,(writePos-readPos)*sizeof(T));
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

class TrackPointListBuffer {
 public:
  TrackPointListBuffer();
  ~TrackPointListBuffer();
  
  long write(tplist *tpl);
  tplist *read(long k);
  long n_readable();
  void advance(long n);
  
  long readPos;
  long writePos;

 protected:
  tplist **buf;  
  long length;
};

class GrainBuf {
 public:
  GrainBuf(int N, int h);
  GrainBuf(int N, int h, real pad);
  ~GrainBuf();

  void init(int N, int h, real pad);
  long write(audio *buf, long n);
  void write(grain *g);
  void advance(long n);
  long n_readable() { return writePos - readPos; }
  void clear();
  grain* read(long k);

  long length;
  long readPos, writePos;
  int N,h;
  real pad;

 protected:
  void convert(audio *buf);
  audio *iBuf;
  long iBufWritePos;
  grain **buf;
  
};

class Mixer : public SampleBufBase {
 public:
  Mixer(SampleBufBase *, SampleBuf *);
  long read(audio *buf, long n);
  void advance(long n);
  long n_readable();
  ~Mixer() {}

protected:
  SampleBuf *buf;
  SampleBufBase *b1;
  SampleBuf *b2;
 
};

}

#endif
