#ifndef RINGBUFFER_H
#define RINGBUFFER_H

#include <pthread.h>

namespace _sbsms_ {

class AudioBuffer {

 public:
  AudioBuffer(long size, int channels);
  ~AudioBuffer();

  long n_readable();
  long read(float *outputBuffer, long block_size);
  long write(float *buf, long n);
  void writingComplete();
  void flush();
  bool isFull();

 protected:
  bool isWriteReady();
  bool isReadReady();
  void copy(float *out, long outOffset, float *in, long inOffset, long n);
  pthread_cond_t importWriteCondition;
  pthread_mutex_t importWriteMutex;
  pthread_cond_t importReadCondition;
  pthread_mutex_t importReadMutex;
  pthread_mutex_t importMutex;
  bool importWriteReady;
  bool importReadReady;
  bool bWritingComplete;

  int channels;
  float *shareBuf;
  long readBlockSize;
  long shareBufStart;
  long shareBufEnd;
  long shareBufSize;
  long halfShareBufSize;
  long n_done;
};

}

#endif
