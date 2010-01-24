#include <stdlib.h>
#include "audiobuffer.h"
#include "audio.h"
#include "sbsms.h"
#include <pthread.h>
#include <string.h>
#include "utils.h"

namespace _sbsms_ {

void AudioBuffer :: copy(float *out, long outOffset, float *in, long inOffset, long n)
{
  if(channels==1)
    memcpy(out+outOffset,in+inOffset,n*sizeof(float));
  else if(channels==2)
    memcpy(out+(outOffset<<1),in+(inOffset<<1),(n<<1)*sizeof(float));
}

AudioBuffer :: AudioBuffer(long size, int channels)
{
  pthread_cond_init(&importWriteCondition, NULL);
  pthread_mutex_init(&importWriteMutex, NULL);
  pthread_cond_init(&importReadCondition, NULL);
  pthread_mutex_init(&importReadMutex, NULL);
  pthread_mutex_init(&importMutex, NULL);
  
  shareBufStart = 0;
  shareBufEnd = 0;
  readBlockSize = 0;
  
  n_done = 0;
  bWritingComplete = false;
  this->channels = channels;
  shareBuf = new float[channels*size];
  shareBufSize = size;
  halfShareBufSize = size/2;
  importWriteReady = true;
  importReadReady = false;
}

AudioBuffer :: ~AudioBuffer()
{
  delete [] shareBuf;
  pthread_cond_destroy(&importWriteCondition);
  pthread_mutex_destroy(&importWriteMutex);
  pthread_cond_destroy(&importReadCondition);
  pthread_mutex_destroy(&importReadMutex);
  pthread_mutex_destroy(&importMutex);
}

long AudioBuffer :: read(float *outputBuffer, long block_size) 
{
  long n_toread = block_size;
  long n_done_now = 0;

  if(pthread_mutex_lock(&importReadMutex) == 0) {
    readBlockSize = block_size;
    if(pthread_mutex_lock(&importMutex) == 0) {
      importReadReady = isReadReady();
      pthread_mutex_unlock(&importMutex);
    }
    while(!importReadReady) {
      pthread_cond_wait(&importReadCondition, &importReadMutex);
    }
    readBlockSize = 0;
    pthread_mutex_unlock(&importReadMutex);
  }

  if(pthread_mutex_lock(&importMutex) == 0) {
    if(shareBufStart <= shareBufEnd) {
      n_toread = min(n_toread, shareBufEnd-shareBufStart);
      copy(outputBuffer,0,shareBuf,shareBufStart,n_toread);
      n_done_now += n_toread;
    } else {
      n_toread = min(n_toread, shareBufSize-shareBufStart);
      copy(outputBuffer,0,shareBuf,shareBufStart,n_toread);
      n_done_now += n_toread;
      n_toread = block_size - n_done_now;
      n_toread = min(n_toread, shareBufEnd);
      copy(outputBuffer,n_done_now,shareBuf,0,n_toread);
      n_done_now += n_toread;
    }

    shareBufStart += n_done_now;  
    shareBufStart %= shareBufSize;

    importWriteReady = isWriteReady();
    if(importWriteReady) {
      pthread_cond_broadcast(&importWriteCondition);
    }

    pthread_mutex_unlock(&importMutex);
  }

  n_done += n_done_now;
  return n_done_now;  
}

void AudioBuffer :: writingComplete()
{
  if(pthread_mutex_lock(&importMutex) == 0) {
    bWritingComplete = true;
    importReadReady = isReadReady();
    if(importReadReady) {
      pthread_cond_broadcast(&importReadCondition);
    }
    pthread_mutex_unlock(&importMutex);
  }
}

void AudioBuffer :: flush()
{
  if(pthread_mutex_lock(&importMutex) == 0) {
    shareBufStart = 0;
    shareBufEnd = 0;
    readBlockSize = 0;
    bWritingComplete = false;
    pthread_mutex_unlock(&importMutex);
  }
}

long AudioBuffer :: n_readable()
{
  long n = 0;
  if(pthread_mutex_lock(&importMutex) == 0) {
    if(shareBufStart <= shareBufEnd) 
      n = (shareBufEnd-shareBufStart);
    else
      n = (shareBufSize-shareBufStart + shareBufEnd);
    pthread_mutex_unlock(&importMutex);
  }
  return n;
}

bool AudioBuffer :: isReadReady()
{
  if(bWritingComplete)
    return true;
  if(shareBufStart <= shareBufEnd) 
    return (shareBufEnd-shareBufStart >= readBlockSize);
  else
    return (shareBufSize-shareBufStart + shareBufEnd >= readBlockSize);
}

bool AudioBuffer :: isWriteReady() 
{
  bool importWriteReady;
  if(shareBufStart <= shareBufEnd) {
    importWriteReady = (shareBufEnd-shareBufStart < halfShareBufSize);
  } else {
    importWriteReady = (shareBufStart-shareBufEnd >= halfShareBufSize);
  }
  return importWriteReady;
}

bool AudioBuffer :: isFull()
{
  bool bFull = false;
  if(pthread_mutex_lock(&importMutex) == 0) {
    bFull = !isWriteReady();
    pthread_mutex_unlock(&importMutex);
  }  
  return bFull;
}

long AudioBuffer :: write(float *buf, long n)
{
  if(n>=halfShareBufSize) {
    abort();
  }

  if(pthread_mutex_lock(&importWriteMutex) == 0) {
    if(pthread_mutex_lock(&importMutex) == 0) {
      importWriteReady = isWriteReady();
      pthread_mutex_unlock(&importMutex);
    }
    while(!importWriteReady) {
      pthread_cond_wait(&importWriteCondition, &importWriteMutex);
    }
    pthread_mutex_unlock(&importWriteMutex);
  }

  long n_written=0, n_towrite=0;
  if(pthread_mutex_lock(&importMutex) == 0) {
    n_towrite = min(n,shareBufSize-shareBufEnd);
    copy(shareBuf,shareBufEnd,buf,n_written,n_towrite);
    shareBufEnd += n_towrite;
    n_written = n_towrite;
    n_towrite = n - n_written;

    if(n_towrite) {
      shareBufEnd = 0;
      copy(shareBuf,shareBufEnd,buf,n_written,n_towrite);
      shareBufEnd += n_towrite;
      n_written += n_towrite;
    }

    shareBufEnd %= shareBufSize;
    importReadReady = isReadReady();
    if(importReadReady) {
      pthread_cond_broadcast(&importReadCondition);
    }
    pthread_mutex_unlock(&importMutex);
  }
  
  return n_written;
}

}
