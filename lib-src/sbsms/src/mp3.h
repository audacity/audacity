#ifndef MP3_H
#define MP3_H

#include <iostream>
#include <fstream>
#include "import.h"
#include "mad.h"
#include "audiobuffer.h"
using namespace std;
#include <pthread.h>

namespace _sbsms_ {

class MP3Reader : public AudioDecoder {
 public:
  MP3Reader(const char *filename);
  ~MP3Reader();
  bool isError();
  long read(float *buf, long block_size);
  int getSampleRate();
  int getChannels();
  long getFrames();
  bool done();
  float* getOutputBuffer(long nsamples);

  bool bError;
  int channels;
  int sampleRate;
  long samples;
  float *outputBuffer;
  unsigned char *inputBuffer;
  bool bFirst;
  long n_done;
  AudioBuffer *rb;
  bool bDone;
  struct mad_decoder decoder;
  pthread_t importThread;
  ifstream *file;
  audio_in_cb cb;
  void *data;
};

}

#endif
