#ifndef IMPORT_H
#define IMPORT_H

namespace _sbsms_ {

typedef int (*audio_in_cb)(float *buf, long n, int Fs, void *data);

class AudioDecoder {
 public:
  virtual long read(float *buf, long block_size) = 0 ;
  virtual bool done() = 0;
  virtual int getSampleRate() = 0;
  virtual long getFrames() = 0;
  virtual int getChannels() = 0;
  virtual bool isError() = 0;
  virtual ~AudioDecoder() {};
};

AudioDecoder *import(const char *filename);

}

#endif
