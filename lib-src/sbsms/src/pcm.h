#ifndef PCM_H
#define PCM_H

#include "config.h"

#ifdef HAVE_SNDFILE

#include <sndfile.h>
#include "import.h"

namespace _sbsms_ {

#define PCM_READ_BUF_SIZE 4096
#define PCM_WRITE_BUF_SIZE 4096

class PcmReader : public AudioDecoder {
 public:
  PcmReader(const char *filename);
  ~PcmReader();
  long decode(audio_in_cb, long block_size, void *data);
  long read(float *buf, long block_size);
  int getChannels();
  int getSampleRate();
  long getFrames();
  bool isError();

  bool done();

 protected:
  bool bError;
  bool bDone;
  long total;
  SF_INFO info;
  SNDFILE *in;
};

class PcmWriter  {
 public:
  PcmWriter(const char *filename,long,int,int);
  ~PcmWriter();
  long write(float *buf, long block_size);
  void close();
  bool isError();

 protected:
  bool bError;
  long total;
  SF_INFO info;
  SNDFILE *out;

};

}

#endif

#endif
