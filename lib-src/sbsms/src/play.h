#include "config.h"
#ifdef HAVE_PORTAUDIO

#ifndef SBSMSPLAY_H
#define SBSMSPLAY_H

#include "portaudio.h"
#include "audio.h"
#include "utils.h"
#include <pthread.h>
#include "audiobuffer.h"
#include <stdio.h>
#include "sbsms.h"
#include <vector>
using namespace std;

class sbsmsplayer {
 public:
  sbsmsplayer();
  sbsmsplayer(const char *filenameIn, real stretch0, real stretch1, real ratio0, real ratio1);
  ~sbsmsplayer();

  bool play();
  bool pause();
  void close();
  bool open(const char *filename);
  real getPos();
  bool setPos(real pos);
  real getTime();
  real getDuration();
  void setVolume(real vol);
  void setRate(real rate);
  void setRatio(real ratio);
  real getVolume();
  real getRate();
  real getRatio();
  void readFooter(FILE *fp);
  long getFrameIndexForSamplePos(long samplePos);
  long readframe(float *buf, long n);
  bool isPlaying();
  bool isStarted();
  bool isDonePlaying();
  void writeframe();

  int channels;
  AudioBuffer *rb;
  sbsms *sbsmser;
  pitcher *pitch;

 protected:
  void setLength();
  real ratio;
  real volume;
  real rate;
  long playPos;
  long stopPos;
  long frameSize;
  long frameOffset;
  long samplesToProcess;
  long framesToProcess;
  long samplesOut;

  bool bWriteThread;
  bool bOpen;
  bool bStarted;
  bool bPlaying;
  bool bDonePlaying;
  bool bLinear;

  FILE *sbsmsIn;
  void init();
  PaStream *stream;
  int quality;
  int Fs;
  pthread_t writeThread;


  pthread_mutex_t playMutex;
  pthread_mutex_t writeMutex;

  vector<long> vByteOffset;
  vector<long> vSampleOffset;
  vector<long> vSamples;
  
  long blockSize;
  float *fbuf;
  audio *abuf;
  float *nullBuf;
  sbsmsInfo si;
};


#endif

#endif
