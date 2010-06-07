#ifndef SBSMS_INCLUDE
#define SBSMS_INCLUDE

#include <stdio.h>

#define SBSMS_REAL_FLOAT 1

namespace _sbsms_ {
typedef float real;
typedef real interleaved[2];
typedef interleaved audio;

struct sbsms_quality {
  long inframesize;
  long maxoutframesize;
  real minrate;
  real maxrate;
  int bands;
  int M_MAX;
  int H[5];
  int N[8];
  int S[8];
  int res[8];
  real pad[8];
  real P[8];
  real Q[8];
};

extern const sbsms_quality sbsms_quality_standard;
extern const sbsms_quality sbsms_quality_fast;

struct sbsms_resample_frame {
  real ratio0;
  real ratio1;
  audio *in;
  long size;
};

typedef long (*sbsms_cb)(audio *buf, long n, void *data);
typedef real (*sbsms_rate_cb)(long nProcessed, void *data);
typedef real (*sbsms_pitch_cb)(long nProcessed, void *data);
typedef long (*sbsms_resample_cb)(void *cb_data, sbsms_resample_frame *frame);

class subband;
class TrackAllocator;
class PeakAllocator;

struct sbsms {
  FILE *fp;
  sbsms_cb getSamplesCB;
  sbsms_rate_cb getRateCB;
  sbsms_pitch_cb getPitchCB;
  subband *top;
  TrackAllocator *ta;
  PeakAllocator *pa;
  long n_prepad, n_postpad, n_prespent, n_processed;
  bool bWritingComplete;
  sbsms_quality quality;
  int channels;
  audio *ina;
  void *threadData;
};

class SampleBufBase {
 public:
  SampleBufBase() {};
  virtual ~SampleBufBase() {};
  virtual long read(audio *buf, long n)=0;
  virtual void advance(long n)=0;
  virtual long n_readable()=0;
};

class grain;

class SampleBuf {
 public:
  //SampleBuf() {};
  SampleBuf(int N);
  SampleBuf(int N, long delay);
  void init(int N, long delay);
  void clear();
  virtual ~SampleBuf();

  void grow(long pos);
  long write(audio *buf, long n);
  long write(grain* g, int h);
  virtual long read(audio *buf, long n);
  virtual void advance(long n);
  virtual long n_readable();
  audio *getReadBuf();

  long readPos, writePos;
  long delay;

  int N;
  long length;
  audio *buf;  
};

class Resampler {
 public:
  Resampler(sbsms_resample_cb func, void *data);
  Resampler(SampleBuf *in, real pitch);
  ~Resampler();
  long read(audio *audioOut, long frames);
  void writingComplete();
  void reset();
  void init();
  long samplesInOutput();

 protected:
  sbsms_resample_frame frame;
  long startAbs;
  long midAbs;
  real midAbsf;
  long endAbs;
  long writePosAbs;
  bool bInput;
  SampleBuf *out;
  sbsms_resample_cb cb;
  void *data;
  bool bPull;
  SampleBuf *in;
  long inOffset;
  real sincZeros;
  bool bWritingComplete;
};

void sbsms_init(int n);
void sbsms_reset(sbsms *sbsmser);
void sbsms_seek(sbsms *sbsmser, long framePos, long samplePos);
sbsms* sbsms_create(sbsms_cb getSamplesCB, sbsms_rate_cb getRateCB, sbsms_pitch_cb getPitchCB, int channels, sbsms_quality *quality, bool bPreAnalyze, bool bSynthesize);
sbsms* sbsms_create(FILE *fp, sbsms_rate_cb getRateCB, sbsms_pitch_cb getPitchCB);
void sbsms_destroy(sbsms* sbsmser);
long sbsms_read_frame(audio *out, void *data, sbsms *sbsmer, real *pitch0, real *pitch1);
long sbsms_write_frame(FILE *fp, void *data, sbsms *sbsmser);
long sbsms_samples_processed(sbsms *sbsmser);
long sbsms_pre_analyze(sbsms_cb getSamplesCB, void *data, sbsms *sbsmser);
void sbsms_pre_analyze_complete(sbsms *sbsmser);

long sbsms_get_samples_queued(sbsms *sbsmser);
long sbsms_get_frames_queued(sbsms *sbsmser);
long sbsms_get_last_input_frame_size(sbsms *sbsmser);
long sbsms_get_frame_pos(sbsms *sbsmser);

void sbsms_close_write(FILE *fp, sbsms *sbsmser);
FILE *sbsms_open_write(const char *fileName, sbsms *sbsmser, long samples_to_process);
void sbsms_close_read(FILE *fp);
FILE *sbsms_open_read(const char *fileName);
long sbsms_get_samples_to_process(FILE *fp);
long sbsms_get_frames_to_process(FILE *fp);
long sbsms_get_channels(FILE *fp);
void sbsms_get_quality(FILE *fp, sbsms_quality *quality);
void sbsms_seek_start_data(FILE *fp);

struct sbsmsInfo {
  real rate0, rate1;
  real pitch0, pitch1;
  long samplesToProcess;
  long samplesToGenerate;
  Resampler *rs;
};

long getLinearOutputSamples(sbsmsInfo *si);
real rateCBLinear(long nProcessed, void *userData);
real rateCBConstant(long nProcessed, void *userData);
real pitchCBLinear(long nProcessed, void *userData);
real pitchCBConstant(long nProcessed, void *userData);

}

#endif
