#ifndef SBSMS_INCLUDE
#define SBSMS_INCLUDE

#include <stdio.h>

#define SBSMS_REAL_FLOAT 1

namespace _sbsms_ {
typedef float real;
typedef real interleaved[2];
typedef interleaved audio;
}

#define SBSMS_WINDOW SBSMS_HANN
extern _sbsms_::real SBSMS_P1;
extern _sbsms_::real SBSMS_Q1;
#define SBSMS_S 4
#define SBSMS_MAX_BANDS 7
#define SBSMS_QUALITIES 3
#define SBSMS_MAX_STRETCH 12
#define SBSMS_MIN_STRETCH 1.0/12.0
extern int SBSMS_BANDS[SBSMS_QUALITIES];
extern int SBSMS_M_MAX[SBSMS_QUALITIES];
extern int SBSMS_FRAME_SIZE[SBSMS_QUALITIES];
extern int SBSMS_MAX_FRAME_SIZE[SBSMS_QUALITIES];
extern int SBSMS_H[SBSMS_QUALITIES][5];
extern _sbsms_::real SBSMS_PAD[SBSMS_QUALITIES][SBSMS_MAX_BANDS];
extern int SBSMS_RES[SBSMS_QUALITIES][SBSMS_MAX_BANDS];
extern int SBSMS_N[SBSMS_QUALITIES][SBSMS_MAX_BANDS];

struct sbsms_resample_frame {
  _sbsms_::real ratio0;
  _sbsms_::real ratio1;
  _sbsms_::audio *in;
  long size;
};

typedef long (*sbsms_cb)(_sbsms_::audio *buf, long n, void *data);
typedef _sbsms_::real (*sbsms_stretch_cb)(long nProcessed, void *data);
typedef _sbsms_::real (*sbsms_ratio_cb)(long nProcessed, void *data);
typedef long (*sbsms_resample_cb)(void *cb_data, sbsms_resample_frame *frame);

namespace _sbsms_ {
  class subband;
  class TrackAllocator;
  class PeakAllocator;
}

struct sbsms {
  FILE *fp;
  sbsms_cb getSamplesCB;
  sbsms_stretch_cb getStretchCB;
  sbsms_ratio_cb getRatioCB;
  _sbsms_::subband *top;
  _sbsms_::TrackAllocator *ta;
  _sbsms_::PeakAllocator *pa;
  long n_prepad, n_postpad, n_prespent, n_processed;
  bool bWritingComplete;
  int quality, channels;
  long bufsize;
  long chunksize;
  _sbsms_::audio *ina;
  void *threadData;
};

namespace _sbsms_ {

class SampleBufBase {
 public:
  SampleBufBase() {};
  virtual ~SampleBufBase() {};
  virtual long read(_sbsms_::audio *buf, long n)=0;
  virtual void advance(long n)=0;
  virtual long n_readable()=0;
};


class grain;
class SampleBuf {
 public:
  SampleBuf() {};
  SampleBuf(int N);
  SampleBuf(int N, long delay);
  void init(int N, long delay);
  void clear();
  virtual ~SampleBuf();

  void grow(long pos);
  long write(audio *buf, long n);
  long write(grain* g, int h);
  virtual long read(_sbsms_::audio *buf, long n);
  virtual void advance(long n);
  virtual long n_readable();
  audio *getReadBuf();

  long readPos, writePos;
  long delay;

  int N;
  long length;
  audio *buf;  
};

}


using namespace _sbsms_;
class Resampler {
 public:
  Resampler(sbsms_resample_cb func, void *data);
  Resampler(SampleBuf *in, real ratio);
  ~Resampler();
  long read(_sbsms_::audio *audioOut, long frames);
  void writingComplete();
  void reset();
  void init();
  long samplesInOutput();

 protected:
  sbsms_resample_frame frame;
  long startAbs;
  long midAbs;
  _sbsms_::real midAbsf;
  long endAbs;
  long writePosAbs;
  bool bInput;
  _sbsms_::SampleBuf *out;
  sbsms_resample_cb cb;
  void *data;
  bool bPull;
  SampleBuf *in;
  long inOffset;
  real sincZeros;
  bool bWritingComplete;
};

struct pitcher {
  void *sbsmsData;
  sbsms *sbsmser;
  _sbsms_::real ratio;
  Resampler *postResampler;
  _sbsms_::audio *buf;
  long bufsize;
};

void sbsms_init(int n);
void sbsms_reset(sbsms *sbsmser);
void sbsms_seek(sbsms *sbsmser, long framePos, long samplePos);
sbsms* sbsms_create(sbsms_cb getSamplesCB, sbsms_stretch_cb getStretchCB, sbsms_ratio_cb getRatioCB, int channels, int quality, bool bPreAnalyze, bool bSynthesize);
sbsms* sbsms_create(FILE *fp, sbsms_stretch_cb getStretchCB, sbsms_ratio_cb getRatioCB);
void sbsms_destroy(sbsms* sbsmser);
long sbsms_read_frame(_sbsms_::audio *out, void *data, sbsms *sbsmer, _sbsms_::real *ratio0, _sbsms_::real *ratio1);
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
long sbsms_get_quality(FILE *fp);
void sbsms_seek_start_data(FILE *fp);


pitcher *pitch_create(sbsms *sbsmser, void *sbsmsData, _sbsms_::real ratio);
void pitch_reset(pitcher *pitch);
void pitch_destroy(pitcher *pitch);
long pitch_process(_sbsms_::audio *out, long n, pitcher *pitch);
long pitch_get_samples_queued(pitcher *pitch);

struct sbsmsInfo {
  float stretch0, stretch1;
  float ratio0, ratio1;
  long samplesToProcess;
  long samplesToGenerate;
  Resampler *rs;
};

real stretchCBLinear(long nProcessed, void *userData);
real stretchCBConstant(long nProcessed, void *userData);
real ratioCBLinear(long nProcessed, void *userData);
real ratioCBConstant(long nProcessed, void *userData);

#endif
