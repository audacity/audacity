#include <stdlib.h>
#include <string.h>
#include "sbsms.h"

using namespace _sbsms_;
long pitchCB(void *cb_data, sbsms_resample_frame *frame) 
{
  pitcher *pitch = (pitcher*) cb_data;
  long n_read = sbsms_read_frame(pitch->buf,pitch->sbsmsData,pitch->sbsmser,&(frame->ratio0),&(frame->ratio1));
  frame->ratio0 *= pitch->ratio;
  frame->ratio1 *= pitch->ratio;
  frame->size = n_read;
  frame->in = pitch->buf;
  return n_read;
}

pitcher *pitch_create(sbsms *sbsmser, void *sbsmsData, real ratio)
{
  pitcher *pitch = (pitcher*)calloc(1,sizeof(pitcher));
  pitch->ratio = ratio;
  pitch->bufsize = sbsmser->bufsize;
  pitch->buf = (audio*)calloc(pitch->bufsize,sizeof(audio));
  pitch->postResampler = new Resampler(&pitchCB, pitch);
  pitch->sbsmsData = sbsmsData;
  pitch->sbsmser = sbsmser;
  return pitch;
}

void pitch_destroy(pitcher *pitch) 
{
  delete pitch->postResampler;
  free(pitch->buf);
  free(pitch);
}

void pitch_reset(pitcher *pitch)
{
  pitch->postResampler->reset();
}

long pitch_process(audio *out, long n, pitcher *pitch)
{
  return pitch->postResampler->read(out, n);
}

long pitch_get_samples_queued(pitcher *pitch)
{
  long queued = sbsms_get_samples_queued(pitch->sbsmser);
  long output = pitch->postResampler->samplesInOutput();
  return queued + output;
}
