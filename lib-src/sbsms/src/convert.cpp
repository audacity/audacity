#include "config.h"

#include <math.h>
#include "convert.h"
#include "sbsms.h"
#include <stdlib.h>
#include "audio.h"
#ifdef HAVE_SNDFILE
#include "import.h"
#include "pcm.h"
#define BWAV 1
#endif

struct resampleBuf {
  float *buf;
  audio *abuf;
  double ratio;
  long samplesIn;
  long block;
#ifdef BWAV
  AudioDecoder *decoder;
#else
  FILE *decoder;
#endif
};

long resampleCB(void *cb_data, sbsms_resample_frame *data) 
{
  resampleBuf *r = (resampleBuf*) cb_data;
#ifdef BWAV
  long n_read_in = r->decoder->read(r->buf,r->block);
  audio_convert_to(r->abuf,0,r->buf,0,n_read_in);
#else
  long n_read_in = 0;
  for(int k=0;k<r->block;k++) {
    if(fscanf(r->decoder,"%g %g\n",&(r->abuf[k][0]),&(r->abuf[k][1])) == EOF)
      break;
    n_read_in++;
  }
#endif  
  r->samplesIn += n_read_in;
  data->size = n_read_in;
  data->ratio0 = r->ratio;
  data->ratio1 = r->ratio;
  data->in = r->abuf;

  return n_read_in;
}

long samplesCB(audio *chdata, long numFrames, void *userData)
{ 
  sbsmsInfo *si = (sbsmsInfo*) userData;
  long n_read = si->rs->read(chdata, numFrames);
  return n_read;
}

#ifndef BWAV
long lineCount(FILE *fp) {
  long n = 0;
  char c = 0;
  do {
    c = fgetc(fp);
    if (c == '\n') n++;
  } while (c != EOF);
  fseek(fp,0,SEEK_SET);
  return n;
}
#endif

bool sbsms_convert(const char *filenameIn, const char *filenameOut, bool bAnalyzeOnly, bool bSynthesizeOnly, bool bPreAnalyze, int quality, progress_cb progressCB, void *data, real stretch0, real stretch1, real ratio0, real ratio1, real volume)
{  
  bool status = true;
  
  real srOut = 44100.0f;
  int channels;
  long samplesToProcess;
  long samplesIn;
  sbsmsInfo si;
  sbsms *sbsmser = NULL;
  Resampler *resampler = NULL;
  Resampler *resamplerPre = NULL;
  resampleBuf rb;
  rb.buf = NULL;
  rb.abuf = NULL;
  resampleBuf rbPre;
  rbPre.buf = NULL;
  rbPre.abuf = NULL;
  FILE *sbsmsIn = NULL;
  FILE *sbsmsOut = NULL;
  float *fbuf = NULL;
  audio *abuf = NULL;
  pitcher *pitch = NULL;
  float srIn;
  real stretch2;
  long samplesOut;

#ifdef BWAV
  AudioDecoder *decoder = NULL;
  AudioDecoder *decoderPre = NULL;
#else
  FILE *decoder = NULL;
  FILE *decoderPre = NULL;
#endif
  if(bSynthesizeOnly) {
    sbsmsIn = sbsms_open_read(filenameIn);
    if(!sbsmsIn) {
      printf("Cannot open file: :%s\n",filenameIn);
      status = false;
      goto cleanup;
    }
    samplesIn = 0;
    samplesToProcess = sbsms_get_samples_to_process(sbsmsIn);
    channels = sbsms_get_channels(sbsmsIn);
    quality = sbsms_get_quality(sbsmsIn);
    sbsmser = sbsms_create(sbsmsIn,&stretchCBLinear,&ratioCBLinear);
    decoder = NULL;
  } else {
#ifdef BWAV
    decoder = import(filenameIn);
    if(!decoder) {
      printf("File: %s cannot be opened\n",filenameIn);
      exit(-1);
    }
    srIn = (float) decoder->getSampleRate();
    channels = decoder->getChannels();
    samplesIn = decoder->getFrames();
#else
    decoder = fopen(filenameIn,"r");
    if(!decoder) {
      printf("File: %s cannot be opened\n",filenameIn);
      status = false;
      goto cleanup;
    }
    srIn = 44100.0;
    channels = 2;
    samplesIn = lineCount(decoder);
#endif
    samplesToProcess = (long) (samplesIn*srOut/srIn);
    
    rb.ratio = (float)srOut / (float)srIn;
    rb.decoder = decoder;
    rb.block = SBSMS_FRAME_SIZE[quality];
    rb.samplesIn = 0;
    rb.buf = (float*)calloc(rb.block*2,sizeof(float));
    rb.abuf = (audio*)calloc(rb.block,sizeof(audio));
    resampler = new Resampler(resampleCB, &rb);
    si.rs = resampler;
    sbsmser = sbsms_create(&samplesCB,&stretchCBLinear,&ratioCBLinear,channels,quality,bPreAnalyze,!bAnalyzeOnly);
  }
  
  if(bPreAnalyze && !bSynthesizeOnly) {
#ifdef BWAV
    decoderPre = import(filenameIn);
    if(!decoderPre) {
      printf("File: %s cannot be opened\n",filenameIn);
      status = false;
      goto cleanup;
    }
#else
    decoderPre = fopen(filenameIn,"r");
    if(!decoderPre) {
      printf("File: %s cannot be opened\n",filenameIn);
      status = false;
      goto cleanup;
    }
#endif
    rbPre.ratio = (float)srOut / (float)srIn;
    rbPre.decoder = decoderPre;
    rbPre.block = SBSMS_FRAME_SIZE[quality];
    rbPre.samplesIn = 0;
    rbPre.buf = (float*)calloc(rbPre.block*2,sizeof(float));
    rbPre.abuf = (audio*)calloc(rbPre.block,sizeof(audio));
    resamplerPre = new Resampler(resampleCB, &rbPre);
    si.rs = resamplerPre;
  }
  if(stretch0 == stretch1)
    stretch2 = 1.0/stretch0;
  else
    stretch2 = log(stretch1/stretch0)/(stretch1-stretch0);
  samplesOut = samplesToProcess * stretch2;
  si.samplesToProcess = samplesToProcess;
  si.samplesToGenerate = samplesOut;
  si.stretch0 = stretch0;
  si.stretch1 = stretch1;
  si.ratio0 = ratio0;
  si.ratio1 = ratio1;

  if(bPreAnalyze && !bSynthesizeOnly) {
    long pos = 0;
    long lastPos = 0;
    long ret = 0;
    while(lastPos<samplesToProcess) {
      long lastPercent=0;
      
      ret = sbsms_pre_analyze(&samplesCB,&si,sbsmser);
      lastPos = pos;
      pos += ret;
      
      int percent = 100.*(real)lastPos/(real)samplesToProcess;
      //progressCB(percent,"Analysis",data);
    }
    sbsms_pre_analyze_complete(sbsmser);
    sbsms_reset(sbsmser);
    si.rs = resampler;
  }

  if(bAnalyzeOnly) {
    long pos = 0;
    long lastPos = 0;
    long ret = -1;
    sbsmsOut = sbsms_open_write(filenameOut, sbsmser, samplesToProcess);
    if(!sbsmsOut) {
      printf("File: %s cannot be opened\n",filenameOut);
      status = false;
      goto cleanup;
    }
    
    while(lastPos<samplesToProcess && ret) {
      long lastPercent=0;
      
      ret = sbsms_write_frame(sbsmsOut,&si,sbsmser);
      lastPos = pos;
      pos += ret;
      
      int percent = 100.*(real)lastPos/(real)samplesToProcess;
      progressCB(percent,"Progress",data);
    }
  } else {
    fbuf = (float*)calloc(SBSMS_MAX_FRAME_SIZE[quality]*2,sizeof(float));
    abuf = (audio*)calloc(SBSMS_MAX_FRAME_SIZE[quality],sizeof(audio));
    
    pitch = pitch_create(sbsmser,&si,1.0);
    long blockSize = SBSMS_FRAME_SIZE[quality]; 
    
#ifdef BWAV
    PcmWriter writer(filenameOut,samplesOut,(int)srOut,channels);
    if(writer.isError()) {
      printf("File: %s cannot be opened\n",filenameOut);
      status = false;
      goto cleanup;
    }
#else
    FILE *OUT = fopen(filenameOut,"w");
    if(!OUT) {
      printf("File: %s cannot be opened\n",filenameOut);
      status = false;
      goto cleanup;
    }
#endif
    
    long pos = 0;
    long ret = -1;
    
    while(pos<samplesOut && ret) {
      long frames;
      long lastPercent=0;
      
      if(pos+blockSize>samplesOut) {
        frames = samplesOut - pos;
      } else {
        frames = blockSize;
      }
      
      ret = pitch_process(abuf, frames, pitch);
      audio_convert_from(fbuf,0,abuf,0,ret);
      if(channels==1) {
        for(int k=0;k<ret;k++) {
          int k2 = k<<1;
          fbuf[k] = volume*fbuf[k2];
        }
      } else if(channels==2) {
        for(int k=0;k<ret;k++) {
          int k2 = k<<1;
          fbuf[k2] = volume*fbuf[k2];
          fbuf[k2+1] = volume*fbuf[k2+1];
        }
      }
      
#ifdef BWAV
      writer.write(fbuf, ret);
#else
      for(int k=0;k<ret;k++)
        fprintf(OUT,"%g %g\n",fbuf[2*k],fbuf[2*k+1]);
#endif
      pos += ret;
      
      int percent = 100.*(real)pos / (real)samplesOut;
      progressCB(percent,"Progress",data);
    }
    
#ifdef BWAV
    writer.close();
#else
    if(OUT) fclose(OUT);
#endif
  }
  
 cleanup:
#ifdef BWAV
  if(decoderPre) delete decoderPre;
  if(decoder) delete decoder;
#else
  if(decoderPre) fclose(decoderPre);
  if(decoder) fclose(decoder);
#endif
  if(fbuf) free(fbuf);
  if(abuf) free(abuf);
  if(pitch) pitch_destroy(pitch);
  if(sbsmsIn) sbsms_close_read(sbsmsIn);
  if(sbsmsOut) sbsms_close_write(sbsmsOut,sbsmser);
  if(resampler) delete resampler;
  if(resamplerPre) delete resamplerPre;
  if(rb.buf) free(rb.buf);
  if(rb.abuf) free(rb.abuf);
  if(rbPre.buf) free(rbPre.buf);
  if(rbPre.abuf) free(rbPre.abuf);
  if(sbsmser) sbsms_destroy(sbsmser);

  return status;
}
