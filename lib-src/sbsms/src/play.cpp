#include "config.h"
#ifdef HAVE_PORTAUDIO

#include "sbsms.h"
#include "play.h"
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "audiobuffer.h"

void *writeThreadCB(void *data) {
  sbsmsplayer *player = (sbsmsplayer*)data;
  
  while(player->isPlaying()) {
    player->writeframe();
  }

  player->rb->flush();
  player->rb->writingComplete();
  pitch_reset(player->pitch);
  sbsms_reset(player->sbsmser);
  pthread_exit(NULL);
  return NULL;
}

static int audioCB(const void *inputBuffer, void *outputBuffer,
		   unsigned long framesPerBuffer,
		   const PaStreamCallbackTimeInfo* timeInfo,
		   PaStreamCallbackFlags statusFlags,
		   void *userData )
{
  sbsmsplayer *player = (sbsmsplayer*)userData;
  float *out = (float*)outputBuffer;
  int channels = player->channels;
  unsigned long pos = 0;
  long ret = player->isStarted()?1:0;
  long chunk = framesPerBuffer;
  while(pos<framesPerBuffer && ret) {
    chunk = framesPerBuffer-pos;
    ret = player->readframe(out+pos*channels,chunk);
    pos += ret;
  }
  if(!ret) {
    memset(out+pos*channels,0,sizeof(float)*chunk*channels);
  }
  return paContinue;
}

sbsmsplayer :: sbsmsplayer()
{
  init();
  bLinear = false;
}

void sbsmsplayer :: init()
{
  rb = NULL;
  fbuf = NULL;
  abuf = NULL;
  nullBuf = NULL;
  sbsmsIn = NULL;
  Fs = 44100;
  sbsms_init(4096);
  PaError err;
  err = Pa_Initialize();
  if( err != paNoError ) abort();
  frameOffset = 0;
  bPlaying = false;
  bStarted = false;
  bOpen = false;
  stream = NULL;
  bWriteThread = false;
  pthread_mutex_init(&playMutex, NULL);
  pthread_mutex_init(&writeMutex, NULL);
  playPos = 0;
  stopPos = 0;
  volume = 0.9f;
  rate = 1.0f;
  ratio = 1.0f;
}

sbsmsplayer :: sbsmsplayer(const char *filenameIn, real stretch0, real stretch1, real ratio0, real ratio1)
{
  init();
  si.stretch0 = stretch0;
  si.stretch1 = stretch1;
  si.ratio0 = ratio0;
  si.ratio1 = ratio1;
  bLinear = true;
  open(filenameIn);
}

sbsmsplayer :: ~sbsmsplayer()
{
  PaError err;
  err = Pa_Terminate();
  if( err != paNoError ) abort();
}

void sbsmsplayer :: writeframe()
{
  long n_towrite = 0;
  long n_todrop = 0;
  if(pthread_mutex_lock(&writeMutex) == 0) {
    frameSize = pitch_process(abuf, blockSize, pitch);
    n_towrite = frameSize;
    n_todrop = 0;
    if(frameOffset) {
      n_todrop = min(frameOffset,frameSize);
      frameOffset -= n_todrop;
      n_towrite -= n_todrop;
    }

    pthread_mutex_unlock(&writeMutex);
    if(n_towrite) {
      audio_convert_from(fbuf,0,abuf+n_todrop,0,n_towrite);
      if(channels==1) {
	for(int k=0;k<n_towrite;k++) {
	  int k2 = k<<1;
	  fbuf[k] = volume*fbuf[k2];
	}
      } else if(channels==2) {
	for(int k=0;k<n_towrite;k++) {
	  int k2 = k<<1;
	  fbuf[k2] = volume*fbuf[k2];
	  fbuf[k2+1] = volume*fbuf[k2+1];
	}
      }
      rb->write(fbuf,n_towrite);
    }
  }

}

bool sbsmsplayer :: play()
{
  if(!bOpen) return false;
  if(isPlaying()) {
    return false;
  }
  bDonePlaying = false;
  bPlaying = true;
  stopPos = samplesOut;
  int rc = pthread_create(&writeThread, NULL, writeThreadCB, (void*)this);
  if(rc) {
    fprintf(stderr,"ERROR; return code from pthread_create() is %d\n", rc);
    exit(-1);
  }
  bWriteThread = true;
  while(!rb->isFull()) {
    Pa_Sleep(64);
  }
  bStarted = true;
  return true;
}

bool sbsmsplayer :: pause()
{
  if(!isPlaying()) 
    return false;
  if(pthread_mutex_lock(&playMutex) == 0) {
    stopPos = playPos;
    pthread_mutex_unlock(&playMutex);
  }
  if(bWriteThread) {
    pthread_join(writeThread,NULL);
    bWriteThread = false;
  }
  bStarted = false;
  bPlaying = false;
  setPos(getPos());
  return true;
}

void sbsmsplayer :: close()
{
  if(rb) { delete rb; rb = NULL; }
  if(fbuf) { free(fbuf); fbuf = NULL; }
  if(abuf) { free(abuf); abuf = NULL; }
  if(nullBuf) { free(nullBuf); nullBuf = NULL; }
  if(sbsmsIn) { fclose(sbsmsIn); sbsmsIn = NULL; }

  if(stream) {
    PaError err;
    err = Pa_StopStream( stream );
    if( err != paNoError ) abort();
    err = Pa_CloseStream( stream );
    if( err != paNoError ) abort();
    stream = NULL;
  }
  vSamples.clear();
  vSampleOffset.clear();
  vByteOffset.clear();
  bOpen = false;
  bPlaying = false;
  bStarted = false;
}

bool sbsmsplayer :: open(const char *filenameIn)
{
  sbsmsIn = sbsms_open_read(filenameIn);
  if(!sbsmsIn) {
    printf("Cannot open file %s\n",filenameIn);
    return false;
  }

  samplesToProcess = sbsms_get_samples_to_process(sbsmsIn);
  framesToProcess = sbsms_get_frames_to_process(sbsmsIn);
  channels = sbsms_get_channels(sbsmsIn);
  quality = sbsms_get_quality(sbsmsIn);
  rb = new AudioBuffer(Fs/2,channels);
  bDonePlaying = false;

  if(bLinear) {
    real stretch2;
    if(si.stretch0 == si.stretch1)
      stretch2 = 1.0/si.stretch0;
    else
      stretch2 = log(si.stretch1/si.stretch0)/(si.stretch1-si.stretch0);    
    samplesOut = (long)(samplesToProcess * stretch2);
    si.samplesToProcess = samplesToProcess;
    si.samplesToGenerate = samplesOut;
    sbsmser = sbsms_create(sbsmsIn,&stretchCBLinear,&ratioCBLinear);
  } else {
    samplesOut = samplesToProcess;
    si.stretch0 = 1.0f;
    si.ratio0 = 1.0f;
    sbsmser = sbsms_create(sbsmsIn,&stretchCBConstant,&ratioCBConstant);
    readFooter(sbsmsIn);
  }
  sbsms_seek_start_data(sbsmsIn);
  pitch = pitch_create(sbsmser,&si,1.0);

  fbuf = (float*)calloc(SBSMS_MAX_FRAME_SIZE[quality]*2,sizeof(float));
  abuf = (audio*)calloc(SBSMS_MAX_FRAME_SIZE[quality],sizeof(audio));
  nullBuf = (float*)calloc(SBSMS_MAX_FRAME_SIZE[quality]*2,sizeof(float));

  blockSize = SBSMS_FRAME_SIZE[quality]; 

  bOpen = true;
  playPos = 0;
  frameOffset = 0;
  
  PaError err;
  PaStreamParameters outputParameters;
  outputParameters.device = Pa_GetDefaultOutputDevice();
  outputParameters.channelCount = channels;
  outputParameters.sampleFormat = paFloat32;
  outputParameters.suggestedLatency = .1;
  outputParameters.hostApiSpecificStreamInfo = NULL;

  PaStreamFlags flags = paNoFlag;

  err = Pa_OpenStream( &stream,
		       NULL,
		       &outputParameters,
		       Fs,
		       blockSize,
		       flags,
		       audioCB,
		       this);

  if( err != paNoError ) {
    printf("pa error %d\n",err);
    exit(-1);
  }

  err = Pa_StartStream( stream );
  
  if( err != paNoError ) {
    printf("pa error %d\n",err);
    exit(-1);
  }

  setRate(rate);
  setRatio(ratio);
  
  return true;
}

long sbsmsplayer :: readframe(float *buf, long n)
{
  long ret = 0;
  if(pthread_mutex_lock(&playMutex) == 0) {
    ret = rb->read(buf,n);
    playPos += ret;
    if(playPos >= stopPos) {
      bPlaying = false;
      bDonePlaying = true;
    }
    pthread_mutex_unlock(&playMutex);
  }
  return ret;
}

bool sbsmsplayer :: isStarted()
{
  return bStarted;
}

bool sbsmsplayer :: isPlaying()
{  
  return bPlaying;
}

bool sbsmsplayer :: isDonePlaying()
{
  return bDonePlaying;
}

real sbsmsplayer :: getPos()
{
  real pos = 0.0f;
  if(pthread_mutex_lock(&playMutex) == 0) {
    pos = (real)playPos/(real)samplesOut;
    pthread_mutex_unlock(&playMutex);
  }
  return pos;
}

real sbsmsplayer :: getTime()
{
  real t = 0.0f;
  if(pthread_mutex_lock(&playMutex) == 0) {
    t = (real)playPos/(real)Fs;
    pthread_mutex_unlock(&playMutex);
  }

  return t;
}

void sbsmsplayer :: readFooter(FILE *fp)
{
  long frames = sbsms_get_frames_to_process(fp);
  long footerFrameSize = 2*sizeof(long);
  long footerSize = frames*footerFrameSize;
  fseek(fp,-footerSize,SEEK_END);
  long samplesAcc = 0;
  for(int k=0;k<frames;k++) {
    long offset;
    long samples;
    fread(&offset,sizeof(long),1,fp);
    fread(&samples,sizeof(long),1,fp);
    vByteOffset.push_back(offset);
    vSamples.push_back(samples);
    vSampleOffset.push_back(samplesAcc);
    samplesAcc += samples;
  }
}

long sbsmsplayer :: getFrameIndexForSamplePos(long samplePos)
{
  if(samplePos<0) samplePos = 0;
  else if(samplePos>=samplesToProcess) samplePos = samplesToProcess-1;
  long i = vSampleOffset.size()>>1;
  long imin = 0;
  long imax = (long)vSampleOffset.size()-1;
  bool bDone = false;
  do {
    bDone = (i==0 || i==(long)vSampleOffset.size()-1 || 
	     (vSampleOffset[i]<=samplePos && vSampleOffset[i+1] > samplePos));
    if(!bDone) {
      if(vSampleOffset[i] < samplePos) {
	imin = i;
	if(imin+1==imax)
	  i = imax;
	else
	  i = (imin+imax)>>1;
      } else {
	imax = i;
	if(imin+1==imax)
	  i = imin;
	else
	  i = (imin+imax)>>1;
      }
    }
  } while(!bDone);
  return i;
}

bool sbsmsplayer :: setPos(real pos)
{
  if(isPlaying())
    return false;
  
  bDonePlaying = false;
  bStarted = false;
  if(bOpen) {
    long samplePos = (long)(pos*samplesToProcess);
    long i = getFrameIndexForSamplePos(samplePos);
    if(bWriteThread) {
      pthread_join(writeThread,NULL);
      bWriteThread = false;
    }
    sbsms_seek(sbsmser,i,vSampleOffset[i]);
    fseek(sbsmsIn,vByteOffset[i],SEEK_SET);
    real stretch = 1.0f;
    if(pthread_mutex_lock(&writeMutex) == 0) {
      stretch = 1.0f/si.stretch0;
      frameOffset = (long)(stretch*(real)(samplePos - vSampleOffset[i]));
      pthread_mutex_unlock(&writeMutex);
    }
    if(pthread_mutex_lock(&playMutex) == 0) {
      samplesOut =(long)(samplesToProcess*stretch);
      stopPos = samplesOut;
      playPos = (long)(pos*samplesToProcess*stretch);
      pthread_mutex_unlock(&playMutex);
    }
  }
  return true;
}

void sbsmsplayer :: setLength() 
{
  real stretch = 1.0f;
  long nFramesQueued = 0;
  long nSamplesQueued = 0;
  long nSamplesReadable = 0;
  long inputFrameSize = 0;
  long framePos = 0;
  if(pthread_mutex_lock(&writeMutex) == 0) {
    stretch = 1.0f/si.stretch0;
    nFramesQueued = sbsms_get_frames_queued(sbsmser);
    nSamplesQueued = pitch_get_samples_queued(pitch);
    nSamplesReadable = rb->n_readable();
    inputFrameSize = sbsms_get_last_input_frame_size(sbsmser);
    framePos = sbsms_get_frame_pos(sbsmser);
    pthread_mutex_unlock(&writeMutex);
  }

  long nSamplesLeft = (long)((samplesToProcess-(framePos+nFramesQueued)*inputFrameSize)*stretch);
  long newSamplesOut = playPos + nSamplesReadable + nSamplesQueued + nSamplesLeft;
  if(pthread_mutex_lock(&playMutex) == 0) {
    samplesOut = newSamplesOut;
    stopPos = newSamplesOut;
    pthread_mutex_unlock(&playMutex);
  }
}

real sbsmsplayer :: getDuration()
{
  return (real)(samplesOut)/(real)(Fs);
}

void sbsmsplayer :: setVolume(real vol)
{
  this->volume = vol;
}
  
void sbsmsplayer :: setRate(real rate)
{
  this->rate = rate;
  if(pthread_mutex_lock(&writeMutex) == 0) {
	this->si.stretch0 = rate;
    pthread_mutex_unlock(&writeMutex);
  }
  setLength();
}

void sbsmsplayer :: setRatio(real ratio)
{
  this->ratio = ratio;
  if(pthread_mutex_lock(&writeMutex) == 0) {	 
    this->si.ratio0 = ratio;
    this->si.ratio1 = ratio;
    pthread_mutex_unlock(&writeMutex);
  }
}

real sbsmsplayer :: getVolume()
{
  return volume;
}

real sbsmsplayer :: getRate()
{
  return rate;
}

real sbsmsplayer :: getRatio()
{
  return ratio;
}

#endif
