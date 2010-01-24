#include "sbsms.h"
#include "real.h"
#include "subband.h"
#include "track.h"
#include "utils.h"
#include <stdlib.h>
#include <string.h>
#include "config.h"
#ifdef MULTITHREADED
#ifdef _WIN32
#include <windows.h>
#include <winbase.h>
#else
#include <unistd.h>
#endif
#include <pthread.h>
#endif

using namespace _sbsms_;

real SBSMS_P1 = 2.0f;
real SBSMS_Q1 = (4.0f/3.0f);
int SBSMS_FRAME_SIZE[SBSMS_QUALITIES] = {512,512,512};
int SBSMS_BANDS[SBSMS_QUALITIES] = {7,7,7};
int SBSMS_MAX_FRAME_SIZE[SBSMS_QUALITIES] = {SBSMS_MAX_STRETCH*512,SBSMS_MAX_STRETCH*512,SBSMS_MAX_STRETCH*512};
int SBSMS_H[SBSMS_QUALITIES][5] = {{8,12,16,24,32},{6,8,8,8,12},{4,6,8,12,16}};
int SBSMS_M_MAX[SBSMS_QUALITIES] = {64,64,64};
int SBSMS_RES[SBSMS_QUALITIES][SBSMS_MAX_BANDS]  = {{1,1,1,2,2,1,1}, {1,1,2,2,1,2,1}, {1,1,2,2,2,2,1}};
real SBSMS_PAD[SBSMS_QUALITIES][SBSMS_MAX_BANDS]  = {{2,2,4,4,4,4,4},{2,2,4,4,4,4,6},{4,4,4,4,4,6}};
int SBSMS_N[SBSMS_QUALITIES][SBSMS_MAX_BANDS] = {{384,384,576,480,384,288,252},{384,384,576,480,384,288,336},{768,768,576,480,384,288,288}};


#ifdef MULTITHREADED

struct assign_data {
  sbsms *sbsmser;
  int c;
  bool bFirst;
  bool bLast;
};

struct thread_data {
  pthread_mutex_t writeMutex;
  pthread_cond_t writeCond;

  pthread_t addThread;
  pthread_mutex_t addMutex;
  pthread_cond_t addCond;
  bool bAddThread;

  pthread_t assignThread[2];
  pthread_mutex_t assignMutex;
  pthread_cond_t assignCond;
  bool bAssignThread;
  assign_data assignData[2];

  pthread_t synthThread;
  pthread_mutex_t synthMutex;
  pthread_cond_t synthCond;
  bool bSynthThread;

  // hack to break out of possible (but very rare) deadlocks caused by checking 
  // condition predicates after broadcasting conditions
  pthread_t pollThread;
  
  bool bActive;
};

void *pollThreadCB(void *data) {
  sbsms *sbsmser = (sbsms*)data;
  thread_data *threadData = (thread_data*)sbsmser->threadData;
  subband *top = sbsmser->top;
  
  while(threadData->bActive) {
    if(top->isWriteReady()) {
      pthread_mutex_lock(&threadData->writeMutex);
      pthread_cond_broadcast(&threadData->writeCond);    
      pthread_mutex_unlock(&threadData->writeMutex);
    }
    if(top->isAddReady()) {
      pthread_mutex_lock(&threadData->addMutex);
      pthread_cond_broadcast(&threadData->addCond);
      pthread_mutex_unlock(&threadData->addMutex);
    } 
    if(top->isMarkReady()) {
      pthread_mutex_lock(&threadData->assignMutex);
      pthread_cond_broadcast(&threadData->assignCond);
      pthread_mutex_unlock(&threadData->assignMutex);
    }
    if(top->isSynthReady()) {
      if(threadData->bSynthThread) {
	pthread_mutex_lock(&threadData->synthMutex);
	pthread_cond_broadcast(&threadData->synthCond);
	pthread_mutex_unlock(&threadData->synthMutex);
      }
      pthread_mutex_lock(&threadData->writeMutex);
      pthread_cond_broadcast(&threadData->writeCond);
      pthread_mutex_unlock(&threadData->writeMutex);
    }
#ifdef _WIN32
    Sleep(100);
#else
    usleep(100000);
#endif
  }
  
  pthread_exit(NULL);
  return NULL;
}

void *addThreadCB(void *data) {
  sbsms *sbsmser = (sbsms*)data;
  thread_data *threadData = (thread_data*)sbsmser->threadData;
  subband *top = sbsmser->top;

  while(threadData->bActive) {
    if(!top->isAddReady()) {
      pthread_mutex_lock(&threadData->addMutex);
      pthread_cond_wait(&threadData->addCond,&threadData->addMutex);
      pthread_mutex_unlock(&threadData->addMutex);
    }
    while(top->addInit(true)) {
      top->addTrackPoints();
      top->stepAddFrame();
      if(top->isMarkReady()) {
	pthread_mutex_lock(&threadData->assignMutex);
	pthread_cond_broadcast(&threadData->assignCond);
	pthread_mutex_unlock(&threadData->assignMutex);
      }
      if(top->isWriteReady()) {
	pthread_mutex_lock(&threadData->writeMutex);
	pthread_cond_broadcast(&threadData->writeCond);
	pthread_mutex_unlock(&threadData->writeMutex);
      }
    }
  }
  pthread_exit(NULL);
  return NULL;
}

void *assignThreadCB(void *data) {
  assign_data *assignData = (assign_data*)data;
  sbsms *sbsmser = assignData->sbsmser;
  int c = assignData->c;
  bool bFirst = assignData->bFirst;
  bool bLast = assignData->bLast;
  thread_data *threadData = (thread_data*)sbsmser->threadData;
  subband *top = sbsmser->top;

  while(threadData->bActive) {
    if(!top->markInit(false,c)) {
      pthread_mutex_lock(&threadData->assignMutex);
      pthread_cond_wait(&threadData->assignCond,&threadData->assignMutex);
      pthread_mutex_unlock(&threadData->assignMutex);
    }
    while(top->markInit(true,c)) {
      top->markDuplicates(c);
      top->stepMarkFrame(c);      
      if(top->isAddReady()) {
	pthread_mutex_lock(&threadData->addMutex);
	pthread_cond_broadcast(&threadData->addCond);
	pthread_mutex_unlock(&threadData->addMutex);
      }
    }      
    while(top->assignInit(true,c)) {
      top->assignTrackPoints(c);
      top->advanceTrackPoints(c);
      top->stepAssignFrame(c);
      if(top->isSynthReady()) {
	if(threadData->bSynthThread) {
	  pthread_mutex_lock(&threadData->synthMutex);
	  pthread_cond_broadcast(&threadData->synthCond);
	  pthread_mutex_unlock(&threadData->synthMutex);
	}
	pthread_mutex_lock(&threadData->writeMutex);
	pthread_cond_broadcast(&threadData->writeCond);
	pthread_mutex_unlock(&threadData->writeMutex);
      }
    }	
  }
  pthread_exit(NULL);
  return NULL;
}

void *synthThreadCB(void *data) {
  sbsms *sbsmser = (sbsms*)data;
  thread_data *threadData = (thread_data*)sbsmser->threadData;
  subband *top = sbsmser->top;

  while(threadData->bActive) {
    if(!top->isSynthReady()) {
      pthread_mutex_lock(&threadData->synthMutex);
      pthread_cond_wait(&threadData->synthCond,&threadData->synthMutex);
      pthread_mutex_unlock(&threadData->synthMutex);
    }
    while(top->synthInit(true)) {
      top->synthTracks();
      top->stepSynthFrame();
      if(top->isAssignReady()) {
	pthread_mutex_lock(&threadData->assignMutex);
	pthread_cond_wait(&threadData->assignCond,&threadData->assignMutex);
	pthread_mutex_unlock(&threadData->assignMutex);		
      }
    }
  }
  pthread_exit(NULL);
  return NULL;
}

void sbsms_init_threads(sbsms *sbsmser, bool bAnalyze, bool bSynthesize)
{
  thread_data *threadData = (thread_data*)calloc(1,sizeof(thread_data));
  sbsmser->threadData = threadData;
  threadData->bActive = true;

  pthread_cond_init(&threadData->writeCond, NULL);
  pthread_mutex_init(&threadData->writeMutex, NULL);

  if(bAnalyze) {
    pthread_cond_init(&threadData->addCond, NULL);
    pthread_mutex_init(&threadData->addMutex, NULL);
    threadData->bAddThread = true;

    pthread_cond_init(&threadData->assignCond, NULL);    
    pthread_mutex_init(&threadData->assignMutex, NULL);
    threadData->bAssignThread = true;

    for(int c=0;c<sbsmser->channels;c++) {
      threadData->assignData[c].sbsmser = sbsmser;
      threadData->assignData[c].c = c;
      threadData->assignData[c].bFirst = (c==0);
      threadData->assignData[c].bLast = (c==sbsmser->channels-1);
    }
  } else {
    threadData->bAddThread = false;
    threadData->bAssignThread = false;
  }

  if(!bAnalyze) bSynthesize = false;

  if(bSynthesize) {
    pthread_cond_init(&threadData->synthCond, NULL);
    pthread_mutex_init(&threadData->synthMutex, NULL);
    threadData->bSynthThread = true;
  } else {
    threadData->bSynthThread = false;
  }

  if(bAnalyze) {
    pthread_create(&threadData->addThread, NULL, addThreadCB, (void*)sbsmser);
    for(int c=0;c<sbsmser->channels;c++) {
      pthread_create(&threadData->assignThread[c], NULL, assignThreadCB, (void*)(&threadData->assignData[c]));
    }
  }
  if(bSynthesize) {
    pthread_create(&threadData->synthThread, NULL, synthThreadCB, (void*)sbsmser);
  }

  pthread_create(&threadData->pollThread, NULL, pollThreadCB, (void*)sbsmser);
}

void sbsms_destroy_threads(sbsms *sbsmser)
{
  thread_data *threadData = (thread_data*)sbsmser->threadData;
  threadData->bActive = false;
  if(threadData->bAddThread) {
    pthread_mutex_lock(&threadData->addMutex);
    pthread_cond_broadcast(&threadData->addCond);
    pthread_mutex_unlock(&threadData->addMutex);
    pthread_join(threadData->addThread,NULL);
  }
  if(threadData->bAssignThread) {
    pthread_mutex_lock(&threadData->assignMutex);
    pthread_cond_broadcast(&threadData->assignCond);
    pthread_mutex_unlock(&threadData->assignMutex);
    for(int c=0;c<sbsmser->channels;c++) {
      pthread_join(threadData->assignThread[c],NULL);
    }
  }
  if(threadData->bSynthThread) {
    pthread_mutex_lock(&threadData->synthMutex);
    pthread_cond_broadcast(&threadData->synthCond);
    pthread_mutex_unlock(&threadData->synthMutex);
    pthread_join(threadData->synthThread,NULL);
  }
  pthread_join(threadData->pollThread,NULL);
}
#else
void sbsms_init_threads(sbsms *sbsmser, bool bAnalyze, bool bSynthesize)
{
  sbsmser->threadData = NULL;
}
void sbsms_destroy_threads(sbsms *sbsmser)
{
}
#endif

void sbsms_init(int n) {
  cosinit(n);
}

void sbsms_private_init(sbsms *sbsmser, bool bAnalyze, bool bSynthesize)
{
  sbsmser->bufsize = SBSMS_MAX_FRAME_SIZE[sbsmser->quality];
  sbsmser->chunksize = SBSMS_FRAME_SIZE[sbsmser->quality];
  sbsmser->n_postpad = LONG_MAX;
  sbsmser->ina = make_audio_buf(sbsmser->bufsize);
  sbsms_init_threads(sbsmser,bAnalyze,bSynthesize);
}

void sbsms_reset(sbsms *sbsmser)
{
  sbsmser->ta->init();
  sbsmser->n_processed = 0;
  sbsmser->bWritingComplete = false;
  if(sbsmser->getSamplesCB) {
    sbsmser->n_prepad = SBSMS_N[sbsmser->quality][SBSMS_BANDS[sbsmser->quality]-1]*SBSMS_M_MAX[sbsmser->quality];
    sbsmser->n_prespent = SBSMS_N[sbsmser->quality][SBSMS_BANDS[sbsmser->quality]-1]/SBSMS_H[sbsmser->quality][2]/2;
  } else {
    sbsmser->n_prepad = SBSMS_N[sbsmser->quality][SBSMS_BANDS[sbsmser->quality]-1]/SBSMS_H[sbsmser->quality][2]/2;
    sbsmser->n_prespent = SBSMS_N[sbsmser->quality][SBSMS_BANDS[sbsmser->quality]-1]/SBSMS_H[sbsmser->quality][2]/2;
  }
  sbsmser->top->reset();
}

void sbsms_seek(sbsms *sbsmser, long framePos, long samplePos)
{
  sbsmser->n_processed = samplePos;
  sbsmser->top->seek(framePos);
}

sbsms* sbsms_create(FILE *fp, sbsms_stretch_cb getStretchCB, sbsms_ratio_cb getRatioCB)
{
  sbsms *sbsmser = (sbsms*) calloc(1,sizeof(sbsms));
  // samples, frames
  fseek(fp,0,SEEK_SET);
  long samples, frames;
  fread(&samples,sizeof(long),1,fp);
  fread(&frames,sizeof(long),1,fp);
  fread(&(sbsmser->channels),sizeof(int),1,fp);
  fread(&(sbsmser->quality),sizeof(int),1,fp);
  unsigned short maxtrackindex;
  fread(&maxtrackindex,sizeof(unsigned short),1,fp);
  sbsmser->fp = fp;
  sbsmser->getSamplesCB = NULL;
  sbsmser->getStretchCB = getStretchCB;
  sbsmser->getRatioCB = getRatioCB;
  sbsmser->ta = new TrackAllocator(false,maxtrackindex);
  sbsmser->pa = new PeakAllocator();
  sbsmser->top = new subband(NULL,1,sbsmser->channels,sbsmser->quality,2,false,sbsmser->ta,sbsmser->pa);
  sbsmser->top->setFramesInFile(frames);
  sbsms_private_init(sbsmser,false,false);
  sbsms_reset(sbsmser);
  return sbsmser;
}

sbsms* sbsms_create(sbsms_cb getSamplesCB, sbsms_stretch_cb getStretchCB, sbsms_ratio_cb getRatioCB, int channels, int quality, bool bPreAnalyze, bool bSynthesize)
{
  sbsms *sbsmser = (sbsms*) calloc(1,sizeof(sbsms));
  sbsmser->channels = channels;
  sbsmser->quality = quality;
  sbsmser->getSamplesCB = getSamplesCB;
  sbsmser->getStretchCB = getStretchCB;
  sbsmser->getRatioCB = getRatioCB;
  sbsmser->ta = new TrackAllocator(true);
  sbsmser->pa = new PeakAllocator();
  sbsmser->top = new subband(NULL,1,sbsmser->channels,sbsmser->quality,6,bPreAnalyze,sbsmser->ta,sbsmser->pa);
  sbsms_private_init(sbsmser,true,bSynthesize);
  sbsms_reset(sbsmser);
  return sbsmser;
}

void sbsms_destroy(sbsms* sbsmser)
{
  free_audio_buf((audio*)sbsmser->ina);
  sbsms_destroy_threads(sbsmser);
  if(sbsmser->threadData)
    free(sbsmser->threadData);
  delete sbsmser->top;
  delete sbsmser->ta;
  delete sbsmser->pa;
  free(sbsmser);
}

void sbsms_pre_analyze_complete(sbsms *sbsmser) 
{
  sbsmser->top->preAnalyzeComplete();
}

long sbsms_pre_analyze(sbsms_cb getSamplesCB, void *data, sbsms *sbsmser)
{
  long n_towrite = 0;
  real stretch = (sbsmser->getStretchCB)(sbsmser->n_processed,data);
  real ratio = (sbsmser->getRatioCB)(sbsmser->n_processed,data);
  real a = 1.0/(ratio*stretch);

  if(sbsmser->n_prepad) {
    a = 1.0;
    n_towrite = min(sbsmser->chunksize,sbsmser->n_prepad);
    memset(sbsmser->ina,0,n_towrite*sizeof(audio));
    sbsmser->n_prepad -= n_towrite;
  } else {
    n_towrite = getSamplesCB(sbsmser->ina,sbsmser->chunksize,data);
    sbsmser->n_processed += n_towrite;
    if(n_towrite == 0) {
      if(sbsmser->n_postpad) {
	n_towrite = min(sbsmser->chunksize,sbsmser->n_postpad);
	memset(sbsmser->ina,0,n_towrite*sizeof(audio));
	sbsmser->n_postpad -= n_towrite;
      }
    }
    if(n_towrite==0) {
      sbsmser->bWritingComplete = true;
      sbsmser->top->writingComplete();
    }
  }
  return sbsmser->top->preAnalyze(sbsmser->ina, n_towrite, a, ratio);
}

long sbsms_read_frame(audio *buf, void *data, sbsms *sbsmser, real *ratio0, real *ratio1)
{
  long n_read = 0;
  long n_write = 0;
#ifdef MULTITHREADED
  thread_data *threadData = (thread_data*)sbsmser->threadData;
#endif

  do {
    real stretch = (sbsmser->getStretchCB)(sbsmser->n_processed,data);
    real ratio = (sbsmser->getRatioCB)(sbsmser->n_processed,data);
    real a = 1.0/(ratio*stretch);

    if(sbsmser->n_prespent) {
      n_read = sbsmser->top->read(NULL, ratio0, ratio1);
      if(n_read) sbsmser->n_prespent--;
      n_read = 0;
    } else {
      n_read = sbsmser->top->read(buf, ratio0, ratio1);
    }

    n_write = 0;    
#ifdef MULTITHREADED
    if(threadData->bAddThread && sbsmser->top->isAddReady()) {
      pthread_mutex_lock(&threadData->addMutex);
      pthread_cond_broadcast(&threadData->addCond);
      pthread_mutex_unlock(&threadData->addMutex);
    }
#endif
    if(n_read == 0) {
#ifdef MULTITHREADED
      if(!sbsmser->top->isWriteReady()) {
	pthread_mutex_lock(&threadData->writeMutex);
	pthread_cond_wait(&threadData->writeCond,&threadData->writeMutex);
	pthread_mutex_unlock(&threadData->writeMutex);
      }
#endif
      long n_towrite = 0;
      if(sbsmser->getSamplesCB) {
	if(sbsmser->n_prepad) {
	  a = 1.0;
	  n_towrite = min(sbsmser->chunksize,sbsmser->n_prepad);
	  memset(sbsmser->ina,0,n_towrite*sizeof(audio));
	  sbsmser->n_prepad -= n_towrite;
	} else {
	  n_towrite = (sbsmser->getSamplesCB)(sbsmser->ina,sbsmser->chunksize,data);
	  sbsmser->n_processed += n_towrite;
	  if(n_towrite == 0) {
	    if(sbsmser->n_postpad) {
	      n_towrite = min(sbsmser->chunksize,sbsmser->n_postpad);
	      memset(sbsmser->ina,0,n_towrite*sizeof(audio));
	      sbsmser->n_postpad -= n_towrite;
	    }
	  }
	  if(n_towrite==0) {
	    sbsmser->bWritingComplete = true;
 	    sbsmser->top->writingComplete();
	  }
	}
	n_write = sbsmser->top->write(sbsmser->ina, n_towrite, a, ratio);
      } else {
	if(sbsmser->n_prepad) {
	  n_write = sbsmser->top->zeroPad();
	  sbsmser->n_prepad--;
	} else {
	  n_write = sbsmser->top->readFromFile(sbsmser->fp, a, ratio);
	  sbsmser->n_processed += n_write;
	  if(n_write == 0) {
	    sbsmser->bWritingComplete = true;
	    sbsmser->top->writingComplete();
	  }
	}
      }
    }
#ifdef MULTITHREADED
    if(threadData->bSynthThread && sbsmser->top->isSynthReady()) {
      pthread_mutex_lock(&threadData->synthMutex);
      pthread_cond_broadcast(&threadData->synthCond);
      pthread_mutex_unlock(&threadData->synthMutex);
    }
#endif
    bool bProcess = true;
    bool bSynth = true;
#ifdef MULTITHREADED
    if(threadData->bAddThread && threadData->bAssignThread) bProcess = false;
    if(threadData->bSynthThread) bSynth = false;
#endif
    if(bProcess) sbsmser->top->process();
    if(bSynth) sbsmser->top->synth();
#ifdef MULTITHREADED
    if(threadData->bAssignThread && sbsmser->top->isAssignReady()) {
      pthread_mutex_lock(&threadData->assignMutex);
      pthread_cond_broadcast(&threadData->assignCond);
      pthread_mutex_unlock(&threadData->assignMutex);
    }
#endif
    if(sbsmser->bWritingComplete && 
       !sbsmser->top->isframe_readable() && 
       sbsmser->top->getFramesAtBack() == 0) {
      n_write = sbsmser->top->zeroPad();
    }
  } while(!n_read);
  return n_read;
}

long sbsms_write_frame(FILE *fp, void *data, sbsms *sbsmser)
{
  long n_tofile = 0;
  long n_write = 0;
#ifdef MULTITHREADED
  thread_data *threadData = (thread_data*)sbsmser->threadData;
#endif

  do {
    real stretch = (sbsmser->getStretchCB)(sbsmser->n_processed,data);
    real ratio = (sbsmser->getRatioCB)(sbsmser->n_processed,data);
    real a = 1.0/(ratio*stretch);

    n_tofile = sbsmser->top->getFramesWrittenToFile();

    n_write = 0;
#ifdef MULTITHREADED
    if(threadData->bAddThread && sbsmser->top->isAddReady()) {
      pthread_mutex_lock(&threadData->addMutex);
      pthread_cond_broadcast(&threadData->addCond);
      pthread_mutex_unlock(&threadData->addMutex);
    }
#endif
    if(n_tofile == 0) {
#ifdef MULTITHREADED
      if(!sbsmser->top->isWriteReady()) {
	pthread_mutex_lock(&threadData->writeMutex);
	pthread_cond_wait(&threadData->writeCond,&threadData->writeMutex);
	pthread_mutex_unlock(&threadData->writeMutex);
      }
#endif
      if(sbsmser->top->isWriteReady()) {
	long n_towrite = 0;
	if(sbsmser->n_prepad) {
	  a = 1.0;
	  n_towrite = min(sbsmser->chunksize,sbsmser->n_prepad);
	  memset(sbsmser->ina,0,n_towrite*sizeof(audio));
	  sbsmser->n_prepad -= n_towrite;
	} else {
	  n_towrite = (sbsmser->getSamplesCB)(sbsmser->ina,sbsmser->chunksize,data);
	  sbsmser->n_processed += n_towrite;
	  if(n_towrite == 0) {
	    if(sbsmser->n_postpad) {
	      n_towrite = min(sbsmser->chunksize,sbsmser->n_postpad);
	      memset(sbsmser->ina,0,n_towrite*sizeof(audio));
	      sbsmser->n_postpad -= n_towrite;
	    }
	  }
	  if(!sbsmser->n_postpad)
	    sbsmser->top->writingComplete();
	}
	n_write = sbsmser->top->write(sbsmser->ina, n_towrite, a, ratio);
      }
    }
#ifdef MULTITHREADED
    if(threadData->bAddThread && sbsmser->top->isAddReady()) {
      pthread_mutex_lock(&threadData->addMutex);
      pthread_cond_broadcast(&threadData->addCond);
      pthread_mutex_unlock(&threadData->addMutex);
    }
#endif    
#ifndef MULTITHREADED
    sbsmser->top->process();	
#endif
    long n_written = 0;
    if(sbsmser->n_prespent) {
      n_written = sbsmser->top->writeToFile(NULL);
      if(n_written) sbsmser->n_prespent--;
      n_written = 0;
    } else {
      n_written = sbsmser->top->writeToFile(fp);
    }
#ifdef MULTITHREADED
    if(threadData->bAssignThread && sbsmser->top->isAssignReady()) {
      pthread_mutex_lock(&threadData->assignMutex);
      pthread_cond_broadcast(&threadData->assignCond);
      pthread_mutex_unlock(&threadData->assignMutex);
    }
#endif
  } while(!n_tofile);
  return n_tofile;
}

FILE *sbsms_open_read(const char *fileName)
{
  FILE *fp = fopen(fileName,"rb");
  return fp;
}

void sbsms_close_read(FILE *fp)
{
  fclose(fp);
}

FILE *sbsms_open_write(const char *fileName, sbsms *sbsmser, long samples_to_process)
{
  FILE *fp = fopen(fileName,"wb");
  if(!fp)
    return NULL;
  fwrite(&samples_to_process,sizeof(long),1,fp);
  long nframes = 0;
  fwrite(&nframes,sizeof(long),1,fp);
  fwrite(&(sbsmser->channels),sizeof(int),1,fp);
  fwrite(&(sbsmser->quality),sizeof(int),1,fp);
  unsigned short maxtrackindex = 0;
  fwrite(&maxtrackindex,sizeof(unsigned short),1,fp);
  sbsmser->top->fp = fp;
  return fp;
}

void sbsms_close_write(FILE *fp, sbsms *sbsmser)
{
  sbsmser->top->writeFramePositionsToFile(fp);
  fclose(fp);
}

long sbsms_get_samples_to_process(FILE *fp)
{
  fseek(fp,0,SEEK_SET);
  long samples;
  fread(&samples,sizeof(long),1,fp);
  return samples;
}

long sbsms_get_frames_to_process(FILE *fp)
{
  // samples
  long offset = sizeof(long);
  fseek(fp,offset,SEEK_SET);
  long frames;
  fread(&frames,sizeof(long),1,fp);
  return frames;
}

long sbsms_get_channels(FILE *fp)
{
  //samples, frames
  long offset = 2*sizeof(long);
  fseek(fp,offset,SEEK_SET);
  int channels;
  fread(&channels,sizeof(int),1,fp);
  return channels;
}

long sbsms_get_quality(FILE *fp)
{
  //samples, frames, channels
  long offset = 2*sizeof(long) + sizeof(int);
  fseek(fp,offset,SEEK_SET);
  int quality;
  fread(&quality,sizeof(int),1,fp);
  return quality;
}

void sbsms_seek_start_data(FILE *fp)
{
  // samples, frames, channels, quality, maxtrackindex
  long offset = 2*sizeof(long) + 2*sizeof(int) + sizeof(unsigned short);
  fseek(fp,offset,SEEK_SET);
}

long sbsms_samples_processed(sbsms *sbsmser)
{
  return sbsmser->n_processed;
}

long sbsms_get_samples_queued(sbsms *sbsmser)
{
  return sbsmser->top->getSamplesQueued();
}

long sbsms_get_frames_queued(sbsms *sbsmser)
{
  return sbsmser->top->getFramesQueued();
}

long sbsms_get_last_input_frame_size(sbsms *sbsmser)
{
  return sbsmser->top->getLastInputFrameSize();
}

long sbsms_get_frame_pos(sbsms *sbsmser)
{
  return sbsmser->top->getFramePos();
}

real stretchCBLinear(long nProcessed, void *userData)
{
  sbsmsInfo *si = (sbsmsInfo*) userData;
  real t0 = (real)nProcessed/(real)si->samplesToProcess;
  real stretch = si->stretch0 + (si->stretch1-si->stretch0)*t0;
  return stretch;
}

real stretchCBConstant(long nProcessed, void *userData)
{
  sbsmsInfo *si = (sbsmsInfo*) userData;
  return si->stretch0;
}

real ratioCBLinear(long nProcessed, void *userData)
{
  sbsmsInfo *si = (sbsmsInfo*) userData;
  real t0 = (real)nProcessed/(real)si->samplesToProcess;
  real stretch = si->stretch0 + (si->stretch1-si->stretch0)*t0;
  real t1;
  if(stretch == si->stretch0)
    t1 = 1.0/stretch;
  else
    t1 = log(stretch/si->stretch0)/(stretch-si->stretch0);

  real ratio = si->ratio0 + (si->ratio1-si->ratio0)*t1*(real)nProcessed/(real)si->samplesToGenerate;
  
  return ratio;
}

real ratioCBConstant(long nProcessed, void *userData)
{
  sbsmsInfo *si = (sbsmsInfo*) userData;
  return si->ratio0;
}
