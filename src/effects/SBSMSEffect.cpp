/**********************************************************************

Audacity: A Digital Audio Editor

SBSMSEffect.cpp

Clayton Otey

This abstract class contains all of the common code for an
effect that uses SBSMS to do its processing (TimeScale)

**********************************************************************/

#include "../Audacity.h"

#if USE_SBSMS

#include <math.h>

#include "SBSMSEffect.h"
#include "../WaveTrack.h"
#include "../Project.h"
#include "TimeWarper.h"

class resampleBuf
{
public:
   resampleBuf()
   {
      buf = NULL;
      leftBuffer = NULL;
      rightBuffer = NULL;

      sbsmser = NULL;
      pitch = NULL;
      outBuf = NULL;
      outputLeftBuffer = NULL;
      outputRightBuffer = NULL;
      outputLeftTrack = NULL;
      outputRightTrack = NULL;
      resampler = NULL;
   }

   ~resampleBuf()
   {
      if(buf)                 free(buf);
      if(leftBuffer)          free(leftBuffer);
      if(rightBuffer)         free(rightBuffer);
      if(pitch)               pitch_destroy(pitch);
      if(sbsmser)             sbsms_destroy(sbsmser);
      if(outBuf)              free(outBuf);
      if(outputLeftBuffer)    free(outputLeftBuffer);
      if(outputRightBuffer)   free(outputRightBuffer);
      if(outputLeftTrack)     delete outputLeftTrack;
      if(outputRightTrack)    delete outputRightTrack;
      if(resampler)           delete resampler;
   }

   audio *buf;
   double ratio;
   sampleCount block;
   sampleCount offset;
   sampleCount end;
   float *leftBuffer;
   float *rightBuffer;
   WaveTrack *leftTrack;
   WaveTrack *rightTrack;

   // Not required by callbacks, but makes for easier cleanup
   sbsms *sbsmser;
   pitcher *pitch;
   audio *outBuf;
   float *outputLeftBuffer;
   float *outputRightBuffer;
   WaveTrack *outputLeftTrack;
   WaveTrack *outputRightTrack;
   Resampler *resampler;
};

long samplesCB(audio *chdata, long numFrames, void *userData)
{ 
   sbsmsInfo *si = (sbsmsInfo*) userData;
   long n_read = si->rs->read(chdata, numFrames);
   return n_read;
}

real stretchCB(long nProcessed, void *userData)
{
   sbsmsInfo *si = (sbsmsInfo*) userData;
   real t0 = (real)nProcessed/(real)si->samplesToProcess;
   real stretch = si->stretch0 + (si->stretch1-si->stretch0)*t0;
   return stretch;
}

real ratioCB(long nProcessed, void *userData)
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

void EffectSBSMS :: setParameters(double rateStart, double rateEnd, double pitchStart, double pitchEnd, int quality, bool bPreAnalyze)
{
   this->rateStart = rateStart;
   this->rateEnd = rateEnd;
   this->pitchStart = pitchStart;
   this->pitchEnd = pitchEnd;
   this->quality = quality;
   this->bPreAnalyze = bPreAnalyze;
}

bool EffectSBSMS :: bInit = FALSE;

long resampleCB(void *cb_data, sbsms_resample_frame *data) 
{
   resampleBuf *r = (resampleBuf*) cb_data;
   
   long blockSize = r->leftTrack->GetBestBlockSize(r->offset);
   
   //Adjust the block size if it is the final block in the track
   if (r->offset + blockSize > r->end)
      blockSize = r->end - r->offset;
   
   // Get the samples from the tracks and put them in the buffers.
   r->leftTrack->Get((samplePtr)(r->leftBuffer), floatSample, r->offset, blockSize);
   r->rightTrack->Get((samplePtr)(r->rightBuffer), floatSample, r->offset, blockSize);
   
   // convert to sbsms audio format
   for(int i=0; i<blockSize; i++) {
      r->buf[i][0] = r->leftBuffer[i];
      r->buf[i][1] = r->rightBuffer[i];
   }
   
   r->offset += blockSize;
   data->in = r->buf;
   data->size = blockSize;
   data->ratio0 = r->ratio;
   data->ratio1 = r->ratio;
   
   return blockSize;
}

// Labels inside the affected region are moved to match the audio; labels after
// it are shifted along appropriately.
bool EffectSBSMS::ProcessLabelTrack(Track *t)
{
   TimeWarper *warper = NULL;
   if (rateStart == rateEnd)
   {
      warper = new LinearTimeWarper(mCurT0, mCurT0,
            mCurT1, mCurT0+(mCurT1-mCurT0)*mTotalStretch);
   } else
   {
      warper = new LogarithmicTimeWarper(mCurT0, mCurT1,
            rateStart, rateEnd);
   }
   SetTimeWarper(new RegionTimeWarper(mCurT0, mCurT1, warper));
   LabelTrack *lt = (LabelTrack*)t;
   if (lt == NULL) return false;
   lt->WarpLabels(*GetTimeWarper());
   return true;
}

bool EffectSBSMS::Process()
{
   if(!bInit) {
      sbsms_init(4096);
      bInit = TRUE;
   }
   
   bool bGoodResult = true;
   
   //Iterate over each track
   //Track::All is needed because this effect needs to introduce silence in the group tracks to keep sync
   this->CopyInputTracks(Track::All); // Set up mOutputTracks.
   TrackListIterator iter(mOutputTracks);
   // go to first wavetrack
   Track* t;
   for (t = iter.First(); t->GetKind() != Track::Wave; t = iter.Next());
   if (!t)
      return false;
   mCurTrackNum = 0;

   double maxDuration = 0.0;

   if(rateStart == rateEnd)
      mTotalStretch = 1.0/rateStart;
   else
      mTotalStretch = 1.0/(rateEnd-rateStart)*log(rateEnd/rateStart);


   // we only do a "group change" in the first selected track of the group. 
   // ClearAndPaste has a call to Paste that does changes to the group tracks
   bool first = true;

   while (t != NULL) {
      if (t->GetKind() == Track::Label) {
         first = true;
         if (t->GetSelected() && !ProcessLabelTrack(t))
         {
            bGoodResult = false;
            break;
         }
      }
      else if (t->GetKind() == Track::Wave && t->GetSelected()) {
         WaveTrack* leftTrack = (WaveTrack*)t;

         //Get start and end times from track
         mCurT0 = leftTrack->GetStartTime();
         mCurT1 = leftTrack->GetEndTime();
         
         //Set the current bounds to whichever left marker is
         //greater and whichever right marker is less
         mCurT0 = wxMax(mT0, mCurT0);
         mCurT1 = wxMin(mT1, mCurT1);
         
         // Process only if the right marker is to the right of the left marker
         if (mCurT1 > mCurT0) {
            sampleCount start;
            sampleCount end;
            start = leftTrack->TimeToLongSamples(mCurT0);
            end = leftTrack->TimeToLongSamples(mCurT1);
            
            WaveTrack* rightTrack = NULL;
            if (leftTrack->GetLinked()) {
               double t;
               rightTrack = (WaveTrack*)(iter.Next());
               
               //Adjust bounds by the right tracks markers
               t = rightTrack->GetStartTime();
               t = wxMax(mT0, t);
               mCurT0 = wxMin(mCurT0, t);
               t = rightTrack->GetEndTime();
               t = wxMin(mT1, t);
               mCurT1 = wxMax(mCurT1, t);
               
               //Transform the marker timepoints to samples
               start = leftTrack->TimeToLongSamples(mCurT0);
               end = leftTrack->TimeToLongSamples(mCurT1);
               
               mCurTrackNum++; // Increment for rightTrack, too.	
            }
            
            sampleCount trackEnd = leftTrack->TimeToLongSamples(leftTrack->GetEndTime());

            // SBSMS has a fixed sample rate - we just convert to its sample rate and then convert back
            float srIn = leftTrack->GetRate();
            float srSBSMS = 44100.0;
            
            // the resampler needs a callback to supply its samples
            resampleBuf rb;
            sampleCount maxBlockSize = leftTrack->GetMaxBlockSize();
            rb.block = maxBlockSize;
            rb.buf = (audio*)calloc(rb.block,sizeof(audio));
            rb.leftTrack = leftTrack;
            rb.rightTrack = rightTrack?rightTrack:leftTrack;
            rb.leftBuffer = (float*)calloc(maxBlockSize,sizeof(float));
            rb.rightBuffer = (float*)calloc(maxBlockSize,sizeof(float));
            rb.offset = start;
            rb.end = trackEnd;
            rb.ratio = srSBSMS/srIn;
            rb.resampler = new Resampler(resampleCB, &rb);
            
            // Samples in selection
            sampleCount samplesIn = end-start;
            
            // Samples for SBSMS to process after resampling
            sampleCount samplesToProcess = (sampleCount) ((real)samplesIn*(srSBSMS/srIn));
            
            // Samples in output after resampling back
            sampleCount samplesToGenerate = (sampleCount) ((real)samplesToProcess * mTotalStretch);
            sampleCount samplesOut = (sampleCount) ((real)samplesIn * mTotalStretch);
            double duration =  (mCurT1-mCurT0) * mTotalStretch;

            if(duration > maxDuration)
               maxDuration = duration;

            TimeWarper *warper = NULL;
            if (rateStart == rateEnd)
            {
               warper = new LinearTimeWarper(mCurT0, mCurT0,
                                             mCurT1, mCurT0+maxDuration);
            } else
            {
               warper = new LogarithmicTimeWarper(mCurT0, mCurT1,
                                                  rateStart, rateEnd);
            }
            SetTimeWarper(warper);
            
            sbsmsInfo si;
            si.rs = rb.resampler;
            si.samplesToProcess = samplesToProcess;
            si.samplesToGenerate = samplesToGenerate;
            si.stretch0 = rateStart;
            si.stretch1 = rateEnd;
            si.ratio0 = pitchStart;
            si.ratio1 = pitchEnd;
            
            rb.sbsmser = sbsms_create(&samplesCB,&stretchCB,&ratioCB,rightTrack?2:1,quality,bPreAnalyze,true);
            rb.pitch = pitch_create(rb.sbsmser,&si,srIn/srSBSMS);
            
            rb.outputLeftTrack = mFactory->NewWaveTrack(leftTrack->GetSampleFormat(),
                                                        leftTrack->GetRate());
            if(rightTrack)
               rb.outputRightTrack = mFactory->NewWaveTrack(rightTrack->GetSampleFormat(),
                                                            rightTrack->GetRate());
            
            
            sampleCount blockSize = SBSMS_FRAME_SIZE[quality];
            rb.outBuf = (audio*)calloc(blockSize,sizeof(audio));
            rb.outputLeftBuffer = (float*)calloc(blockSize*2,sizeof(float));
            if(rightTrack)
               rb.outputRightBuffer = (float*)calloc(blockSize*2,sizeof(float));
            
            long pos = 0;
            long outputCount = -1;
            
            // pre analysis
            real fracPre = 0.0f;
            if(bPreAnalyze) {
               fracPre = 0.05f;
               resampleBuf rbPre;
               rbPre.block = maxBlockSize;
               rbPre.buf = (audio*)calloc(rb.block,sizeof(audio));
               rbPre.leftTrack = leftTrack;
               rbPre.rightTrack = rightTrack?rightTrack:leftTrack;
               rbPre.leftBuffer = (float*)calloc(maxBlockSize,sizeof(float));
               rbPre.rightBuffer = (float*)calloc(maxBlockSize,sizeof(float));
               rbPre.offset = start;
               rbPre.end = end;
               rbPre.ratio = srSBSMS/srIn;
               rbPre.resampler = new Resampler(resampleCB, &rbPre);
               si.rs = rbPre.resampler;
               
               long pos = 0;
               long lastPos = 0;
               long ret = 0;
               while(lastPos<samplesToProcess) {
                  ret = sbsms_pre_analyze(&samplesCB,&si,rb.sbsmser);
                  lastPos = pos;
                  pos += ret;
                  real completion = (real)lastPos/(real)samplesToProcess;
                  if (TrackProgress(0,fracPre*completion))
                     return false;
               }
               sbsms_pre_analyze_complete(rb.sbsmser);
               sbsms_reset(rb.sbsmser);
               si.rs = rb.resampler;
            }
            
            // process
            while(pos<samplesOut && outputCount) {
               long frames;
               if(pos+blockSize>samplesOut) {
                  frames = samplesOut - pos;
               } else {
                  frames = blockSize;
               }
               
               outputCount = pitch_process(rb.outBuf, frames, rb.pitch);
               for(int i = 0; i < outputCount; i++) {
                  rb.outputLeftBuffer[i] = rb.outBuf[i][0];
                  if(rightTrack)
                     rb.outputRightBuffer[i] = rb.outBuf[i][1];
               }
               pos += outputCount;
               rb.outputLeftTrack->Append((samplePtr)rb.outputLeftBuffer, floatSample, outputCount);
               if(rightTrack)
                  rb.outputRightTrack->Append((samplePtr)rb.outputRightBuffer, floatSample, outputCount);
               
               double frac = (double)pos/(double)samplesOut;
               int nWhichTrack = mCurTrackNum;
               if(rightTrack) {
                  nWhichTrack = 2*(mCurTrackNum/2);
                  if (frac < 0.5)
                     frac *= 2.0; // Show twice as far for each track, because we're doing 2 at once. 
                  else {
                     nWhichTrack++;
                     frac -= 0.5;
                     frac *= 2.0; // Show twice as far for each track, because we're doing 2 at once. 
                  }
               }
               if (TrackProgress(nWhichTrack, fracPre + (1.0-fracPre)*frac))
                  return false;
            }
            rb.outputLeftTrack->Flush();
            if(rightTrack)
               rb.outputRightTrack->Flush();
            
            leftTrack->ClearAndPaste(mCurT0, mCurT1, rb.outputLeftTrack, true, false, NULL, true, first, GetTimeWarper());

            if(rightTrack) {
               rightTrack->ClearAndPaste(mCurT0, mCurT1, rb.outputRightTrack, true, false, NULL, true, false, GetTimeWarper());
            }

            first = false;
         }
         mCurTrackNum++;
      }
      //Iterate to the next track
      t = iter.Next();
   }
   
   if (bGoodResult)
      ReplaceProcessedTracks(bGoodResult); 

   // Update selection
   mT0 = mCurT0;
   mT1 = mCurT0 + maxDuration;
   
   return bGoodResult;
}

#endif
