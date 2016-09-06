/**********************************************************************

Audacity: A Digital Audio Editor

SBSMSEffect.cpp

Clayton Otey

This class contains all of the common code for an
effect that uses SBSMS to do its processing (TimeScale)

**********************************************************************/

#include "../Audacity.h"

#if USE_SBSMS

#include <math.h>

#include "SBSMSEffect.h"
#include "../LabelTrack.h"
#include "../WaveTrack.h"
#include "../Project.h"
#include "TimeWarper.h"

enum {
  SBSMSOutBlockSize = 512
};

class ResampleBuf
{
public:
   ResampleBuf()
   {
      processed = 0;
      buf = NULL;
      leftBuffer = NULL;
      rightBuffer = NULL;

      SBSMSBuf = NULL;
      outputLeftTrack = NULL;
      outputRightTrack = NULL;
   }

   ~ResampleBuf()
   {
      if(buf)                 free(buf);
      if(leftBuffer)          free(leftBuffer);
      if(rightBuffer)         free(rightBuffer);
      if(SBSMSBuf)            free(SBSMSBuf);
   }

   bool bPitch;
   audio *buf;
   double ratio;
   sampleCount processed;
   size_t blockSize;
   long SBSMSBlockSize;
   sampleCount offset;
   sampleCount end;
   float *leftBuffer;
   float *rightBuffer;
   WaveTrack *leftTrack;
   WaveTrack *rightTrack;
   std::unique_ptr<SBSMS> sbsms;
   std::unique_ptr<SBSMSInterface> iface;
   audio *SBSMSBuf;

   // Not required by callbacks, but makes for easier cleanup
   std::unique_ptr<Resampler> resampler;
   std::unique_ptr<SBSMSQuality> quality;
   std::unique_ptr<WaveTrack> outputLeftTrack;
   std::unique_ptr<WaveTrack> outputRightTrack;
};

class SBSMSEffectInterface final : public SBSMSInterfaceSliding {
public:
   SBSMSEffectInterface(Resampler *resampler,
                        Slide *rateSlide, Slide *pitchSlide,
                        bool bReferenceInput,
                        long samples, long preSamples,
                        SBSMSQuality *quality)
      : SBSMSInterfaceSliding(rateSlide,pitchSlide,bReferenceInput,samples,preSamples,quality)
   {
      this->resampler = resampler;
   }
   virtual ~SBSMSEffectInterface() {}

   long samples(audio *buf, long n) {
      return resampler->read(buf, n);
   }

protected:
   Resampler *resampler;
};

long resampleCB(void *cb_data, SBSMSFrame *data)
{
   ResampleBuf *r = (ResampleBuf*) cb_data;

   auto blockSize = limitSampleBufferSize(
      r->leftTrack->GetBestBlockSize(r->offset),
      r->end - r->offset
   );

   // Get the samples from the tracks and put them in the buffers.
   r->leftTrack->Get((samplePtr)(r->leftBuffer), floatSample, r->offset, blockSize);
   r->rightTrack->Get((samplePtr)(r->rightBuffer), floatSample, r->offset, blockSize);

   // convert to sbsms audio format
   for(decltype(blockSize) i=0; i<blockSize; i++) {
      r->buf[i][0] = r->leftBuffer[i];
      r->buf[i][1] = r->rightBuffer[i];
   }

   data->buf = r->buf;
   data->size = blockSize;
   if(r->bPitch) {
     float t0 = r->processed.as_float() / r->iface->getSamplesToInput();
     float t1 = (r->processed + blockSize).as_float() / r->iface->getSamplesToInput();
     data->ratio0 = r->iface->getStretch(t0);
     data->ratio1 = r->iface->getStretch(t1);
   } else {
     data->ratio0 = r->ratio;
     data->ratio1 = r->ratio;
   }
   r->processed += blockSize;
   r->offset += blockSize;
   return blockSize;
}

long postResampleCB(void *cb_data, SBSMSFrame *data)
{
   ResampleBuf *r = (ResampleBuf*) cb_data;
   auto count = r->sbsms->read(r->iface.get(), r->SBSMSBuf, r->SBSMSBlockSize);
   data->buf = r->SBSMSBuf;
   data->size = count;
   data->ratio0 = 1.0 / r->ratio;
   data->ratio1 = 1.0 / r->ratio;
   return count;
}

void EffectSBSMS :: setParameters(double rateStart, double rateEnd, double pitchStart, double pitchEnd,
                                  SlideType rateSlideType, SlideType pitchSlideType,
                                  bool bLinkRatePitch, bool bRateReferenceInput, bool bPitchReferenceInput)
{
   this->rateStart = rateStart;
   this->rateEnd = rateEnd;
   this->pitchStart = pitchStart;
   this->pitchEnd = pitchEnd;
   this->bLinkRatePitch = bLinkRatePitch;
   this->rateSlideType = rateSlideType;
   this->pitchSlideType = pitchSlideType;
   this->bRateReferenceInput = bRateReferenceInput;
   this->bPitchReferenceInput = bPitchReferenceInput;
}

void EffectSBSMS::setParameters(double tempoRatio, double pitchRatio)
{
   setParameters(tempoRatio, tempoRatio, pitchRatio, pitchRatio,
                 SlideConstant, SlideConstant, false, false, false);
}

std::unique_ptr<TimeWarper> createTimeWarper(double t0, double t1, double duration,
                             double rateStart, double rateEnd, SlideType rateSlideType)
{
   std::unique_ptr<TimeWarper> warper;
   if (rateStart == rateEnd || rateSlideType == SlideConstant) {
      warper = std::make_unique<LinearTimeWarper>(t0, t0, t1, t0+duration);
   } else if(rateSlideType == SlideLinearInputRate) {
      warper = std::make_unique<LinearInputRateTimeWarper>(t0, t1, rateStart, rateEnd);
   } else if(rateSlideType == SlideLinearOutputRate) {
      warper = std::make_unique<LinearOutputRateTimeWarper>(t0, t1, rateStart, rateEnd);
   } else if(rateSlideType == SlideLinearInputStretch) {
      warper = std::make_unique<LinearInputStretchTimeWarper>(t0, t1, rateStart, rateEnd);
   } else if(rateSlideType == SlideLinearOutputStretch) {
      warper = std::make_unique<LinearOutputStretchTimeWarper>(t0, t1, rateStart, rateEnd);
   } else if(rateSlideType == SlideGeometricInput) {
      warper = std::make_unique<GeometricInputTimeWarper>(t0, t1, rateStart, rateEnd);
   } else if(rateSlideType == SlideGeometricOutput) {
      warper = std::make_unique<GeometricOutputTimeWarper>(t0, t1, rateStart, rateEnd);
   }
   return warper;
}

// Labels inside the affected region are moved to match the audio; labels after
// it are shifted along appropriately.
bool EffectSBSMS::ProcessLabelTrack(LabelTrack *lt)
{
   auto warper = createTimeWarper(mT0,mT1,(mT1-mT0)*mTotalStretch,rateStart,rateEnd,rateSlideType);
   SetTimeWarper(std::make_unique<RegionTimeWarper>(mT0, mT1, std::move(warper)));
   lt->WarpLabels(*GetTimeWarper());
   return true;
}

double EffectSBSMS::getInvertedStretchedTime(double rateStart, double rateEnd, SlideType slideType, double outputTime)
{
   Slide slide(slideType,rateStart,rateEnd,0);
   return slide.getInverseStretchedTime(outputTime);
}

double EffectSBSMS::getRate(double rateStart, double rateEnd, SlideType slideType, double t)
{
   Slide slide(slideType,rateStart,rateEnd,0);
   return slide.getRate(t);
}

bool EffectSBSMS::Process()
{
   bool bGoodResult = true;

   //Iterate over each track
   //Track::All is needed because this effect needs to introduce silence in the group tracks to keep sync
   this->CopyInputTracks(Track::All); // Set up mOutputTracks.
   TrackListIterator iter(mOutputTracks.get());
   Track* t;
   mCurTrackNum = 0;

   double maxDuration = 0.0;

   // Must sync if selection length will change
   bool mustSync = (rateStart != rateEnd);
   Slide rateSlide(rateSlideType,rateStart,rateEnd);
   Slide pitchSlide(pitchSlideType,pitchStart,pitchEnd);
   mTotalStretch = rateSlide.getTotalStretch();

   t = iter.First();
   while (t != NULL) {
      if (t->GetKind() == Track::Label &&
            (t->GetSelected() || (mustSync && t->IsSyncLockSelected())) )
      {
         if (!ProcessLabelTrack(static_cast<LabelTrack*>(t))) {
            bGoodResult = false;
            break;
         }
      }
      else if (t->GetKind() == Track::Wave && t->GetSelected() )
      {
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
            auto start = leftTrack->TimeToLongSamples(mCurT0);
            auto end = leftTrack->TimeToLongSamples(mCurT1);

            WaveTrack* rightTrack = NULL;
            if (leftTrack->GetLinked()) {
               double t;
               // Assume linked track is wave or null
               rightTrack = static_cast<WaveTrack*>(iter.Next());

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
            const auto trackStart =
               leftTrack->TimeToLongSamples(leftTrack->GetStartTime());
            const auto trackEnd =
               leftTrack->TimeToLongSamples(leftTrack->GetEndTime());

            // SBSMS has a fixed sample rate - we just convert to its sample rate and then convert back
            float srTrack = leftTrack->GetRate();
            float srProcess = bLinkRatePitch ? srTrack : 44100.0;

            // the resampler needs a callback to supply its samples
            ResampleBuf rb;
            auto maxBlockSize = leftTrack->GetMaxBlockSize();
            rb.blockSize = maxBlockSize;
            rb.buf = (audio*)calloc(rb.blockSize,sizeof(audio));
            rb.leftTrack = leftTrack;
            rb.rightTrack = rightTrack?rightTrack:leftTrack;
            rb.leftBuffer = (float*)calloc(maxBlockSize,sizeof(float));
            rb.rightBuffer = (float*)calloc(maxBlockSize,sizeof(float));

            // Samples in selection
            auto samplesIn = end - start;

            // Samples for SBSMS to process after resampling
            auto samplesToProcess = (sampleCount) (samplesIn.as_float() * (srProcess/srTrack));

            SlideType outSlideType;
            SBSMSResampleCB outResampleCB;

            if(bLinkRatePitch) {
              rb.bPitch = true;
              outSlideType = rateSlideType;
              outResampleCB = resampleCB;
              rb.offset = start;
              rb.end = end;
               // Third party library has its own type alias, check it
               static_assert(sizeof(sampleCount::type) <=
                             sizeof(_sbsms_::SampleCountType),
                             "Type _sbsms_::SampleCountType is too narrow to hold a sampleCount");
              rb.iface = std::make_unique<SBSMSInterfaceSliding>
                  (&rateSlide, &pitchSlide, bPitchReferenceInput,
                   static_cast<_sbsms_::SampleCountType>
                      ( samplesToProcess.as_long_long() ),
                   0, nullptr);
               
            } else {
              rb.bPitch = false;
              outSlideType = (srProcess==srTrack?SlideIdentity:SlideConstant);
              outResampleCB = postResampleCB;
              rb.ratio = srProcess/srTrack;
              rb.quality = std::make_unique<SBSMSQuality>(&SBSMSQualityStandard);
              rb.resampler = std::make_unique<Resampler>(resampleCB, &rb, srProcess==srTrack?SlideIdentity:SlideConstant);
              rb.sbsms = std::make_unique<SBSMS>(rightTrack ? 2 : 1, rb.quality.get(), true);
              rb.SBSMSBlockSize = rb.sbsms->getInputFrameSize();
              rb.SBSMSBuf = (audio*)calloc(rb.SBSMSBlockSize,sizeof(audio));

              // Note: width of getMaxPresamples() is only long.  Widen it
              decltype(start) processPresamples = rb.quality->getMaxPresamples();
              processPresamples =
                 std::min(processPresamples,
                          decltype(processPresamples)
                             (( start - trackStart ).as_float() *
                                 (srProcess/srTrack)));
              auto trackPresamples = start - trackStart;
              trackPresamples =
                  std::min(trackPresamples,
                           decltype(trackPresamples)
                              (processPresamples.as_float() *
                                  (srTrack/srProcess)));
              rb.offset = start - trackPresamples;
              rb.end = trackEnd;
              rb.iface = std::make_unique<SBSMSEffectInterface>
                  (rb.resampler.get(), &rateSlide, &pitchSlide,
                   bPitchReferenceInput,
                   // UNSAFE_SAMPLE_COUNT_TRUNCATION
                   // The argument type is only long!
                   static_cast<long> ( samplesToProcess.as_long_long() ),
                   // This argument type is also only long!
                   static_cast<long> ( processPresamples.as_long_long() ),
                   rb.quality.get());
            }
            
            Resampler resampler(outResampleCB,&rb,outSlideType);

            audio outBuf[SBSMSOutBlockSize];
            float outBufLeft[2*SBSMSOutBlockSize];
            float outBufRight[2*SBSMSOutBlockSize];

            // Samples in output after SBSMS
            sampleCount samplesToOutput = rb.iface->getSamplesToOutput();

            // Samples in output after resampling back
            auto samplesOut = (sampleCount) (samplesToOutput.as_float() * (srTrack/srProcess));

            // Duration in track time
            double duration =  (mCurT1-mCurT0) * mTotalStretch;

            if(duration > maxDuration)
               maxDuration = duration;

            auto warper = createTimeWarper(mCurT0,mCurT1,maxDuration,rateStart,rateEnd,rateSlideType);
            SetTimeWarper(std::move(warper));

            rb.outputLeftTrack = mFactory->NewWaveTrack(leftTrack->GetSampleFormat(),
                                                        leftTrack->GetRate());
            if(rightTrack)
               rb.outputRightTrack = mFactory->NewWaveTrack(rightTrack->GetSampleFormat(),
                                                            rightTrack->GetRate());
            long pos = 0;
            long outputCount = -1;

            // process
            while(pos<samplesOut && outputCount) {
               const auto frames =
                  limitSampleBufferSize( SBSMSOutBlockSize, samplesOut - pos );

               outputCount = resampler.read(outBuf,frames);
               for(int i = 0; i < outputCount; i++) {
                  outBufLeft[i] = outBuf[i][0];
                  if(rightTrack)
                     outBufRight[i] = outBuf[i][1];
               }
               pos += outputCount;
               rb.outputLeftTrack->Append((samplePtr)outBufLeft, floatSample, outputCount);
               if(rightTrack)
                  rb.outputRightTrack->Append((samplePtr)outBufRight, floatSample, outputCount);

               double frac = (double)pos / samplesOut.as_double();
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
               if (TrackProgress(nWhichTrack, frac))
                  return false;
            }
            rb.outputLeftTrack->Flush();
            if(rightTrack)
               rb.outputRightTrack->Flush();

            bool bResult =
               leftTrack->ClearAndPaste(mCurT0, mCurT1, rb.outputLeftTrack.get(),
                                          true, false, GetTimeWarper());
            wxASSERT(bResult); // TO DO: Actually handle this.
            wxUnusedVar(bResult);

            if(rightTrack)
            {
               bResult =
                  rightTrack->ClearAndPaste(mCurT0, mCurT1, rb.outputRightTrack.get(),
                                             true, false, GetTimeWarper());
               wxASSERT(bResult); // TO DO: Actually handle this.
            }
         }
         mCurTrackNum++;
      }
      else if (mustSync && t->IsSyncLockSelected())
      {
         t->SyncLockAdjust(mCurT1, mCurT0 + (mCurT1 - mCurT0) * mTotalStretch);
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
