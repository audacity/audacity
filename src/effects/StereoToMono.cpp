/**********************************************************************

  Audacity: A Digital Audio Editor

  StereoToMono.cpp

  Lynn Allan

*******************************************************************//**

\class EffectStereoToMono
\brief An Effect to convert stereo to mono.

*//*******************************************************************/

#include "../Audacity.h"
#include "StereoToMono.h"

#include <wx/intl.h>

#include "../Project.h"
#include "../WaveTrack.h"

EffectStereoToMono::EffectStereoToMono()
{
}

EffectStereoToMono::~EffectStereoToMono()
{
}

// IdentInterface implementation

wxString EffectStereoToMono::GetSymbol()
{
   return STEREOTOMONO_PLUGIN_SYMBOL;
}

wxString EffectStereoToMono::GetDescription()
{
   return XO("Converts stereo tracks to mono");
}

// EffectIdentInterface implementation

EffectType EffectStereoToMono::GetType()
{
   // Really EffectTypeProcess, but this prevents it from showing in the Effect Menu
   return EffectTypeHidden;
}

bool EffectStereoToMono::IsInteractive()
{
   return false;
}

// EffectClientInterface implementation

unsigned EffectStereoToMono::GetAudioInCount()
{
   return 2;
}

unsigned EffectStereoToMono::GetAudioOutCount()
{
   return 1;
}

// Effect implementatino

bool EffectStereoToMono::Process()
{
   // Do not use mWaveTracks here.  We will possibly DELETE tracks,
   // so we must use the "real" tracklist.
   this->CopyInputTracks(); // Set up mOutputTracks.
   bool bGoodResult = true;

   SelectedTrackListOfKindIterator iter(Track::Wave, mOutputTracks.get());
   mLeftTrack = (WaveTrack *)iter.First();
   bool refreshIter = false;

   if(mLeftTrack)
   {
      // create a NEW WaveTrack to hold all of the output
      AudacityProject *p = GetActiveProject();
      mOutTrack = p->GetTrackFactory()->NewWaveTrack(floatSample, mLeftTrack->GetRate());
   }

   int count = 0;
   while (mLeftTrack) {
      if (mLeftTrack->GetKind() == Track::Wave &&
         mLeftTrack->GetSelected() &&
         mLeftTrack->GetLinked()) {

         // Assume linked track is wave
         mRightTrack = static_cast<WaveTrack*>(iter.Next());

         if ((mLeftTrack->GetRate() == mRightTrack->GetRate())) {
            auto leftTrackStart = mLeftTrack->TimeToLongSamples(mLeftTrack->GetStartTime());
            auto rightTrackStart = mRightTrack->TimeToLongSamples(mRightTrack->GetStartTime());
            mStart = wxMin(leftTrackStart, rightTrackStart);

            auto leftTrackEnd = mLeftTrack->TimeToLongSamples(mLeftTrack->GetEndTime());
            auto rightTrackEnd = mRightTrack->TimeToLongSamples(mRightTrack->GetEndTime());
            mEnd = wxMax(leftTrackEnd, rightTrackEnd);

            bGoodResult = ProcessOne(count);
            if (!bGoodResult)
               break;

            mOutTrack->Clear(mOutTrack->GetStartTime(), mOutTrack->GetEndTime());

            // The right channel has been deleted, so we must restart from the beginning
            refreshIter = true;
         }
      }

      if (refreshIter) {
         mLeftTrack = (WaveTrack *)iter.First();
         refreshIter = false;
      }
      else {
         mLeftTrack = (WaveTrack *)iter.Next();
      }
      count++;
   }

   mOutTrack.reset();
   this->ReplaceProcessedTracks(bGoodResult);
   return bGoodResult;
}

bool EffectStereoToMono::ProcessOne(int count)
{
   float  curLeftFrame;
   float  curRightFrame;
   float  curMonoFrame;

   auto idealBlockLen = mLeftTrack->GetMaxBlockSize() * 2;
   auto index = mStart;
   float *leftBuffer = new float[idealBlockLen];
   float *rightBuffer = new float[idealBlockLen];
   bool bResult = true;

   while (index < mEnd) {
      bResult &= mLeftTrack->Get((samplePtr)leftBuffer, floatSample, index, idealBlockLen);
      bResult &= mRightTrack->Get((samplePtr)rightBuffer, floatSample, index, idealBlockLen);
      auto limit = limitSampleBufferSize( idealBlockLen, mEnd - index );
      for (decltype(limit) i = 0; i < limit; ++i) {
         index++;
         curLeftFrame = leftBuffer[i];
         curRightFrame = rightBuffer[i];
         curMonoFrame = (curLeftFrame + curRightFrame) / 2.0;
         leftBuffer[i] = curMonoFrame;
      }
      bResult &= mOutTrack->Append((samplePtr)leftBuffer, floatSample, limit);
      if (TrackProgress(count, 2.*(index.as_double() / (mEnd - mStart).as_double())))
         return false;
   }

   double minStart = wxMin(mLeftTrack->GetStartTime(), mRightTrack->GetStartTime());
   bResult &= mLeftTrack->Clear(mLeftTrack->GetStartTime(), mLeftTrack->GetEndTime());
   bResult &= mOutTrack->Flush();
   bResult &= mLeftTrack->Paste(minStart, mOutTrack.get());
   mLeftTrack->SetLinked(false);
   mRightTrack->SetLinked(false);
   mLeftTrack->SetChannel(Track::MonoChannel);
   mOutputTracks->Remove(mRightTrack);

   delete [] leftBuffer;
   delete [] rightBuffer;

   return bResult;
}

bool EffectStereoToMono::IsHidden()
{
   return true;
}

