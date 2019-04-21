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

// ComponentInterface implementation

ComponentInterfaceSymbol EffectStereoToMono::GetSymbol()
{
   return STEREOTOMONO_PLUGIN_SYMBOL;
}

wxString EffectStereoToMono::GetDescription()
{
   return _("Converts stereo tracks to mono");
}

// EffectDefinitionInterface implementation

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

// Effect implementation

bool EffectStereoToMono::Process()
{
   // Do not use mWaveTracks here.  We will possibly DELETE tracks,
   // so we must use the "real" tracklist.
   this->CopyInputTracks(); // Set up mOutputTracks.
   bool bGoodResult = true;

   auto trackRange = mOutputTracks->SelectedLeaders< WaveTrack >();
   bool refreshIter = false;

   int count = 0;
   while ( trackRange.first != trackRange.second ) {
      mLeftTrack = *trackRange.first;
      auto channels = TrackList::Channels( mLeftTrack );
      if (channels.size() != 2) {
         // TODO: more-than-two-channels
         ++ trackRange.first;
         continue;
      }

      mRightTrack = * channels.rbegin();

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

         // The right channel has been deleted, so we must restart from the beginning
         refreshIter = true;
      }

      if (refreshIter) {
         trackRange = mOutputTracks->SelectedLeaders< WaveTrack >();
         refreshIter = false;
      }
      else
         ++trackRange.first;

      count++;
   }

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
   Floats leftBuffer { idealBlockLen };
   Floats rightBuffer{ idealBlockLen };
   bool bResult = true;

   AudacityProject *p = GetActiveProject();
   auto outTrack =
      p->GetTrackFactory()->NewWaveTrack(floatSample, mLeftTrack->GetRate());

   while (index < mEnd) {
      bResult &= mLeftTrack->Get((samplePtr)leftBuffer.get(), floatSample, index, idealBlockLen);
      bResult &= mRightTrack->Get((samplePtr)rightBuffer.get(), floatSample, index, idealBlockLen);
      auto limit = limitSampleBufferSize( idealBlockLen, mEnd - index );
      for (decltype(limit) i = 0; i < limit; ++i) {
         index++;
         curLeftFrame = leftBuffer[i];
         curRightFrame = rightBuffer[i];
         curMonoFrame = (curLeftFrame + curRightFrame) / 2.0;
         leftBuffer[i] = curMonoFrame;
      }
      outTrack->Append((samplePtr)leftBuffer.get(), floatSample, limit);
      if (TrackProgress(count, 2.*(index.as_double() / (mEnd - mStart).as_double())))
         return false;
   }

   double minStart = wxMin(mLeftTrack->GetStartTime(), mRightTrack->GetStartTime());
   mLeftTrack->Clear(mLeftTrack->GetStartTime(), mLeftTrack->GetEndTime());
   outTrack->Flush();
   mLeftTrack->Paste(minStart, outTrack.get());
   mOutputTracks->GroupChannels( *mLeftTrack,  1 );
   mOutputTracks->Remove(mRightTrack);

   return bResult;
}

bool EffectStereoToMono::IsHidden()
{
   return true;
}

