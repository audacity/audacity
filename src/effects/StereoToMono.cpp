/**********************************************************************

  Audacity: A Digital Audio Editor

  StereoToMono.cpp

  Lynn Allan

*******************************************************************//**

\class EffectStereoToMono
\brief An Effect to convert stereo to mono.

*//*******************************************************************/


#include "StereoToMono.h"
#include "LoadEffects.h"

#include <wx/intl.h>

#include "../Mix.h"
#include "../Project.h"
#include "../WaveTrack.h"
#include "../widgets/ProgressDialog.h"

const ComponentInterfaceSymbol EffectStereoToMono::Symbol
{ XO("Stereo To Mono") };

namespace{ BuiltinEffectsModule::Registration< EffectStereoToMono > reg; }

EffectStereoToMono::EffectStereoToMono()
{
}

EffectStereoToMono::~EffectStereoToMono()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectStereoToMono::GetSymbol()
{
   return Symbol;
}

TranslatableString EffectStereoToMono::GetDescription()
{
   return XO("Converts stereo tracks to mono");
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

   // Determine the total time (in samples) used by all of the target tracks
   sampleCount totalTime = 0;
   
   auto trackRange = mOutputTracks->SelectedLeaders< WaveTrack >();
   while (trackRange.first != trackRange.second)
   {
      auto left = *trackRange.first;
      auto channels = TrackList::Channels(left);
      if (channels.size() > 1)
      {
         auto right = *channels.rbegin();
         auto leftRate = left->GetRate();
         auto rightRate = right->GetRate();

         if (leftRate != rightRate)
         {
            if (leftRate != mProjectRate)
            {
               mProgress->SetMessage(XO("Resampling left channel"));
               left->Resample(mProjectRate, mProgress);
               leftRate = mProjectRate;
            }
            if (rightRate != mProjectRate)
            {
               mProgress->SetMessage(XO("Resampling right channel"));
               right->Resample(mProjectRate, mProgress);
               rightRate = mProjectRate;
            }
         }
         {
            auto start = wxMin(left->TimeToLongSamples(left->GetStartTime()),
                               right->TimeToLongSamples(right->GetStartTime()));
            auto end = wxMax(left->TimeToLongSamples(left->GetEndTime()),
                               right->TimeToLongSamples(right->GetEndTime()));

            totalTime += (end - start);
         }
      }

      ++trackRange.first;
   }

   // Process each stereo track
   sampleCount curTime = 0;
   bool refreshIter = false;

   mProgress->SetMessage(XO("Mixing down to mono"));

   trackRange = mOutputTracks->SelectedLeaders< WaveTrack >();
   while (trackRange.first != trackRange.second)
   {
      auto left = *trackRange.first;
      auto channels = TrackList::Channels(left);
      if (channels.size() > 1)
      {
         auto right = *channels.rbegin();

         bGoodResult = ProcessOne(curTime, totalTime, left, right);
         if (!bGoodResult)
         {
            break;
         }

         // The right channel has been deleted, so we must restart from the beginning
         refreshIter = true;
      }

      if (refreshIter)
      {
         trackRange = mOutputTracks->SelectedLeaders< WaveTrack >();
         refreshIter = false;
      }
      else
      {
         ++trackRange.first;
      }
   }

   this->ReplaceProcessedTracks(bGoodResult);
   return bGoodResult;
}

bool EffectStereoToMono::ProcessOne(sampleCount & curTime, sampleCount totalTime, WaveTrack *left, WaveTrack *right)
{
   auto idealBlockLen = left->GetMaxBlockSize() * 2;
   bool bResult = true;
   sampleCount processed = 0;

   auto start = wxMin(left->GetStartTime(), right->GetStartTime());
   auto end = wxMax(left->GetEndTime(), right->GetEndTime());

   WaveTrackConstArray tracks;
   tracks.push_back(left->SharedPointer< const WaveTrack >());
   tracks.push_back(right->SharedPointer< const WaveTrack >());

   Mixer mixer(tracks,
               true,                // Throw to abort mix-and-render if read fails:
               Mixer::WarpOptions{*inputTracks()},
               start,
               end,
               1,
               idealBlockLen,
               false,               // Not interleaved
               left->GetRate(),     // Process() checks that left and right
                                    // rates are the same
               floatSample);

   auto outTrack = left->EmptyCopy();
   outTrack->ConvertToSampleFormat(floatSample);

   while (auto blockLen = mixer.Process(idealBlockLen))
   {
      auto buffer = mixer.GetBuffer();
      for (auto i = 0; i < blockLen; i++)
      {
         ((float *)buffer)[i] /= 2.0;
      }
      outTrack->Append(buffer, floatSample, blockLen);

      curTime += blockLen;
      if (TotalProgress(curTime.as_double() / totalTime.as_double()))
      {
         return false;
      }
   }
   outTrack->Flush();

   double minStart = wxMin(left->GetStartTime(), right->GetStartTime());
   left->Clear(left->GetStartTime(), left->GetEndTime());
   left->Paste(minStart, outTrack.get());
   mOutputTracks->UnlinkChannels(*left);
   mOutputTracks->Remove(right);

   return bResult;
}

bool EffectStereoToMono::IsHidden()
{
   return true;
}
