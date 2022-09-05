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

#include "Mix.h"
#include "MixAndRender.h"
#include "Project.h"
#include "RealtimeEffectList.h"
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

ComponentInterfaceSymbol EffectStereoToMono::GetSymbol() const
{
   return Symbol;
}

TranslatableString EffectStereoToMono::GetDescription() const
{
   return XO("Converts stereo tracks to mono");
}

// EffectDefinitionInterface implementation

EffectType EffectStereoToMono::GetType() const
{
   // Really EffectTypeProcess, but this prevents it from showing in the Effect Menu
   return EffectTypeHidden;
}

bool EffectStereoToMono::IsInteractive() const
{
   return false;
}

unsigned EffectStereoToMono::GetAudioInCount() const
{
   return 2;
}

unsigned EffectStereoToMono::GetAudioOutCount() const
{
   return 1;
}

// Effect implementation

bool EffectStereoToMono::Process(EffectContext &context,
   EffectInstance &, EffectSettings &)
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
               if (context.pProgress)
                  context.pProgress->SetMessage(XO("Resampling left channel"));
               left->Resample(mProjectRate, context.pProgress);
               leftRate = mProjectRate;
            }
            if (rightRate != mProjectRate)
            {
               if (context.pProgress)
                  context.pProgress->SetMessage(XO("Resampling right channel"));
               right->Resample(mProjectRate, context.pProgress);
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

   if (context.pProgress)
      context.pProgress->SetMessage(XO("Mixing down to mono"));

   trackRange = mOutputTracks->SelectedLeaders< WaveTrack >();
   while (trackRange.first != trackRange.second)
   {
      auto left = *trackRange.first;
      auto channels = TrackList::Channels(left);
      if (channels.size() > 1)
      {
         auto right = *channels.rbegin();

         bGoodResult = ProcessOne(context, curTime, totalTime, left, right);
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

bool EffectStereoToMono::ProcessOne(EffectContext &context,
   sampleCount & curTime, sampleCount totalTime,
   WaveTrack *left, WaveTrack *right)
{
   auto idealBlockLen = left->GetMaxBlockSize() * 2;
   bool bResult = true;
   sampleCount processed = 0;

   auto start = wxMin(left->GetStartTime(), right->GetStartTime());
   auto end = wxMax(left->GetEndTime(), right->GetEndTime());

   Mixer::Inputs tracks;
   for (auto pTrack : { left, right })
      tracks.emplace_back(
         pTrack->SharedPointer<const SampleTrack>(), GetEffectStages(*pTrack));

   Mixer mixer(move(tracks),
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

   while (auto blockLen = mixer.Process()) {
      auto buffer = mixer.GetBuffer();
      for (auto i = 0; i < blockLen; i++)
      {
         ((float *)buffer)[i] /= 2.0;
      }
      // If mixing channels that both had only 16 bit effective format
      // (for example), and no gains or envelopes, still there should be
      // dithering because of the averaging above, which may introduce samples
      // lying between the quantization levels.  So default the effectiveFormat
      // to widest.
      outTrack->Append(buffer, floatSample, blockLen, 1);

      curTime += blockLen;
      if (context.TotalProgress(
         curTime.as_double() / totalTime.as_double()))
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
   RealtimeEffectList::Get(*left).Clear();

   return bResult;
}

bool EffectStereoToMono::IsHiddenFromMenus() const
{
   return true;
}
