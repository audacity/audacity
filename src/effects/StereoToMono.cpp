/**********************************************************************

  Audacity: A Digital Audio Editor

  StereoToMono.cpp

  Lynn Allan

*******************************************************************//**

\class EffectStereoToMono
\brief An Effect to convert stereo to mono.

*//*******************************************************************/


#include "StereoToMono.h"
#include "EffectOutputTracks.h"
#include "LoadEffects.h"

#include "Mix.h"
#include "MixAndRender.h"
#include "Project.h"
#include "RealtimeEffectList.h"
#include "WaveTrack.h"
#include "ProgressDialog.h"

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

bool EffectStereoToMono::Process(EffectInstance &, EffectSettings &)
{
   // Do not use mWaveTracks here.  We will possibly DELETE tracks,
   // so we must use the "real" tracklist.
   EffectOutputTracks outputs { *mTracks,
      // This effect ignores mT0 and mT1 but always mixes the entire tracks.
      {{ mTracks->GetStartTime(), mTracks->GetEndTime() }}
   };
   bool bGoodResult = true;

   // Determine the total time (in samples) used by all of the target tracks
   // only for progress dialog
   sampleCount totalTime = 0;

   auto trackRange = outputs.Get().Selected<WaveTrack>();
   for (const auto left : trackRange) {
      if (left->Channels().size() > 1) {
         auto start = left->TimeToLongSamples(left->GetStartTime());
         auto end = left->TimeToLongSamples(left->GetEndTime());
         totalTime += (end - start);
      }
   }

   // Process each stereo track
   sampleCount curTime = 0;
   bool refreshIter = false;

   mProgress->SetMessage(XO("Mixing down to mono"));

   // Don't use range-for, because iterators may be invalidated by erasure from
   // the track list
   while (trackRange.first != trackRange.second) {
      auto track = *trackRange.first;
      if (track->Channels().size() > 1) {
         if (!ProcessOne(outputs.Get(), curTime, totalTime, *track))
            break;
         // The right channel has been deleted, so we must restart from the beginning
         refreshIter = true;
      }

      if (refreshIter) {
         trackRange = outputs.Get().Selected<WaveTrack>();
         refreshIter = false;
      }
      else
         ++trackRange.first;
   }

   if (bGoodResult)
      outputs.Commit();

   return bGoodResult;
}

bool EffectStereoToMono::ProcessOne(TrackList &outputs,
   sampleCount & curTime, sampleCount totalTime, WaveTrack &track)
{
   auto idealBlockLen = track.GetMaxBlockSize() * 2;
   bool bResult = true;
   sampleCount processed = 0;

   const auto start = track.GetStartTime();
   const auto end = track.GetEndTime();

   Mixer::Inputs tracks;
   tracks.emplace_back(
      track.SharedPointer<const SampleTrack>(), GetEffectStages(track));

   Mixer mixer(move(tracks),
      true,                // Throw to abort mix-and-render if read fails:
      Mixer::WarpOptions{ inputTracks()->GetOwner() },
      start,
      end,
      1,
      idealBlockLen,
      false,               // Not interleaved
      track.GetRate(),
      floatSample);

   // Always make mono output; don't use WideEmptyCopy
   auto outTrack = track.EmptyCopy();
   auto tempList = TrackList::Temporary(nullptr, outTrack, nullptr);
   assert(outTrack->IsLeader());
   outTrack->ConvertToSampleFormat(floatSample);

   while (auto blockLen = mixer.Process()) {
      auto buffer = mixer.GetBuffer();
      for (auto i = 0; i < blockLen; i++)
         ((float *)buffer)[i] /= 2.0;

      // If mixing channels that both had only 16 bit effective format
      // (for example), and no gains or envelopes, still there should be
      // dithering because of the averaging above, which may introduce samples
      // lying between the quantization levels.  So default the effectiveFormat
      // to widest.
      outTrack->Append(buffer, floatSample, blockLen, 1);

      curTime += blockLen;
      if (TotalProgress(curTime.as_double() / totalTime.as_double()))
         return false;
   }
   outTrack->Flush();

   outputs.UnlinkChannels(track);
   const auto right = * ++track.GetOwner()->Find(&track);
   // Should be a consequence of unlinking:
   assert(right->IsLeader());
   outputs.Remove(*right);

   track.Clear(start, end);
   track.Paste(start, *outTrack);
   RealtimeEffectList::Get(track).Clear();

   return bResult;
}

bool EffectStereoToMono::IsHiddenFromMenus() const
{
   return true;
}
