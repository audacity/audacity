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
   EffectOutputTracks outputs {
      *mTracks,
      GetType(),
      // This effect ignores mT0 and mT1 but always mixes the entire tracks.
      { { mTracks->GetStartTime(), mTracks->GetEndTime() } }
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

   mProgress->SetMessage(XO("Mixing down to mono"));

   for (const auto track : trackRange) {
      if (track->Channels().size() > 1) {
         if (!ProcessOne(outputs.Get(), curTime, totalTime, *track))
            break;
      }
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

   // Always make mono output; don't use EmptyCopy
   auto outTrack = track.EmptyCopy(1);
   auto tempList = TrackList::Temporary(nullptr, outTrack);
   outTrack->ConvertToSampleFormat(floatSample);

   double denominator = track.GetChannelGain(0) + track.GetChannelGain(1);
   while (auto blockLen = mixer.Process()) {
      auto buffer = mixer.GetBuffer();
      for (auto i = 0; i < blockLen; i++)
         ((float *)buffer)[i] /= denominator;

      // If mixing channels that both had only 16 bit effective format
      // (for example), and no gains or envelopes, still there should be
      // dithering because of the averaging above, which may introduce samples
      // lying between the quantization levels.  So use widestSampleFormat.
      outTrack->Append(0,
         buffer, floatSample, blockLen, 1, widestSampleFormat);

      curTime += blockLen;
      if (TotalProgress(curTime.as_double() / totalTime.as_double()))
         return false;
   }
   outTrack->Flush();

   track.Clear(start, end);
   track.MakeMono();
   track.Paste(start, *outTrack);
   RealtimeEffectList::Get(track).Clear();

   return bResult;
}

bool EffectStereoToMono::IsHiddenFromMenus() const
{
   return true;
}

// Attach a menu item
#include "CommonCommandFlags.h"
#include "PluginManager.h"
#include "CommandManager.h"
#include "effects/EffectManager.h"
#include "effects/EffectUI.h"

namespace {
void OnStereoToMono(const CommandContext &context)
{
   EffectUI::DoEffect(
      EffectManager::Get().GetEffectByIdentifier(wxT("StereoToMono")),
      context,
      EffectManager::kConfigured);
}

using namespace MenuRegistry;
auto MenuItem()
{
   static auto item = std::shared_ptr{
   // Delayed evaluation
   // Stereo to Mono is an oddball command that is also subject to control
   // by the plug-in manager, as if an effect.  Decide whether to show or
   // hide it.
   Items( "",
      [](AudacityProject&) -> std::unique_ptr<CommandItem> {
         const PluginID ID =
            EffectManager::Get().GetEffectByIdentifier(wxT("StereoToMono"));
         const PluginDescriptor *plug = PluginManager::Get().GetPlugin(ID);
         if (plug && plug->IsEnabled())
            return Command( wxT("Stereo to Mono"),
               XXO("Mix Stereo Down to &Mono"), OnStereoToMono,
               AudioIONotBusyFlag() | StereoRequiredFlag() |
                  WaveTracksSelectedFlag(), Options{} );
         else
            return {};
      }
   ) };
   return item;
}

AttachedItem sAttachment{
   Indirect(MenuItem()),
   { wxT("Tracks/Mix/Mix"), { OrderingHint::Begin, {} } }
};
}
