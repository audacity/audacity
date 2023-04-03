/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportUtils.cpp

  Dominic Mazzoni
 
  Vitaly Sverchinsky split from ExportPlugin.h

**********************************************************************/

#include "ExportUtils.h"
#include "Track.h"
#include "Mix.h"
#include "BasicUI.h"
#include "WaveTrack.h"
#include "MixAndRender.h"
#include "wxFileNameWrapper.h"

//Create a mixer by computing the time warp factor
std::unique_ptr<Mixer> ExportUtils::CreateMixer(const TrackList &tracks,
         bool selectionOnly,
         double startTime, double stopTime,
         unsigned numOutChannels, size_t outBufferSize, bool outInterleaved,
         double outRate, sampleFormat outFormat,
         MixerOptions::Downmix *mixerSpec)
{
   Mixer::Inputs inputs;

   for (auto pTrack: FindExportWaveTracks(tracks, selectionOnly))
      inputs.emplace_back(
         pTrack->SharedPointer<const SampleTrack>(), GetEffectStages(*pTrack));
   // MB: the stop time should not be warped, this was a bug.
   return std::make_unique<Mixer>(move(inputs),
                  // Throw, to stop exporting, if read fails:
                  true,
                  Mixer::WarpOptions{tracks},
                  startTime, stopTime,
                  numOutChannels, outBufferSize, outInterleaved,
                  outRate, outFormat,
                  true, mixerSpec);
}

double ExportUtils::EvalExportProgress(Mixer &mixer, double t0, double t1)
{
   const auto duration = t1 - t0;
   if(duration > 0)
      return std::clamp(mixer.MixGetCurrentTime() - t0, .0, duration) / duration;
   return .0;
}

//TODO: used in many places in anticipation that Exporter yields same result, fix that
TrackIterRange<const WaveTrack> ExportUtils::FindExportWaveTracks(const TrackList& tracks, bool selectedOnly)
{
   bool anySolo = !(( tracks.Any<const WaveTrack>() + &WaveTrack::GetSolo ).empty());

   return tracks.Any<const WaveTrack>()
            + ( selectedOnly ? &Track::IsSelected : &Track::Any  )
            - ( anySolo ? &WaveTrack::GetNotSolo :  &WaveTrack::GetMute);
}

ExportPlugin::Parameters ExportUtils::ParametersFromEditor(const ExportOptionsEditor& editor)
{
   ExportPlugin::Parameters parameters;
   for(int i = 0, count = editor.GetOptionsCount(); i < count; ++i)
   {
      ExportOption option;
      ExportValue value;
      if(editor.GetOption(i, option) && editor.GetValue(option.id, value))
         parameters.emplace_back(i, value);
   }
   return parameters;
}

