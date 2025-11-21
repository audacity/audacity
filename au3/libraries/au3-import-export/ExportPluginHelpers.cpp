/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportPluginHelpers.cpp

  Dominic Mazzoni

  Vitaly Sverchinsky split from ExportPlugin.h

**********************************************************************/

#include "ExportPluginHelpers.h"
#include "Track.h"
#include "Mix.h"
#include "WaveTrack.h"
#include "MixAndRender.h"
#include "ExportUtils.h"
#include "ExportPlugin.h"
#include "StretchingSequence.h"

//Create a mixer by computing the time warp factor
std::unique_ptr<Mixer> ExportPluginHelpers::CreateMixer(
    const AudacityProject& project, bool selectionOnly, double startTime,
    double stopTime, unsigned numOutChannels, size_t outBufferSize,
    bool outInterleaved, double outRate, sampleFormat outFormat,
    MixerOptions::Downmix* mixerSpec)
{
    Mixer::Inputs inputs;
    const auto& tracks = TrackList::Get(project);
    for (auto pTrack: ExportUtils::FindExportWaveTracks(tracks, selectionOnly)) {
        inputs.emplace_back(
            StretchingSequence::Create(*pTrack, pTrack->GetClipInterfaces()),
            GetEffectStages(*pTrack));
    }
    auto masterEffectStages = GetMasterEffectStages(project);
    //custom channel mapping isn't support with master effects on
    assert(masterEffectStages.empty() || (numOutChannels <= 2 && mixerSpec == nullptr));
    // MB: the stop time should not be warped, this was a bug.
    return std::make_unique<Mixer>(
        std::move(inputs), std::move(masterEffectStages),
        // Throw, to stop exporting, if read fails:
        true, Mixer::WarpOptions { tracks.GetOwner() }, startTime, stopTime,
        numOutChannels, outBufferSize, outInterleaved, outRate, outFormat, true,
        mixerSpec,
        mixerSpec ? Mixer::ApplyVolume::MapChannels : Mixer::ApplyVolume::Mixdown);
}

namespace {
double EvalExportProgress(Mixer& mixer, double t0, double t1)
{
    const auto duration = t1 - t0;
    if (duration > 0) {
        return std::clamp(mixer.MixGetCurrentTime() - t0, .0, duration) / duration;
    }
    return .0;
}
}

ExportResult ExportPluginHelpers::UpdateProgress(ExportProcessorDelegate& delegate, Mixer& mixer, double t0, double t1)
{
    delegate.OnProgress(EvalExportProgress(mixer, t0, t1));
    if (delegate.IsStopped()) {
        return ExportResult::Stopped;
    }
    if (delegate.IsCancelled()) {
        return ExportResult::Cancelled;
    }
    return ExportResult::Success;
}
