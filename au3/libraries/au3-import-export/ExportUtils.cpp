/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportUtils.cpp

  Dominic Mazzoni

  Vitaly Sverchinsky split from ExportPlugin.h

**********************************************************************/

#include "ExportUtils.h"

#include <algorithm>
#include <vector>

#include "Track.h"
#include "ViewInfo.h"
#include "WaveTrack.h"

//TODO: used in many places in anticipation that Exporter yields same result, fix that
TrackIterRange<const WaveTrack> ExportUtils::FindExportWaveTracks(const TrackList& tracks, bool selectedOnly)
{
    bool anySolo
        =!(tracks.Any<const WaveTrack>() + &WaveTrack::GetSolo).empty();

    return tracks.Any<const WaveTrack>()
           + (selectedOnly ? &Track::IsSelected : &Track::Any)
           - (anySolo ? &WaveTrack::GetNotSolo : &WaveTrack::GetMute);
}

bool ExportUtils::HasSelectedAudio(const AudacityProject& project)
{
    return !FindExportWaveTracks(TrackList::Get(project), true).empty()
           && !ViewInfo::Get(project).selectedRegion.isPoint();
}

ExportProcessor::Parameters ExportUtils::ParametersFromEditor(const ExportOptionsEditor& editor)
{
    ExportProcessor::Parameters parameters;
    for (int i = 0, count = editor.GetOptionsCount(); i < count; ++i) {
        ExportOption option;
        ExportValue value;
        if (editor.GetOption(i, option) && editor.GetValue(option.id, value)) {
            parameters.emplace_back(option.id, value);
        }
    }
    return parameters;
}

namespace {
struct ExportHookElement final
{
    ExportUtils::ExportHook hook;
    ExportUtils::Priority priority;
};

auto& ExportHooks()
{
    static std::vector<ExportHookElement> hooks;
    return hooks;
}
}

void ExportUtils::RegisterExportHook(ExportHook hook, Priority priority)
{
    auto& hooks = ExportHooks();
    hooks.insert(
        std::upper_bound(
            hooks.begin(), hooks.end(), priority,
            [](Priority priority, const ExportHookElement& element)
    { return priority > element.priority; }),
        { hook, priority });
}

void ExportUtils::PerformInteractiveExport(
    AudacityProject& project, const FileExtension& format, AudiocomTrace trace,
    bool selectedOnly)
{
    auto& hooks = ExportHooks();
    for (auto& hook : hooks) {
        if (
            hook.hook(project, format, trace, selectedOnly)
            != ExportHookResult::Continue) {
            return;
        }
    }
}
