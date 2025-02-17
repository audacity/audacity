/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportUtils.h

  Dominic Mazzoni

  Vitaly Sverchinsky split from ExportPlugin.h

**********************************************************************/

#pragma once

#include "ExportTypes.h"
#include "ExportPlugin.h"

#include <functional>

class AudacityProject;
class TrackList;
class WaveTrack;

template<typename TrackType> struct TrackIterRange;

enum class AudiocomTrace
{
    ignore,
    ShareAudioButton,
    ShareAudioMenu,
    ShareAudioExportMenu,
    ShareAudioExportExtraMenu,
    SaveToCloudMenu,
    SaveProjectSaveToCloudMenu, // user chose "Save Project" and then saw the
                                // dialog and chose "Save to Cloud"
    PrefsPanel, // The Cloud preference panel also has a "Link Account" button
    ProjectOpenedAndUploadResumed,
    UpdateCloudAudioPreviewMenu,
    LinkAudiocomAccountHelpMenu,
    OpenFromCloudMenu,
};

class IMPORT_EXPORT_API ExportUtils final
{
public:

    static TrackIterRange<const WaveTrack> FindExportWaveTracks(const TrackList& tracks, bool selectedOnly);

    static bool HasSelectedAudio(const AudacityProject& project);

    static ExportProcessor::Parameters ParametersFromEditor(const ExportOptionsEditor& editor);

    enum class ExportHookResult
    {
        Handled,
        Continue,
        Cancel,
    };

    using ExportHook = std::function<ExportHookResult (
                                         AudacityProject&, const FileExtension&,
                                         AudiocomTrace, bool)>;

    using Priority = unsigned;
    static constexpr Priority DEFAULT_EXPORT_HOOK_PRIORITY = 0;

    static void RegisterExportHook(ExportHook hook, Priority = DEFAULT_EXPORT_HOOK_PRIORITY);
    static void PerformInteractiveExport(
        AudacityProject& project, const FileExtension& format, AudiocomTrace trace, bool selectedOnly);
};
