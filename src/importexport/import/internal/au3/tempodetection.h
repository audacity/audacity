/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <optional>
#include <vector>

#include "modularity/ioc.h"
#include "interactive/iinteractive.h"
#include "workspace/iworkspacemanager.h"
#include "context/iglobalcontext.h"
#include "trackedit/itrackeditinteraction.h"

#include "../../iimporterconfiguration.h"

#include "io/path.h"

#include "au3-file-formats/AcidizerTags.h"

class WaveTrack;
class WaveClip;

namespace au::importexport {
struct ImportedClipInfo {
    trackedit::TrackId dstTrackId;
    int64_t clipId;
    double endTimeBeforeStretch;
};
struct TempoDetectionResult
{
    double bpm = 0.0;
    /// Ordered by reliability: Header is most reliable (lowest value = highest priority).
    enum class Source {
        Header, Filename, Signal
    };
    Source source = Source::Signal;
};

class TempoDetection : public muse::Contextable
{
    muse::Inject<muse::IInteractive> interactive{ this };
    muse::Inject<muse::workspace::IWorkspaceManager> workspacesManager{ this };
    muse::Inject<au::context::IGlobalContext> globalContext{ this };
    muse::Inject<IImporterConfiguration> configuration{ this };
    muse::Inject<trackedit::ITrackeditInteraction> trackeditInteraction{ this };

public:
    explicit TempoDetection(const muse::modularity::ContextPtr& ctx);

    /// Run tempo detection on the given imported tracks and show appropriate dialogs.
    void onFilesImported(const std::vector<muse::io::path_t>& filePaths, const std::vector<WaveTrack*>& waveTracks,
                         const std::vector<trackedit::TrackId>& dstTrackIds, const std::optional<LibFileFormats::AcidizerTags>& acidTags,
                         bool projectWasEmpty);

private:
    std::optional<TempoDetectionResult> detectTempo(const muse::io::path_t& filePath, const WaveTrack& track,
                                                    const std::optional<LibFileFormats::AcidizerTags>& acidTags);

    void showEmptyMusicWorkspaceDialog(double bpm, const std::vector<WaveTrack*>& waveTracks,
                                       const std::vector<trackedit::TrackId>& dstTrackIds);
    void showSubsequentImportDialog(double bpm, const std::vector<WaveTrack*>& waveTracks,
                                    const std::vector<trackedit::TrackId>& dstTrackIds);

    static void setRawAudioTempoOnClips(const std::vector<WaveTrack*>& waveTracks, double bpm);
    void stretchClipsToProjectTempo(const std::vector<WaveTrack*>& waveTracks, const std::vector<trackedit::TrackId>& dstTrackIds,
                                    double detectedBpm);

    static std::vector<ImportedClipInfo> collectImportedClipInfos(const std::vector<WaveTrack*>& waveTracks,
                                                                  const std::vector<trackedit::TrackId>& dstTrackIds);
    static void applyTempoToClips(const std::vector<WaveTrack*>& waveTracks, double detectedBpm, double projectTempo);
    void makeRoomAndCloseGaps(const std::vector<ImportedClipInfo>& clips);

    void setProjectTempo(double bpm);
    void reloadProject();
};
}
