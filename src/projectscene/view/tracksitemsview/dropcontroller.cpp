#include "dropcontroller.h"

using namespace au::projectscene;

namespace {
auto isAudioTrack = [](au::trackedit::TrackType type) {
    return type == au::trackedit::TrackType::Mono || type == au::trackedit::TrackType::Stereo;
};
}

DropController::DropController(QObject* parent)
{}

void DropController::probeAudioFilesLength(const QStringList& fileUrls)
{
    if (fileUrls == m_lastDraggedUrls) {
        return;
    }

    m_lastDraggedUrls = fileUrls;
    m_lastDraggedFilesInfo.clear();
    m_lastDraggedFilesInfo.reserve(fileUrls.size());

    std::vector<muse::io::path_t> localPaths;
    localPaths.reserve(fileUrls.size());

    for (const auto& fileUrl : fileUrls) {
        const QUrl url(fileUrl);
        localPaths.push_back(muse::io::path_t(url.toLocalFile()));
    }

    if (localPaths.empty()) {
        return;
    }

    project::IAudacityProjectPtr prj = globalContext()->currentProject();

    for (const auto& path : localPaths) {
        au::importexport::FileInfo fileInfo = importer()->fileInfo(path);
        m_lastDraggedFilesInfo.push_back(std::move(fileInfo));
    }
}

QVariantList DropController::lastProbedDurations() const
{
    QVariantList out;
    out.reserve(static_cast<int>(m_lastDraggedFilesInfo.size()));
    for (const auto& info : m_lastDraggedFilesInfo) {
        out.push_back(info.duration);
    }
    return out;
}

QVariantList DropController::lastProbedFileNames() const
{
    QVariantList out;
    out.reserve(static_cast<int>(m_lastDraggedUrls.size()));
    for (const auto& info : m_lastDraggedUrls) {
        std::string title = muse::io::filename(info, false /* including extension */).toStdString();
        out.push_back(QString::fromStdString(title));
    }
    return out;
}

void DropController::startImportDrag()
{
    if (m_tracksCountWhenDragStarted != -1) {
        return;
    }

    au::trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    m_tracksCountWhenDragStarted = prj->trackList().size();
}

void DropController::endImportDrag()
{
    tracksInteraction()->removeDragAddedTracks(m_tracksCountWhenDragStarted, true /* emptyOnly */);

    m_tracksCountWhenDragStarted = -1;
    m_lastDraggedFilesInfo.clear();
    m_lastDraggedUrls.clear();
}

void DropController::prepareConditionalTracks(int currentTrackId, int draggedFilesCount)
{
    if (draggedFilesCount <= 0) {
        return;
    }

    au::trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    std::vector<trackedit::Track> trackList = prj->trackList();

    const int totalTracks = static_cast<int>(trackList.size());

    const int tracksCreated
        =(m_tracksCountWhenDragStarted >= 0)
          ? std::max(0, totalTracks - m_tracksCountWhenDragStarted)
          : 0;

    if (tracksCreated >= draggedFilesCount) {
        return;
    }

    int currentIndex = -1;
    for (int i = 0; i < totalTracks; ++i) {
        if (trackList[i].id == currentTrackId) {
            currentIndex = i;
            break;
        }
    }

    int startIndex = (currentIndex < 0) ? totalTracks : currentIndex;

    int availableAudioTracks = 0;
    for (int i = startIndex; i < totalTracks; ++i) {
        if (isAudioTrack(trackList[i].type)) {
            ++availableAudioTracks;
        }
    }

    int missingTracks = draggedFilesCount - availableAudioTracks;
    if (missingTracks <= 0) {
        return;
    }

    const int maxNewAllowed = draggedFilesCount - tracksCreated;
    if (maxNewAllowed <= 0) {
        return;
    }

    const int toCreate = std::min(missingTracks, maxNewAllowed);

    for (int i = 0; i < toCreate; ++i) {
        tracksInteraction()->addWaveTrack(1);
    }
}

QVariantList DropController::draggedTracksIds(int currentTrackId, int draggedFilesCount)
{
    au::trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return {};
    }

    std::vector<trackedit::Track> trackList = prj->trackList();

    QVariantList trackIds;
    if (draggedFilesCount <= 0 || trackList.empty()) {
        return trackIds;
    }

    const int total = static_cast<int>(trackList.size());

    // collect indices of all audio tracks
    std::vector<int> audioIndices;
    audioIndices.reserve(total);
    for (int i = 0; i < total; ++i) {
        if (isAudioTrack(trackList[i].type)) {
            audioIndices.push_back(i);
        }
    }

    if (audioIndices.empty()) {
        return trackIds;
    }

    int currentRow = -1;
    if (currentTrackId >= 0) {
        for (int i = 0; i < total; ++i) {
            if (trackList[i].id == currentTrackId) {
                currentRow = i;
                break;
            }
        }
    }

    int thresholdRow;
    if (currentRow >= 0) {
        // cursor is over a track, start from that track
        thresholdRow = currentRow;
    } else {
        // cursor is below tracks, start from newly created track
        thresholdRow = std::max(0, m_tracksCountWhenDragStarted);
    }

    int startAudioPos = 0;
    while (startAudioPos < static_cast<int>(audioIndices.size())
           && audioIndices[startAudioPos] < thresholdRow) {
        ++startAudioPos;
    }

    const int availFromStart = static_cast<int>(audioIndices.size()) - startAudioPos;

    if (availFromStart >= draggedFilesCount) {
        for (int i = 0; i < draggedFilesCount; ++i) {
            int trackIndex = audioIndices[startAudioPos + i];
            trackIds.push_back(static_cast<int>(trackList[trackIndex].id));
        }
        return trackIds;
    }

    // fallback, not enough tracks: use last `draggedFilesCount` audio tracks
    const int totalAudio = static_cast<int>(audioIndices.size());
    const int toTake = std::min(draggedFilesCount, totalAudio);
    const int firstIndex = totalAudio - toTake;

    for (int i = firstIndex; i < totalAudio; ++i) {
        int trackIndex = audioIndices[i];
        trackIds.push_back(static_cast<int>(trackList[trackIndex].id));
    }

    return trackIds;
}

void DropController::removeDragAddedTracks(int currentTrackId, int draggedFilesCount)
{
    if (draggedFilesCount <= 0 || m_tracksCountWhenDragStarted < 0) {
        return;
    }

    au::trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    std::vector<trackedit::Track> trackList = prj->trackList();

    const int total = static_cast<int>(trackList.size());
    if (total <= m_tracksCountWhenDragStarted) {
        return;
    }

    int currentIndex = -1;
    for (int i = 0; i < total; ++i) {
        if (trackList[i].id == currentTrackId) {
            currentIndex = i;
            break;
        }
    }

    int startIndex;
    if (currentIndex >= 0) {
        // cursor is over some existing track (label or audio)
        startIndex = currentIndex;
    } else if (m_tracksCountWhenDragStarted >= 0
               && m_tracksCountWhenDragStarted < total) {
        // cursor is below the last track
        startIndex = m_tracksCountWhenDragStarted;
    } else {
        // fallback
        startIndex = 0;
    }

    int remaining = draggedFilesCount;
    int highestUsedIndex = m_tracksCountWhenDragStarted - 1;

    // detect where dragged files will land: walk from startIndex downwards,
    // counting only audio tracks.
    for (int i = startIndex; i < total && remaining > 0; ++i) {
        if (!isAudioTrack(trackList[i].type)) {
            continue;
        }

        highestUsedIndex = std::max(highestUsedIndex, i);
        --remaining;
    }

    int neededTracksCount = m_tracksCountWhenDragStarted;
    if (highestUsedIndex >= 0) {
        neededTracksCount = std::max(neededTracksCount, highestUsedIndex + 1);
    }

    // remove only extra empty tracks beyond neededTracksCount
    tracksInteraction()->removeDragAddedTracks(neededTracksCount, true /* emptyOnly */);
}

void DropController::handleDroppedFiles(const std::vector<trackedit::TrackId>& trackIds, double startTime, const QStringList& fileUrls)
{
    std::vector<muse::io::path_t> localPaths;
    for (const auto& fileUrl : fileUrls) {
        QUrl url(fileUrl);
        localPaths.push_back(muse::io::path_t(url.toLocalFile()));
    }

    project::IAudacityProjectPtr prj = globalContext()->currentProject();

    prj->importIntoTracks(localPaths, trackIds, startTime);
}
