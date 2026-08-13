#include "dropcontroller.h"

#include "log.h"

using namespace au::projectscene;

namespace {
auto isAudioTrack = [](au::trackedit::TrackType type) {
    return type == au::trackedit::TrackType::Mono || type == au::trackedit::TrackType::Stereo;
};
}

DropController::DropController(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{}

void DropController::probeAudioFiles(const QStringList& fileUrls)
{
    m_lastDraggedUrls.clear();
    m_lastDraggedFilesInfo.clear();
    m_lastDraggedLabelFiles.clear();

    const auto exts = importer()->supportedExtensions();
    const auto labelExts = labelsImporter()->supportedExtensions();
    for (const auto& fileUrl : fileUrls) {
        const QUrl url(fileUrl);
        QString local = url.isLocalFile() ? url.toLocalFile() : fileUrl;
        muse::io::path_t path = muse::io::path_t(local);
        //! NOTE: the audio importer's extension list includes the label extensions,
        //! so check for label files first
        if (muse::contains(labelExts, muse::io::suffix(path))) {
            m_lastDraggedLabelFiles.push_back({ path, m_lastDraggedFilesInfo.size() });
        } else if (muse::contains(exts, muse::io::suffix(path))) {
            au::importexport::FileInfo fileInfo = importer()->fileInfo(path);
            if (fileInfo.isEmpty()) {
                continue;
            }
            m_lastDraggedFilesInfo.push_back(std::move(fileInfo));
            m_lastDraggedUrls.push_back(fileUrl);
        }
    }
}

QVariantList DropController::lastProbedDurations() const
{
    QVariantList out;
    out.reserve(static_cast<int>(m_lastDraggedFilesInfo.size()));
    for (const auto& info : m_lastDraggedFilesInfo) {
        for (int i = 0; i < info.trackCount; ++i) {
            out.push_back(info.duration);
        }
    }
    return out;
}

QVariantList DropController::lastProbedFileNames() const
{
    QVariantList out;
    out.reserve(static_cast<int>(requiredTracksCount()));

    auto urlIter  = m_lastDraggedUrls.cbegin();
    auto infoIter = m_lastDraggedFilesInfo.cbegin();
    for (; urlIter != m_lastDraggedUrls.cend() && infoIter != m_lastDraggedFilesInfo.cend();
         ++urlIter, ++infoIter) {
        std::string title = muse::io::filename(*urlIter, false /* including extension */).toStdString();

        for (int n = 0; n < infoIter->trackCount; ++n) {
            out.push_back(QString::fromStdString(title));
        }
    }

    return out;
}

void DropController::startImportSession()
{
    if (m_trackCountBeforeImport != -1) {
        return;
    }

    au::trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    m_trackCountBeforeImport = prj->trackList().size();
}

void DropController::endImportSession()
{
    tracksInteraction()->removeDragAddedTracks(m_trackCountBeforeImport, true /* emptyOnly */);

    m_trackCountBeforeImport = -1;
    m_lastDraggedFilesInfo.clear();
    m_lastDraggedUrls.clear();
    m_lastDraggedLabelFiles.clear();
}

int DropController::requiredTracksCount() const
{
    int count = 0;
    for (const auto& info : m_lastDraggedFilesInfo) {
        count += info.trackCount;
    }

    return count;
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
        =(m_trackCountBeforeImport >= 0)
          ? std::max(0, totalTracks - m_trackCountBeforeImport)
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
        thresholdRow = std::max(0, m_trackCountBeforeImport);
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
    if (draggedFilesCount <= 0 || m_trackCountBeforeImport < 0) {
        return;
    }

    au::trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    std::vector<trackedit::Track> trackList = prj->trackList();

    const int total = static_cast<int>(trackList.size());
    if (total <= m_trackCountBeforeImport) {
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
    } else if (m_trackCountBeforeImport >= 0
               && m_trackCountBeforeImport < total) {
        // cursor is below the last track
        startIndex = m_trackCountBeforeImport;
    } else {
        // fallback
        startIndex = 0;
    }

    int remaining = draggedFilesCount;
    int highestUsedIndex = m_trackCountBeforeImport - 1;

    // detect where dragged files will land: walk from startIndex downwards,
    // counting only audio tracks.
    for (int i = startIndex; i < total && remaining > 0; ++i) {
        if (!isAudioTrack(trackList[i].type)) {
            continue;
        }

        highestUsedIndex = std::max(highestUsedIndex, i);
        --remaining;
    }

    int neededTracksCount = m_trackCountBeforeImport;
    if (highestUsedIndex >= 0) {
        neededTracksCount = std::max(neededTracksCount, highestUsedIndex + 1);
    }

    // remove only extra empty tracks beyond neededTracksCount
    tracksInteraction()->removeDragAddedTracks(neededTracksCount, true /* emptyOnly */);
}

void DropController::handleDroppedFiles(const std::vector<trackedit::TrackId>& trackIds, double startTime)
{
    //! NOTE: audio first, so that the imported label tracks can be positioned
    //! relative to the audio destination tracks
    if (!m_lastDraggedFilesInfo.empty() && !trackIds.empty()) {
        std::vector<muse::io::path_t> localPaths;

        // NOTE: importer only needs the first trackId (out of many) for multichannel files
        // while `trackIds` contains all, we may need to skip some of them
        std::vector<trackedit::TrackId> adjustedDstTrackIds;
        auto dstTrackIter = trackIds.begin();
        for (const auto& info : m_lastDraggedFilesInfo) {
            localPaths.push_back(info.path);

            adjustedDstTrackIds.push_back(*dstTrackIter);
            std::advance(dstTrackIter, info.trackCount);
        }

        project::IAudacityProjectPtr prj = globalContext()->currentProject();

        prj->importIntoTracks(localPaths, adjustedDstTrackIds, startTime);
    }

    importDroppedLabelFiles(trackIds);
}

void DropController::importDroppedLabelFiles(const std::vector<trackedit::TrackId>& audioDstTrackIds)
{
    if (m_lastDraggedLabelFiles.empty()) {
        return;
    }

    // last destination track of each dragged audio file, used as the insertion
    // anchor for the label track that follows it in the dropped file order
    std::vector<trackedit::TrackId> anchorPerAudioFile;
    size_t consumed = 0;
    for (const auto& info : m_lastDraggedFilesInfo) {
        if (consumed >= audioDstTrackIds.size()) {
            break;
        }
        const size_t count = std::max(info.trackCount, 1);
        const size_t last = std::min(consumed + count, audioDstTrackIds.size()) - 1;
        anchorPerAudioFile.push_back(audioDstTrackIds[last]);
        consumed += count;
    }

    const trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();

    auto trackRow = [&prj](const trackedit::TrackId& trackId) -> int {
        const std::vector<trackedit::Track> tracks = prj->trackList();
        for (size_t i = 0; i < tracks.size(); ++i) {
            if (tracks[i].id == trackId) {
                return static_cast<int>(i);
            }
        }
        return -1;
    };

    trackedit::TrackId previousLabelTrackId = -1;
    size_t previousPrecedingCount = 0;

    for (const LabelFile& labelFile : m_lastDraggedLabelFiles) {
        muse::RetVal<trackedit::TrackId> rv = labelsImporter()->importData(labelFile.path);
        if (!rv.ret) {
            LOGE() << rv.ret.toString();
            continue;
        }

        if (!prj || anchorPerAudioFile.empty()) {
            // no audio dropped along: leave the label track appended at the end
            continue;
        }

        //! NOTE: the new label track is appended at the end of the track list;
        //! move it so that the interleaved order of the dropped files is preserved
        const size_t precedingCount = std::min(labelFile.precedingAudioFiles, anchorPerAudioFile.size());

        int anchorRow = -1;
        bool placeBelowAnchor = true;
        if (previousLabelTrackId != -1 && precedingCount == previousPrecedingCount) {
            // consecutive label files: place below the previously placed label track
            anchorRow = trackRow(previousLabelTrackId);
        } else if (precedingCount == 0) {
            // label file dragged before any audio file: place above the first audio track
            anchorRow = trackRow(anchorPerAudioFile.front());
            placeBelowAnchor = false;
        } else {
            anchorRow = trackRow(anchorPerAudioFile[precedingCount - 1]);
        }

        if (anchorRow >= 0) {
            tracksInteraction()->moveTracksTo({ rv.val }, placeBelowAnchor ? anchorRow + 1 : anchorRow);
        }

        previousLabelTrackId = rv.val;
        previousPrecedingCount = precedingCount;
    }
}
