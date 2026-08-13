#include "dropcontroller.h"

#include <algorithm>

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
    // remove the drag-added tracks that no file ended up being imported into
    const au::trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (prj) {
        trackedit::TrackIdList tracksToRemove;
        for (const trackedit::TrackId& trackId : m_dragAddedWaveTrackIds) {
            if (prj->clipList(trackId).empty()) {
                tracksToRemove.push_back(trackId);
            }
        }
        for (const trackedit::TrackId& trackId : m_dragAddedLabelTrackIds) {
            if (prj->labelList(trackId).empty()) {
                tracksToRemove.push_back(trackId);
            }
        }
        if (!tracksToRemove.empty()) {
            tracksInteraction()->deleteTracks(tracksToRemove);
        }
    }

    m_trackCountBeforeImport = -1;
    m_lastDraggedFilesInfo.clear();
    m_lastDraggedUrls.clear();
    m_lastDraggedLabelFiles.clear();
    m_dragAddedWaveTrackIds.clear();
    m_dragAddedLabelTrackIds.clear();
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

    const int tracksCreated = static_cast<int>(m_dragAddedWaveTrackIds.size());

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
        m_dragAddedWaveTrackIds.push_back(tracksInteraction()->addWaveTrack(1));
    }
}

QVariantList DropController::draggedTracksIds(int currentTrackId, int draggedFilesCount)
{
    QVariantList result;
    for (const trackedit::TrackId& trackId : computeDraggedTracksIds(currentTrackId, draggedFilesCount)) {
        result.push_back(static_cast<int>(trackId));
    }
    return result;
}

std::vector<au::trackedit::TrackId> DropController::computeDraggedTracksIds(int currentTrackId, int draggedFilesCount) const
{
    au::trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return {};
    }

    std::vector<trackedit::Track> trackList = prj->trackList();

    std::vector<trackedit::TrackId> trackIds;
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
            trackIds.push_back(trackList[trackIndex].id);
        }
        return trackIds;
    }

    // fallback, not enough tracks: use last `draggedFilesCount` audio tracks
    const int totalAudio = static_cast<int>(audioIndices.size());
    const int toTake = std::min(draggedFilesCount, totalAudio);
    const int firstIndex = totalAudio - toTake;

    for (int i = firstIndex; i < totalAudio; ++i) {
        int trackIndex = audioIndices[i];
        trackIds.push_back(trackList[trackIndex].id);
    }

    return trackIds;
}

void DropController::removeDragAddedTracks(int currentTrackId, int draggedFilesCount)
{
    if (m_dragAddedWaveTrackIds.empty()) {
        return;
    }

    au::trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    // remove the drag-added tracks that are no longer destinations of the dragged files
    const std::vector<trackedit::TrackId> dstTrackIds = computeDraggedTracksIds(currentTrackId, draggedFilesCount);

    trackedit::TrackIdList tracksToRemove;
    for (const trackedit::TrackId& trackId : m_dragAddedWaveTrackIds) {
        if (!muse::contains(dstTrackIds, trackId) && prj->clipList(trackId).empty()) {
            tracksToRemove.push_back(trackId);
        }
    }

    if (tracksToRemove.empty()) {
        return;
    }

    tracksInteraction()->deleteTracks(tracksToRemove);

    m_dragAddedWaveTrackIds.erase(std::remove_if(m_dragAddedWaveTrackIds.begin(), m_dragAddedWaveTrackIds.end(),
                                                 [&tracksToRemove](const trackedit::TrackId& trackId) {
        return muse::contains(tracksToRemove, trackId);
    }), m_dragAddedWaveTrackIds.end());
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

void DropController::updateLabelPreviewTracks(const std::vector<trackedit::TrackId>& audioDstTrackIds)
{
    if (m_lastDraggedLabelFiles.empty()) {
        return;
    }

    // create one preview label track per dragged label file, named after it;
    // the dropped file will be imported into it
    for (size_t i = m_dragAddedLabelTrackIds.size(); i < m_lastDraggedLabelFiles.size(); ++i) {
        const std::string title = muse::io::filename(m_lastDraggedLabelFiles[i].path, false /* includingExtension */).toStdString();
        muse::RetVal<trackedit::TrackId> rv = tracksInteraction()->newLabelTrack(muse::String::fromStdString(title));
        if (!rv.ret) {
            LOGE() << rv.ret.toString();
            return;
        }
        m_dragAddedLabelTrackIds.push_back(rv.val);
    }

    positionLabelTracks(m_dragAddedLabelTrackIds, audioDstTrackIds);
}

void DropController::positionLabelTracks(const std::vector<trackedit::TrackId>& labelTrackIds,
                                         const std::vector<trackedit::TrackId>& audioDstTrackIds)
{
    const trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    // last destination track of each dragged audio file, used as the insertion
    // anchor for the label track that follows it in the dragged file order
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

    if (anchorPerAudioFile.empty()) {
        // no audio dragged along: leave the label tracks appended at the end
        return;
    }

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

    for (size_t i = 0; i < labelTrackIds.size() && i < m_lastDraggedLabelFiles.size(); ++i) {
        const trackedit::TrackId labelTrackId = labelTrackIds[i];
        if (labelTrackId == -1) {
            continue;
        }

        //! NOTE: move the label track so that the interleaved order of the dragged files is preserved
        const size_t precedingCount = std::min(m_lastDraggedLabelFiles[i].precedingAudioFiles, anchorPerAudioFile.size());

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
            tracksInteraction()->moveTracksTo({ labelTrackId }, placeBelowAnchor ? anchorRow + 1 : anchorRow);
        }

        previousLabelTrackId = labelTrackId;
        previousPrecedingCount = precedingCount;
    }
}

void DropController::importDroppedLabelFiles(const std::vector<trackedit::TrackId>& audioDstTrackIds)
{
    if (m_lastDraggedLabelFiles.empty()) {
        return;
    }

    std::vector<trackedit::TrackId> importedTrackIds;
    for (size_t i = 0; i < m_lastDraggedLabelFiles.size(); ++i) {
        // import into the preview track created during the drag, if any
        const trackedit::TrackId dstTrackId = i < m_dragAddedLabelTrackIds.size() ? m_dragAddedLabelTrackIds[i] : -1;

        muse::RetVal<trackedit::TrackId> rv = labelsImporter()->importData(m_lastDraggedLabelFiles[i].path, dstTrackId);
        if (!rv.ret) {
            LOGE() << rv.ret.toString();
            importedTrackIds.push_back(-1);
            continue;
        }
        importedTrackIds.push_back(rv.val);
    }

    //! NOTE: preview tracks created during the drag are already in place;
    //! this positions the tracks of previewless imports (e.g. pasted files)
    positionLabelTracks(importedTrackIds, audioDstTrackIds);
}
