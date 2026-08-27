/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "importexport/import/iimporter.h"
#include "importexport/labels/ilabelsimporter.h"
#include "trackedit/itracksinteraction.h"

namespace au::projectscene {
class DropController : public QObject, public muse::Contextable
{
    Q_OBJECT

    muse::ContextInject<au::context::IGlobalContext> globalContext{ this };
    muse::ContextInject<importexport::IImporter> importer{ this };
    muse::ContextInject<importexport::ILabelsImporter> labelsImporter{ this };
    muse::ContextInject<trackedit::ITracksInteraction> tracksInteraction{ this };

public:
    explicit DropController(QObject* parent = nullptr);

    Q_INVOKABLE void probeAudioFiles(const QStringList& fileUrls);
    Q_INVOKABLE QVariantList lastProbedDurations() const;
    Q_INVOKABLE QVariantList lastProbedFileNames() const;
    Q_INVOKABLE void startImportSession();
    Q_INVOKABLE void endImportSession();
    Q_INVOKABLE int requiredTracksCount() const;
    Q_INVOKABLE void prepareConditionalTracks(int currentTrackId, int draggedFileCount);
    Q_INVOKABLE QVariantList draggedTracksIds(int currentTrackId, int draggedFilesCount);
    Q_INVOKABLE void removeDragAddedTracks(int currentTrackId, int draggedFilesCount);
    Q_INVOKABLE void updateLabelPreviewTracks(const std::vector<trackedit::TrackId>& audioDstTrackIds);
    Q_INVOKABLE void handleDroppedFiles(const std::vector<trackedit::TrackId>& trackIds, double startTime);

private:
    struct LabelFile {
        muse::io::path_t path;
        //! NOTE: number of dragged audio files preceding this label file,
        //! used to preserve the interleaved order of the dropped files
        size_t precedingAudioFiles = 0;
    };

    std::vector<trackedit::TrackId> computeDraggedTracksIds(int currentTrackId, int draggedFilesCount) const;
    void positionLabelTracks(const std::vector<trackedit::TrackId>& labelTrackIds,
                             const std::vector<trackedit::TrackId>& audioDstTrackIds);
    void importDroppedLabelFiles(const std::vector<trackedit::TrackId>& audioDstTrackIds);

    std::vector<au::importexport::FileInfo> m_lastDraggedFilesInfo;
    QStringList m_lastDraggedUrls;
    std::vector<LabelFile> m_lastDraggedLabelFiles;
    std::vector<trackedit::TrackId> m_dragAddedWaveTrackIds;
    std::vector<trackedit::TrackId> m_dragAddedLabelTrackIds;
    int m_trackCountBeforeImport = -1;
};
}
