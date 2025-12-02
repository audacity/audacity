/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QAbstractListModel>

#include <actions/actionable.h>

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "projectscene/iprojectsceneconfiguration.h"
#include "trackedit/iselectioncontroller.h"
#include "trackedit/iprojecthistory.h"
#include "trackedit/itrackeditinteraction.h"
#include "playback/itrackplaybackcontrol.h"
#include "playback/iplaybackconfiguration.h"
#include "importexport/import/iimporter.h"
#include "trackedit/itracksinteraction.h"

#include "trackedit/dom/track.h"

namespace au::projectscene {
class ViewTracksListModel : public QAbstractListModel, public muse::async::Asyncable, public muse::actions::Actionable
{
    Q_OBJECT

    Q_PROPERTY(bool isVerticalRulersVisible READ isVerticalRulersVisible NOTIFY isVerticalRulersVisibleChanged)
    Q_PROPERTY(int verticalRulerWidth READ verticalRulerWidth NOTIFY verticalRulerWidthChanged FINAL)
    Q_PROPERTY(int totalTracksHeight READ totalTracksHeight NOTIFY totalTracksHeightChanged FINAL)

    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<projectscene::IProjectSceneConfiguration> projectSceneConfiguration;
    muse::Inject<trackedit::ISelectionController> selectionController;
    muse::Inject<trackedit::IProjectHistory> projectHistory;
    muse::Inject<trackedit::ITrackeditInteraction> trackeditInteraction;
    muse::Inject<playback::ITrackPlaybackControl> trackPlaybackControl;
    muse::Inject<playback::IPlaybackConfiguration> playbackConfiguration;
    muse::Inject<importexport::IImporter> importer;
    muse::Inject<trackedit::ITracksInteraction> tracksInteraction;

public:
    explicit ViewTracksListModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();
    Q_INVOKABLE double audioFileLength(const QStringList& fileUrls);
    Q_INVOKABLE QString audioFileName(const QString& fileUrl);
    Q_INVOKABLE void startImportDrag();
    Q_INVOKABLE void endImportDrag();
    Q_INVOKABLE void prepareConditionalTracks(int currentTrackId, int draggedFileCount);
    Q_INVOKABLE QVariant draggedTracksIds(int currentTrackId, int draggedFilesCount);
    Q_INVOKABLE void removeDragAddedTracks(int currentTrackId, int draggedFilesCount);
    Q_INVOKABLE void handleDroppedFiles(const trackedit::TrackId& trackId, double startTime, const QStringList& fileUrls);

    int rowCount(const QModelIndex& parent) const override;
    QVariant data(const QModelIndex& index, int role) const override;
    QHash<int, QByteArray> roleNames() const override;

    bool isVerticalRulersVisible() const;
    int totalTracksHeight() const;
    int verticalRulerWidth() const;

signals:
    void dataSelectedTracksChanged();
    void selectedTracksChanged();
    void isVerticalRulersVisibleChanged();
    void verticalRulerWidthChanged();

    void totalTracksHeightChanged();
    void escapePressed();

private:
    enum RoleNames {
        TypeRole = Qt::UserRole + 1,
        TrackIdRole,
        IsDataSelectedRole,
        IsTrackSelectedRole,
        IsTrackFocusedRole,
        IsMultiSelectionActiveRole,
        IsTrackAudibleRole,
        IsStereoRole,
        IsWaveformViewVisibleRole,
        IsSpectrogramViewVisibleRole,
        DbRangeRole,
        ColorRole,
    };

    std::vector<trackedit::Track> m_trackList;

    // TODO std::pair<importexport::FileInfo, std::chrono::steady_clock::time_point> m_lastProbedFileInfo;
    // TODO: will need a vector for multiple files
    importexport::FileInfo m_lastProbedFileInfo;
    int m_tracksCountWhenDragStarted = -1;
};
}
