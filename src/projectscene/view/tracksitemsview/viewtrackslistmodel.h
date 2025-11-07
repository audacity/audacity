/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QAbstractListModel>

#include <actions/actionable.h>

#include "global/async/asyncable.h"

#include "iprojectsceneconfiguration.h"
#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "playback/iplaybackconfiguration.h"
#include "trackedit/iselectioncontroller.h"
#include "trackedit/iprojecthistory.h"
#include "trackedit/itrackeditinteraction.h"
#include "playback/itrackplaybackcontrol.h"
#include "projectscene/iprojectsceneconfiguration.h"

#include "trackedit/dom/track.h"

namespace au::projectscene {
class ViewTracksListModel : public QAbstractListModel, public muse::async::Asyncable, public muse::actions::Actionable
{
    Q_OBJECT

    Q_PROPERTY(bool isVerticalRulersVisible READ isVerticalRulersVisible NOTIFY isVerticalRulersVisibleChanged)
    Q_PROPERTY(int totalTracksHeight READ totalTracksHeight NOTIFY totalTracksHeightChanged FINAL)

    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<projectscene::IProjectSceneConfiguration> projectSceneConfiguration;
    muse::Inject<trackedit::ISelectionController> selectionController;
    muse::Inject<trackedit::IProjectHistory> projectHistory;
    muse::Inject<trackedit::ITrackeditInteraction> trackeditInteraction;
    muse::Inject<playback::ITrackPlaybackControl> trackPlaybackControl;
    muse::Inject<playback::IPlaybackConfiguration> playbackConfiguration;

public:
    explicit ViewTracksListModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();
    Q_INVOKABLE void handleDroppedFiles(const QStringList& fileUrls);
    Q_INVOKABLE void toggleVerticalRuler() const;
    Q_INVOKABLE void setTrackRulerType(const trackedit::TrackId& trackId, int rulerType);

    int rowCount(const QModelIndex& parent) const override;
    QVariant data(const QModelIndex& index, int role) const override;
    QHash<int, QByteArray> roleNames() const override;

    bool isVerticalRulersVisible() const;
    int totalTracksHeight() const;

signals:
    void dataSelectedTracksChanged();
    void selectedTracksChanged();
    void isVerticalRulersVisibleChanged();

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
        IsLinearRole,
        AvailableRulerTypesRole,
        TrackRulerTypeRole,
        IsWaveformViewVisibleRole,
        IsSpectrogramViewVisibleRole,
        DbRangeRole,
    };

    std::vector<trackedit::Track> m_trackList;
};
}
