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
#include "trackedit/itrackeditinteraction.h"
#include "playback/itrackplaybackcontrol.h"
#include "playback/iplaybackconfiguration.h"

#include "trackedit/dom/track.h"

namespace au::projectscene {
class ViewTracksListModel : public QAbstractListModel, public muse::async::Asyncable, public muse::actions::Actionable
{
    Q_OBJECT

    Q_PROPERTY(bool isVerticalRulersVisible READ isVerticalRulersVisible NOTIFY isVerticalRulersVisibleChanged)
    Q_PROPERTY(int verticalRulerWidth READ verticalRulerWidth NOTIFY verticalRulerWidthChanged FINAL)
    Q_PROPERTY(int totalTracksHeight READ totalTracksHeight NOTIFY totalTracksHeightChanged FINAL)

    muse::GlobalInject<projectscene::IProjectSceneConfiguration> projectSceneConfiguration;
    muse::GlobalInject<playback::IPlaybackConfiguration> playbackConfiguration;

    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<trackedit::ISelectionController> selectionController;
    muse::Inject<trackedit::ITrackeditInteraction> trackeditInteraction;
    muse::Inject<playback::ITrackPlaybackControl> trackPlaybackControl;
public:
    explicit ViewTracksListModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();

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

    QModelIndex indexOf(trackedit::TrackId trackId);

    std::vector<trackedit::Track> m_trackList;
};
}
