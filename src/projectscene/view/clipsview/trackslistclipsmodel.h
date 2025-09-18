/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QAbstractListModel>

#include <actions/actionable.h>
#include <actions/iactionsdispatcher.h>

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "iprojectsceneconfiguration.h"
#include "trackedit/iselectioncontroller.h"
#include "trackedit/iprojecthistory.h"
#include "playback/itrackplaybackcontrol.h"

#include "trackedit/dom/track.h"

namespace au::projectscene {
class TracksListClipsModel : public QAbstractListModel, public muse::async::Asyncable, public muse::actions::Actionable
{
    Q_OBJECT

    Q_PROPERTY(bool isVerticalRulersVisible READ isVerticalRulersVisible NOTIFY isVerticalRulersVisibleChanged)
    Q_PROPERTY(int totalTracksHeight READ totalTracksHeight NOTIFY totalTracksHeightChanged FINAL)

    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<IProjectSceneConfiguration> configuration;
    muse::Inject<trackedit::ISelectionController> selectionController;
    muse::Inject<trackedit::IProjectHistory> projectHistory;
    muse::Inject<playback::ITrackPlaybackControl> trackPlaybackControl;

public:
    explicit TracksListClipsModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();
    Q_INVOKABLE void startUserInteraction();
    Q_INVOKABLE void endUserInteraction();
    Q_INVOKABLE void handleDroppedFiles(const QStringList& fileUrls);

    int rowCount(const QModelIndex& parent) const override;
    QVariant data(const QModelIndex& index, int role) const override;
    QHash<int, QByteArray> roleNames() const override;

    bool isVerticalRulersVisible() const;
    int totalTracksHeight() const;

signals:
    void dataSelectedTracksChanged();
    void selectedTracksChanged();
    void isVerticalRulersVisibleChanged(bool isVerticalRulersVisible);

    void totalTracksHeightChanged();

private:
    void setIsVerticalRulersVisible(bool isVerticalRulersVisible);

    enum RoleNames {
        TypeRole = Qt::UserRole + 1,
        TrackIdRole,
        IsDataSelectedRole,
        IsTrackSelectedRole,
        IsTrackFocusedRole,
        IsMultiSelectionActiveRole,
        IsTrackAudibleRole,
    };

    std::vector<trackedit::Track> m_trackList;
    bool m_isVerticalRulersVisible = false;
};
}
