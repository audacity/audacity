/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QAbstractListModel>

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "iprojectsceneconfiguration.h"
#include "trackedit/iselectioncontroller.h"

#include "trackedit/dom/track.h"

namespace au::projectscene {
class TracksListClipsModel : public QAbstractListModel, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(bool isVerticalRulersVisible READ isVerticalRulersVisible NOTIFY isVerticalRulersVisibleChanged)

    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<IProjectSceneConfiguration> configuration;
    muse::Inject<trackedit::ISelectionController> selectionController;

public:

    TracksListClipsModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();

    int rowCount(const QModelIndex& parent) const override;
    QVariant data(const QModelIndex& index, int role) const override;
    QHash<int, QByteArray> roleNames() const override;

    bool isVerticalRulersVisible() const;

signals:
    void dataSelectedTracksChanged();
    void isVerticalRulersVisibleChanged(bool isVerticalRulersVisible);

private:
    void setIsVerticalRulersVisible(bool isVerticalRulersVisible);

    enum RoleNames {
        TypeRole = Qt::UserRole + 1,
        TrackIdRole,
        IsDataSelectedRole
    };

    void setDataSelectedTracks(const std::vector<trackedit::TrackId>& tracks);

    muse::async::NotifyList<au::trackedit::Track> m_trackList;
    std::vector<trackedit::TrackId> m_dataSelectedTracks;
    bool m_isVerticalRulersVisible = false;
};
}
