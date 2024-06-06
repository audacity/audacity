#pragma once

#include <QAbstractListModel>

#include <vector>

#include "iprojectsceneconfiguration.h"
#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "async/asyncable.h"

#include "processing/dom/track.h"

namespace au::projectscene {
class TracksListClipsModel : public QAbstractListModel, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<IProjectSceneConfiguration> configuration;

    Q_PROPERTY(bool isVerticalRulersVisible READ isVerticalRulersVisible NOTIFY isVerticalRulersVisibleChanged)

public:

    TracksListClipsModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();

    int rowCount(const QModelIndex& parent) const override;
    QVariant data(const QModelIndex& index, int role) const override;
    QHash<int, QByteArray> roleNames() const override;

    bool isVerticalRulersVisible() const;

signals:
    void isVerticalRulersVisibleChanged(bool isVerticalRulersVisible);

private:
    void setIsVerticalRulersVisible(bool isVerticalRulersVisible);

    enum RoleNames {
        TypeRole = Qt::UserRole + 1,
        TrackIdRole
    };

    muse::async::NotifyList<au::processing::Track> m_trackList;
    bool m_isVerticalRulersVisible = false;
};
}
