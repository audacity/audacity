#pragma once

#include <QAbstractListModel>

#include <vector>

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "processing/dom/track.h"

namespace au::projectscene {
class TracksListClipsModel : public QAbstractListModel
{
    Q_OBJECT

    muse::Inject<au::context::IGlobalContext> globalContext;

public:

    TracksListClipsModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();

    int rowCount(const QModelIndex& parent) const override;
    QVariant data(const QModelIndex& index, int role) const override;
    QHash<int, QByteArray> roleNames() const override;

private:

    enum RoleNames {
        TypeRole = Qt::UserRole + 1,
        TrackIdRole
    };

    muse::async::NotifyList<au::processing::Track> m_trackList;
};
}
