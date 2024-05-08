#pragma once

#include <QAbstractListModel>

#include <vector>

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "processing/dom/track.h"

namespace au::projectscene {
class TracksClipsModel : public QAbstractListModel
{
    Q_OBJECT

    muse::Inject<au::context::IGlobalContext> globalContext;

public:

    TracksClipsModel(QObject* parent = nullptr);

    int rowCount(const QModelIndex& parent) const override;
    QVariant data(const QModelIndex& index, int role) const override;
    QHash<int, QByteArray> roleNames() const override;

    Q_INVOKABLE void load();

private:

    enum RoleNames {
        TypeRole = Qt::UserRole + 1,
        TrackIdRole
    };

    muse::async::NotifyList<au::processing::Track> m_trackList;
};
}
