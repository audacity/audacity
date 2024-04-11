#ifndef AU_PROJECTSCENE_TRACKSLISTMODEL_H
#define AU_PROJECTSCENE_TRACKSLISTMODEL_H

#include <QAbstractListModel>

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

namespace au::projectscene {
class TracksListModel : public QAbstractListModel, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<mu::context::IGlobalContext> globalContext;

public:
    TracksListModel() = default;

    QHash<int, QByteArray> roleNames() const override;
    QVariant data(const QModelIndex& index, int role) const override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;

    Q_INVOKABLE void init();
    Q_INVOKABLE void reload();

private:
    enum Roles {
        rItemData = Qt::UserRole + 1
    };

    QVariantList m_items;
};
}

#endif // AU_PROJECTSCENE_TRACKSLISTMODEL_H
