#ifndef AU_PROJECTSCENE_TRACKSLISTMODEL_H
#define AU_PROJECTSCENE_TRACKSLISTMODEL_H

#include <QAbstractListModel>

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

namespace au::projectscene {
class TracksListModel : public QAbstractListModel
{
    Q_OBJECT;

    mu::Inject<mu::context::IGlobalContext> globalContext;

public:
    TracksListModel() = default;

    QHash<int, QByteArray> roleNames() const override;
    QVariant data(const QModelIndex& index, int role) const override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;

    Q_INVOKABLE void load();

private:
    enum Roles {
        TitleRole = Qt::UserRole + 1,
    };
};
}

#endif // AU_PROJECTSCENE_TRACKSLISTMODEL_H
