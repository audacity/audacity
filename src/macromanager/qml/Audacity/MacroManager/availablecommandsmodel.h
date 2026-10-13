/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QAbstractListModel>
#include <QtQml/qqmlregistration.h>

#include "framework/global/async/asyncable.h"
#include "modularity/ioc.h"
#include "framework/rcommand/icommandsregister.h"

namespace au::macromanager {
class AvailableCommandsModel : public QAbstractListModel, public muse::async::Asyncable
{
    Q_OBJECT
    QML_ELEMENT;

    Q_PROPERTY(QString searchText READ searchText NOTIFY searchTextChanged FINAL)

    muse::GlobalInject<muse::rcommand::ICommandsRegister> commandsRegister;

public:
    explicit AvailableCommandsModel(QObject* parent = nullptr);

    QVariant data(const QModelIndex& index, int role) const override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QHash<int, QByteArray> roleNames() const override;

    QString searchText() const;

    Q_INVOKABLE void load();
    Q_INVOKABLE void setSearchText(const QString& text);

signals:
    void searchTextChanged();

private:
    enum Roles {
        CodeRole = Qt::UserRole + 1,
        TitleRole
    };

    struct Item {
        QString code;
        QString title;
    };

    void reload();
    bool matchesSearch(const Item& item) const;

    QString m_searchText;
    QList<Item> m_allItems;
    QList<Item> m_items;
};
}
