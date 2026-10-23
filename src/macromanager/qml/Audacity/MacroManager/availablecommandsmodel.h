/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QAbstractListModel>
#include <QtQml/qqmlregistration.h>

#include "framework/global/async/asyncable.h"

#include "framework/global/modularity/ioc.h"
#include "framework/rcommand/icommandsregister.h"
#include "macromanager/imacrosrepository.h"

namespace au::macromanager {
class AvailableCommandsModel : public QAbstractListModel, public muse::async::Asyncable
{
    Q_OBJECT
    QML_ELEMENT;

    Q_PROPERTY(QString searchText READ searchText NOTIFY searchTextChanged FINAL)

    muse::GlobalInject<muse::rcommand::ICommandsRegister> commandsRegister;
    muse::GlobalInject<IMacrosRepository> repository;

public:
    explicit AvailableCommandsModel(QObject* parent = nullptr);

    QVariant data(const QModelIndex& index, int role) const override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QHash<int, QByteArray> roleNames() const override;

    QString searchText() const;

    Q_INVOKABLE void load(const QString& editedMacroName);
    Q_INVOKABLE void setSearchText(const QString& text);
    Q_INVOKABLE QVariantMap itemAt(int row) const;

signals:
    void searchTextChanged();

private:
    enum Roles {
        CodeRole = Qt::UserRole + 1,
        TitleRole,
        IsMacroRole
    };

    struct Item {
        QString code;
        QString title;
        bool isMacro = false;
    };

    void reload();
    void appendCommands();
    void appendMacros();
    bool matchesSearch(const Item& item) const;

    QString m_editedMacroName;
    QString m_searchText;
    QList<Item> m_allItems;
    QList<Item> m_items;
};
}
