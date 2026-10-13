/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QAbstractListModel>
#include <QtQml/qqmlregistration.h>

#include "modularity/ioc.h"
#include "interactive/iinteractive.h"

namespace au::macromanager {
class MacroManagerModel : public QAbstractListModel, public muse::Contextable
{
    Q_OBJECT
    QML_ELEMENT;

    Q_PROPERTY(bool isEmpty READ isEmpty NOTIFY isEmptyChanged FINAL)

    muse::ContextInject<muse::IInteractive> interactive = { this };

public:
    explicit MacroManagerModel(QObject* parent = nullptr);

    QVariant data(const QModelIndex& index, int role) const override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QHash<int, QByteArray> roleNames() const override;

    bool isEmpty() const;

    Q_INVOKABLE void init();
    Q_INVOKABLE void newMacro();
    Q_INVOKABLE void run(int row);

signals:
    void isEmptyChanged();

private:
    enum Roles {
        TitleRole = Qt::UserRole + 1
    };

    struct Item {
        QString title;
    };

    QList<Item> m_items;
};
}
