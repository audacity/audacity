/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QAbstractListModel>
#include <QVariantList>
#include <QtQml/qqmlregistration.h>

#include "framework/global/async/asyncable.h"

#include "framework/global/modularity/ioc.h"
#include "macromanager/imacrorunner.h"

namespace au::macromanager {
class RunOnFilesModel : public QAbstractListModel, public muse::Contextable, public muse::async::Asyncable
{
    Q_OBJECT
    QML_ELEMENT;

    Q_PROPERTY(bool isRunning READ isRunning NOTIFY isRunningChanged FINAL)

    muse::ContextInject<IMacroRunner> runner = { this };

public:
    explicit RunOnFilesModel(QObject* parent = nullptr);

    QVariant data(const QModelIndex& index, int role) const override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QHash<int, QByteArray> roleNames() const override;

    bool isRunning() const;

    Q_INVOKABLE void load(const QVariantList& files);
    Q_INVOKABLE void abort();

signals:
    void isRunningChanged();

private:
    enum Roles {
        FileNameRole = Qt::UserRole + 1,
        ProgressRole
    };

    struct Item {
        QString fileName;
        double progress = 0.0;
    };

    QList<Item> m_items;
};
}
