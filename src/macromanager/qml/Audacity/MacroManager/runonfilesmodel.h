/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QAbstractListModel>
#include <QVariantList>
#include <QtQml/qqmlregistration.h>

#include "framework/global/async/asyncable.h"

#include "framework/global/modularity/ioc.h"
#include "framework/multiwindows/imultiwindowsprovider.h"
#include "context/iglobalcontext.h"
#include "macromanager/imacrorunner.h"

namespace au::macromanager {
class RunOnFilesModel : public QAbstractListModel, public muse::Contextable, public muse::async::Asyncable
{
    Q_OBJECT
    QML_ELEMENT;

    Q_PROPERTY(bool isRunning READ isRunning NOTIFY isRunningChanged FINAL)

    muse::GlobalInject<muse::mi::IMultiWindowsProvider> multiWindowsProvider;

    muse::ContextInject<IMacroRunner> runner = { this };
    muse::ContextInject<context::IGlobalContext> globalContext = { this };

public:
    explicit RunOnFilesModel(QObject* parent = nullptr);

    enum class Status {
        Queued,
        Processing,
        Complete,
        Failed,
        Stopped
    };
    Q_ENUM(Status)

    QVariant data(const QModelIndex& index, int role) const override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QHash<int, QByteArray> roleNames() const override;

    bool isRunning() const;

    Q_INVOKABLE void load(const QVariantList& files);
    Q_INVOKABLE void abort();
    Q_INVOKABLE void closeWindow();

signals:
    void isRunningChanged();

private:
    enum Roles {
        FilePathRole = Qt::UserRole + 1,
        StatusRole
    };

    Status status(int row) const;
    void updateStatuses();

    QStringList m_filePaths;
};
}
