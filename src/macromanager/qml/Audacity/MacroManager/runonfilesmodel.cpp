/*
* Audacity: A Digital Audio Editor
*/
#include "runonfilesmodel.h"

#include <QCoreApplication>

using namespace au::macromanager;

RunOnFilesModel::RunOnFilesModel(QObject* parent)
    : QAbstractListModel(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

QVariant RunOnFilesModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid() || index.row() >= rowCount()) {
        return QVariant();
    }

    switch (role) {
    case FilePathRole: return m_filePaths.at(index.row());
    case StatusRole: return QVariant::fromValue(status(index.row()));
    default: return QVariant();
    }
}

int RunOnFilesModel::rowCount(const QModelIndex&) const
{
    return m_filePaths.size();
}

QHash<int, QByteArray> RunOnFilesModel::roleNames() const
{
    static const QHash<int, QByteArray> roles {
        { FilePathRole, "filePath" },
        { StatusRole, "status" }
    };

    return roles;
}

bool RunOnFilesModel::isRunning() const
{
    return runner()->isRunning();
}

void RunOnFilesModel::load(const QVariantList& files)
{
    beginResetModel();

    m_filePaths.clear();
    for (const QVariant& file : files) {
        m_filePaths.append(file.toString());
    }

    endResetModel();

    runner()->isRunningChanged().onNotify(this, [this]() {
        emit isRunningChanged();
        updateStatuses();
    });

    runner()->currentFileChanged().onNotify(this, [this]() {
        updateStatuses();
    });
}

void RunOnFilesModel::abort()
{
    runner()->abort();
}

void RunOnFilesModel::closeWindow()
{
    if (runner()->isRunning() || multiWindowsProvider()->windowCount() <= 1) {
        return;
    }

    if (const project::IAudacityProjectPtr project = globalContext()->currentProject()) {
        project->close();
        globalContext()->setCurrentProject(nullptr);
    }

    const std::shared_ptr<muse::mi::IMultiWindowsProvider> provider = multiWindowsProvider();
    const muse::modularity::ContextPtr ctx = iocContext();
    QMetaObject::invokeMethod(qApp, [provider, ctx]() {
        provider->quitWindow(ctx);
    }, Qt::QueuedConnection);
}

RunOnFilesModel::Status RunOnFilesModel::status(int row) const
{
    const std::vector<muse::Ret> results = runner()->fileResults();
    const size_t index = static_cast<size_t>(row);

    if (index < results.size()) {
        return results.at(index) ? Status::Complete : Status::Failed;
    }

    if (index == runner()->currentFileIndex() && runner()->isRunning()) {
        return Status::Processing;
    }

    return Status::Queued;
}

void RunOnFilesModel::updateStatuses()
{
    if (m_filePaths.isEmpty()) {
        return;
    }

    emit dataChanged(index(0), index(m_filePaths.size() - 1), { StatusRole });
}
