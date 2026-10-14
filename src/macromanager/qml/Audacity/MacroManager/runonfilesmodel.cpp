/*
* Audacity: A Digital Audio Editor
*/
#include "runonfilesmodel.h"

#include "framework/global/io/path.h"

using namespace au::macromanager;
using namespace muse;

RunOnFilesModel::RunOnFilesModel(QObject* parent)
    : QAbstractListModel(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

QVariant RunOnFilesModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid() || index.row() >= rowCount()) {
        return QVariant();
    }

    const Item& item = m_items.at(index.row());

    switch (role) {
    case FileNameRole: return item.fileName;
    case ProgressRole: return item.progress;
    default: return QVariant();
    }
}

int RunOnFilesModel::rowCount(const QModelIndex&) const
{
    return m_items.size();
}

QHash<int, QByteArray> RunOnFilesModel::roleNames() const
{
    static const QHash<int, QByteArray> roles {
        { FileNameRole, "fileName" },
        { ProgressRole, "progress" }
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

    m_items.clear();
    for (const QVariant& file : files) {
        m_items.append({ io::filename(io::path_t(file.toString())).toQString(), 0.0 });
    }

    endResetModel();

    runner()->isRunningChanged().onNotify(this, [this]() {
        emit isRunningChanged();
    });

    runner()->fileProgress().onReceive(this, [this](const MacroFileProgress& progress) {
        if (progress.fileIndex >= static_cast<size_t>(m_items.size()) || progress.totalOperations == 0) {
            return;
        }

        m_items[progress.fileIndex].progress = static_cast<double>(progress.completedOperations) / progress.totalOperations;

        const QModelIndex idx = index(static_cast<int>(progress.fileIndex));
        emit dataChanged(idx, idx, { ProgressRole });
    });
}

void RunOnFilesModel::abort()
{
    runner()->abort();
}
