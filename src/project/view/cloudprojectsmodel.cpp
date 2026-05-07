/*
* Audacity: A Digital Audio Editor
*/
#include "cloudprojectsmodel.h"

#include "framework/global/async/asyncable.h"
#include "framework/global/dataformatter.h"
#include "framework/global/types/datetime.h"

#include "project/types/projecttypes.h"

using namespace muse;
using namespace au::project;

namespace {
constexpr int BATCH_SIZE = 20;
}

CloudProjectsModel::CloudProjectsModel(QObject* parent)
    : AbstractItemModel(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void CloudProjectsModel::load()
{
    setState(State::Loading);
    loadItemsIfNecessary();

    connect(this, &CloudProjectsModel::desiredRowCountChanged, this, &CloudProjectsModel::loadItemsIfNecessary);
}

void CloudProjectsModel::reload()
{
    audioComService()->clearProjectListCache();
    m_isWaitingForPromise = false;
    ++m_reloadGeneration;

    beginResetModel();

    m_items.clear();
    m_totalItems = muse::nidx;
    m_desiredRowCount = 0;

    endResetModel();

    emit hasMoreChanged();
    emit desiredRowCountChanged();

    setState(State::Loading);
}

CloudProjectsModel::State CloudProjectsModel::state() const
{
    return m_state;
}

void CloudProjectsModel::setState(State state)
{
    if (m_state == state) {
        return;
    }

    m_state = state;
    emit stateChanged();
}

bool CloudProjectsModel::hasMore() const
{
    return m_totalItems == muse::nidx || m_items.size() < m_totalItems;
}

int CloudProjectsModel::desiredRowCount() const
{
    return m_desiredRowCount;
}

void CloudProjectsModel::setDesiredRowCount(int count)
{
    if (m_desiredRowCount == count) {
        return;
    }

    m_desiredRowCount = count;
    emit desiredRowCountChanged();
}

void CloudProjectsModel::loadItemsIfNecessary()
{
    if (m_isWaitingForPromise) {
        return;
    }

    if (m_state == State::Error) {
        return;
    }

    if (needsLoading()) {
        setState(State::Loading);

        m_isWaitingForPromise = true;

        const auto generation = m_reloadGeneration;

        audioComService()->downloadProjectList(BATCH_SIZE, static_cast<int>(m_items.size()) / BATCH_SIZE + 1)
        .onResolve(this, [this, generation](const au::au3cloud::ProjectList& projectList) {
            if (generation != m_reloadGeneration) {
                return;
            }

            if (!projectList.items.empty()) {
                beginInsertRows(QModelIndex(), static_cast<int>(m_items.size()),
                                static_cast<int>(m_items.size() + projectList.items.size()) - 1);

                for (const au::au3cloud::ProjectList::Item& item : projectList.items) {
                    QVariantMap obj;

                    obj[NAME_KEY] = QString::fromStdString(item.name);
                    obj[PATH_KEY] = configuration()->cloudProjectsPath()
                                    .appendingComponent(item.name)
                                    .appendingSuffix(au::project::AUP4)
                                    .toQString();
                    obj[THUMBNAIL_URL_KEY] = obj[PATH_KEY];
                    obj[IS_CLOUD_KEY] = true;
                    obj[CLOUD_ITEM_ID_KEY] = QString::fromStdString(item.id);
                    obj[TIME_SINCE_MODIFIED_KEY]
                        = DataFormatter::formatTimeSince(Date::fromQDate(QDateTime::fromSecsSinceEpoch(
                                                                             static_cast<qint64>(item.updated)).date())).toQString();
                    obj[FILE_SIZE_KEY] = (item.fileSize > 0) ? DataFormatter::formatFileSize(item.fileSize).toQString() : QString();
                    obj[IS_CREATE_NEW_KEY] = false;
                    obj[IS_NO_RESULTS_FOUND_KEY] = false;

                    m_items.push_back(obj);
                }

                endInsertRows();
            }

            m_totalItems = projectList.meta.total;
            emit hasMoreChanged();

            m_isWaitingForPromise = false;

            loadItemsIfNecessary();
        })
        .onReject(this, [this, generation](int, const std::string&) {
            if (generation != m_reloadGeneration) {
                return;
            }
            m_isWaitingForPromise = false;
            setState(State::Error);
        });
    } else {
        setState(State::Fine);
    }
}

bool CloudProjectsModel::needsLoading()
{
    return hasMore() && static_cast<int>(m_items.size()) < m_desiredRowCount;
}
