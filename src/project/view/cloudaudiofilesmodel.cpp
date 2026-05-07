/*
* Audacity: A Digital Audio Editor
*/
#include "cloudaudiofilesmodel.h"

#include "framework/global/async/asyncable.h"
#include "framework/global/dataformatter.h"
#include "framework/global/types/datetime.h"

using namespace muse;
using namespace au::project;

namespace {
constexpr int BATCH_SIZE = 20;
}

CloudAudioFilesModel::CloudAudioFilesModel(QObject* parent)
    : AbstractItemModel(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void CloudAudioFilesModel::load()
{
    setState(State::Loading);
    loadItemsIfNecessary();

    connect(this, &CloudAudioFilesModel::desiredRowCountChanged, this, &CloudAudioFilesModel::loadItemsIfNecessary);

    audioComService()->audioThumbnailFileUpdated().onReceive(this, [this](const std::string& audioId, const muse::io::path_t& path) {
        for (int i = 0; i < static_cast<int>(m_items.size()); ++i) {
            if (m_items[i][CLOUD_ITEM_ID_KEY].toString() == QString::fromStdString(audioId)) {
                m_items[i][THUMBNAIL_URL_KEY] = path.toQString();
                emit dataChanged(index(i), index(i));
                break;
            }
        }
    });
}

void CloudAudioFilesModel::reload()
{
    audioComService()->clearAudioListCache();
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

CloudAudioFilesModel::State CloudAudioFilesModel::state() const
{
    return m_state;
}

void CloudAudioFilesModel::setState(State state)
{
    if (m_state == state) {
        return;
    }

    m_state = state;
    emit stateChanged();
}

bool CloudAudioFilesModel::hasMore() const
{
    return m_totalItems == muse::nidx || m_items.size() < m_totalItems;
}

int CloudAudioFilesModel::desiredRowCount() const
{
    return m_desiredRowCount;
}

void CloudAudioFilesModel::setDesiredRowCount(int count)
{
    if (m_desiredRowCount == count) {
        return;
    }

    m_desiredRowCount = count;
    emit desiredRowCountChanged();
}

void CloudAudioFilesModel::loadItemsIfNecessary()
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

        audioComService()->downloadAudioList(BATCH_SIZE, static_cast<int>(m_items.size()) / BATCH_SIZE + 1)
        .onResolve(this, [this, generation](const au::au3cloud::AudioList& audioFileList) {
            if (generation != m_reloadGeneration) {
                return;
            }

            if (!audioFileList.items.empty()) {
                beginInsertRows(QModelIndex(), static_cast<int>(m_items.size()),
                                static_cast<int>(m_items.size() + audioFileList.items.size()) - 1);

                for (const au::au3cloud::AudioList::Item& item : audioFileList.items) {
                    QVariantMap obj;

                    obj[NAME_KEY] = QString::fromStdString(item.title);
                    obj[SLUG_KEY] = QString::fromStdString(item.slug);
                    obj[PATH_KEY] = ""; //configuration()->cloudProjectPath(item.id).toQString();
                    obj[THUMBNAIL_URL_KEY] = fileSystem()->exists(item.waveformPath) ? item.waveformPath.toQString() : QString();
                    obj[IS_CLOUD_KEY] = true;
                    obj[CLOUD_ITEM_ID_KEY] = QString::fromStdString(item.id);
                    obj[TIME_SINCE_MODIFIED_KEY]
                        = DataFormatter::formatTimeSince(Date::fromQDate(QDateTime::fromSecsSinceEpoch(
                                                                             static_cast<qint64>(item.created)).date())).toQString();
                    obj[FILE_SIZE_KEY] = (item.fileSize > 0) ? DataFormatter::formatFileSize(item.fileSize).toQString() : QString();
                    obj[DURATION_KEY] = (item.duration > 0) ? QTime(0, 0).addMSecs(static_cast<int>(item.duration)).toString(
                        "hh:mm:ss") : QString();
                    obj[IS_CREATE_NEW_KEY] = false;
                    obj[IS_NO_RESULTS_FOUND_KEY] = false;

                    m_items.push_back(obj);
                }

                endInsertRows();
            }

            m_totalItems = audioFileList.meta.total;
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

bool CloudAudioFilesModel::needsLoading()
{
    return hasMore() && static_cast<int>(m_items.size()) < m_desiredRowCount;
}
