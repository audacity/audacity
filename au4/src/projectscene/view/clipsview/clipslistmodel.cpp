/*
* Audacity: A Digital Audio Editor
*/
#include "clipslistmodel.h"

#include "global/async/async.h"

#include "types/projectscenetypes.h"

#include "log.h"

using namespace au::projectscene;
using namespace au::processing;

constexpr double MOVE_MAX = 100000.0;
constexpr double MOVE_MIN = 0.0;

ClipsListModel::ClipsListModel(QObject* parent)
    : QAbstractListModel(parent)
{
}

void ClipsListModel::init()
{
    IF_ASSERT_FAILED(m_trackId >= 0) {
        return;
    }

    dispatcher()->reg(this, "clip-rename", this, &ClipsListModel::onClipRenameAction);

    onSelectedClip(selectionController()->selectedClip());
    selectionController()->clipSelected().onReceive(this, [this](const processing::ClipKey& k) {
        onSelectedClip(k);
    });

    reload();
}

void ClipsListModel::reload()
{
    ProcessingProjectPtr prj = globalContext()->currentProcessingProject();
    if (!prj) {
        return;
    }

    m_allClipList = prj->clipList(m_trackId);

    //! NOTE Clips in the track may not be in order (relative to startTime), here we arrange them.
    //! Accordingly, the indexes will change, we need to keep this in mind.
    //! To identify clips, clips have a key.
    std::sort(m_allClipList.begin(), m_allClipList.end(), [](const Clip& c1, const Clip& c2) {
        return c1.startTime < c2.startTime;
    });

    m_allClipList.onItemChanged(this, [this](const Clip& clip) {
        for (size_t i = 0; i < m_allClipList.size(); ++i) {
            if (m_allClipList.at(i).key != clip.key) {
                continue;
            }
            m_allClipList[i] = clip;
            break;
        }

        ClipListItem* item = itemByKey(clip.key);
        if (item) {
            item->setClip(clip);
        }

        updateItemsMetrics();
    });

    m_allClipList.onItemAdded(this, [this](const Clip& clip) {
        ProcessingProjectPtr prj = globalContext()->currentProcessingProject();
        muse::async::NotifyList<au::processing::Clip> newList = prj->clipList(m_trackId);
        for (size_t i = 0; i < newList.size(); ++i) {
            if (newList.at(i).key != clip.key) {
                continue;
            }

            m_allClipList.insert(m_allClipList.begin() + i, clip);

            positionViewAtClip(clip);

            update();

            break;
        }
    });

    m_allClipList.onItemRemoved(this, [this](const Clip& clip) {
        for (auto it = m_allClipList.begin(); it != m_allClipList.end(); ++it) {
            if (it->key == clip.key) {
                m_allClipList.erase(it);
                update();
                break;
            }
        }
    });

    update();
}

ClipListItem* ClipsListModel::itemByKey(const processing::ClipKey& k) const
{
    for (ClipListItem* item : std::as_const(m_clipList)) {
        if (item->clip().key != k) {
            continue;
        }
        return item;
    }
    return nullptr;
}

void ClipsListModel::update()
{
    //! NOTE First we form a new list, and then we delete old objects,
    //! otherwise there will be errors in Qml
    QList<ClipListItem*> oldList = m_clipList;

    beginResetModel();

    m_clipList.clear();

    for (const au::processing::Clip& c : m_allClipList) {
        ClipListItem* item = new ClipListItem(this);
        item->setClip(c);
        m_clipList.append(item);
    }

    updateItemsMetrics();

    onSelectedClip(selectionController()->selectedClip());

    endResetModel();

    muse::async::Async::call(this, [oldList]() {
        qDeleteAll(oldList);
    });
}

void ClipsListModel::updateItemsMetrics()
{
    //! NOTE The first step is to calculate the position and width
    for (int i = 0; i < m_clipList.size(); ++i) {
        ClipListItem* item = m_clipList[i];
        const processing::Clip& clip = item->clip();

        item->setX(m_context->timeToPosition(clip.startTime));
        item->setWidth((clip.endTime - clip.startTime) * m_context->zoom());
    }

    //! NOTE The second step is to calculate the minimum and maximum movement.
    for (int i = 0; i < m_clipList.size(); ++i) {
        ClipListItem* item = m_clipList[i];

        // MoveMaximumX
        {
            int nextIdx = i + 1;
            if (nextIdx == m_clipList.size()) {
                item->setMoveMaximumX(MOVE_MAX);
            } else {
                const ClipListItem* next = m_clipList.at(nextIdx);
                item->setMoveMaximumX(next->x() - item->width());
            }
        }

        // MoveMinimumX
        {
            if (i == 0) {
                item->setMoveMaximumX(MOVE_MIN);
            } else {
                const ClipListItem* prev = m_clipList.at(i - 1);
                item->setMoveMinimumX(prev->x() + prev->width());
            }
        }
    }
}

void ClipsListModel::positionViewAtClip(const Clip& clip)
{
    double frameStartTime = m_context->frameStartTime();
    double frameEndTime = m_context->frameEndTime();

    if (frameStartTime < clip.startTime && frameEndTime > clip.startTime) {
        return;
    }

    double OFFSET = (frameEndTime - frameStartTime) / 4.0;

    m_context->moveToFrameTime(clip.startTime - OFFSET);
}

int ClipsListModel::rowCount(const QModelIndex&) const
{
    return static_cast<int>(m_clipList.size());
}

QHash<int, QByteArray> ClipsListModel::roleNames() const
{
    static QHash<int, QByteArray> roles{ { ClipItemRole, "item" } };
    return roles;
}

QVariant ClipsListModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid()) {
        return QVariant();
    }

    if (role == ClipItemRole) {
        ClipListItem* item = m_clipList.at(index.row());
        return QVariant::fromValue(item);
    }

    return QVariant();
}

void ClipsListModel::onTimelineZoomChanged()
{
    updateItemsMetrics();
}

void ClipsListModel::onTimelineFrameTimeChanged()
{
    updateItemsMetrics();
}

void ClipsListModel::onClipRenameAction(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() > 0) {
        return;
    }

    processing::ClipKey key = args.arg<processing::ClipKey>(0);

    if (key.trackId != m_trackId) {
        return;
    }

    IF_ASSERT_FAILED(key.index < m_clipList.size()) {
        return;
    }

    emit requestClipTitleEdit(key.index);
}

bool ClipsListModel::changeClipTitle(const ClipKey& key, const QString& newTitle)
{
    bool ok = processingInteraction()->changeClipTitle(key.key, newTitle);
    return ok;
}

bool ClipsListModel::modeClip(const ClipKey& key, double x)
{
    double sec = m_context->positionToTime(x);
    bool ok = processingInteraction()->changeClipStartTime(key.key, sec);
    return ok;
}

void ClipsListModel::selectClip(const ClipKey& key)
{
    selectionController()->setSelectedClip(key.key);
}

void ClipsListModel::resetSelectedClip()
{
    selectionController()->resetSelectedClip();
}

void ClipsListModel::onSelectedClip(const processing::ClipKey& k)
{
    if (m_selectedItem && m_selectedItem->clip().key == k) {
        return;
    }

    if (m_selectedItem) {
        m_selectedItem->setSelected(false);
    }

    if (m_trackId != k.trackId) {
        m_selectedItem = nullptr;
    } else {
        m_selectedItem = itemByKey(k);
        if (m_selectedItem) {
            m_selectedItem->setSelected(true);
        }
    }
}

QVariant ClipsListModel::trackId() const
{
    return QVariant::fromValue(m_trackId);
}

void ClipsListModel::setTrackId(const QVariant& _newTrackId)
{
    processing::TrackId newTrackId = _newTrackId.toInt();
    if (m_trackId == newTrackId) {
        return;
    }
    m_trackId = newTrackId;
    emit trackIdChanged();
}

TimelineContext* ClipsListModel::timelineContext() const
{
    return m_context;
}

void ClipsListModel::setTimelineContext(TimelineContext* newContext)
{
    if (m_context == newContext) {
        return;
    }

    if (m_context) {
        disconnect(m_context, nullptr, this, nullptr);
    }

    m_context = newContext;

    if (m_context) {
        connect(m_context, &TimelineContext::zoomChanged, this, &ClipsListModel::onTimelineZoomChanged);
        connect(m_context, &TimelineContext::frameTimeChanged, this, &ClipsListModel::onTimelineFrameTimeChanged);
    }

    emit timelineContextChanged();
}
