/*
* Audacity: A Digital Audio Editor
*/
#include "clipslistmodel.h"

#include "global/containers.h"

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

    if (m_context) {
        connect(m_context, &TimelineContext::zoomChanged, this, &ClipsListModel::onTimelineContextValuesChanged);
        connect(m_context, &TimelineContext::frameTimeChanged, this, &ClipsListModel::onTimelineContextValuesChanged);
    }

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

    beginResetModel();

    m_clipList = prj->clipList(m_trackId);

    //! NOTE Clips in the track may not be in order (relative to startTime), here we arrange them.
    //! Accordingly, the indexes will change, we need to keep this in mind.
    //! To identify clips, clips have a key.
    std::sort(m_clipList.begin(), m_clipList.end(), [](const Clip& c1, const Clip& c2) {
        return c1.startTime < c2.startTime;
    });

    m_clipList.onItemChanged(this, [this](const Clip& clip) {
        for (size_t i = 0; i < m_clipList.size(); ++i) {
            if (m_clipList.at(i).key != clip.key) {
                continue;
            }
            m_clipList[i] = clip;

            // update current (all roles)
            QModelIndex idx = this->index(i);
            emit dataChanged(idx, idx);

            // update prev
            if (i > 0) {
                QModelIndex prev = this->index(i - 1);
                emit dataChanged(prev, prev, { ClipMoveMaximumXRole, ClipMoveMinimumXRole });
            }

            // update next
            if (i < (m_clipList.size() - 1)) {
                QModelIndex next = this->index(i + 1);
                emit dataChanged(next, next, { ClipMoveMaximumXRole, ClipMoveMinimumXRole });
            }
        }
    });

    endResetModel();
}

int ClipsListModel::rowCount(const QModelIndex&) const
{
    return static_cast<int>(m_clipList.size());
}

QHash<int, QByteArray> ClipsListModel::roleNames() const
{
    static QHash<int, QByteArray> roles
    {
        { ClipKeyRole, "clipKey" },
        { ClipTitleRole, "clipTitle" },
        { ClipColorRole, "clipColor" },
        { ClipWidthRole, "clipWidth" },
        { ClipLeftRole, "clipLeft" },
        { ClipMoveMaximumXRole, "clipMoveMaximumX" },
        { ClipMoveMinimumXRole, "clipMoveMinimumX" },
    };
    return roles;
}

QVariant ClipsListModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid()) {
        return QVariant();
    }

    auto clipWidth = [this](const au::processing::Clip& clip) {
        return (clip.endTime - clip.startTime) * m_context->zoom();
    };

    auto clipLeft = [this](const au::processing::Clip& clip) {
        return m_context->timeToPosition(clip.startTime);
    };

    const au::processing::Clip& clip = m_clipList.at(index.row());
    switch (role) {
    case ClipKeyRole: {
        ClipKey key;
        key.key = clip.key;
        return QVariant::fromValue(key);
    } break;
    case ClipTitleRole:
        return clip.title.toQString();
    case ClipColorRole:
        return clip.color.toQColor();
    case ClipWidthRole:
        return clipWidth(clip);
    case ClipLeftRole:
        return clipLeft(clip);
    case ClipMoveMaximumXRole: {
        size_t nextIdx = index.row() + 1;
        if (nextIdx == m_clipList.size()) {
            return MOVE_MAX;
        }
        const au::processing::Clip& nextClip = m_clipList.at(nextIdx);
        double nextLeft = clipLeft(nextClip);
        double clipW = clipWidth(clip);
        return nextLeft - clipW;
    } break;
    case ClipMoveMinimumXRole: {
        if (index.row() == 0) {
            return MOVE_MIN;
        }
        const au::processing::Clip& prevClip = m_clipList.at(index.row() - 1);
        double prevL = clipLeft(prevClip);
        double prevW = clipWidth(prevClip);
        return prevL + prevW;
    } break;
    default:
        break;
    }

    return QVariant();
}

bool ClipsListModel::setData(const QModelIndex& index, const QVariant& value, int role)
{
    //LOGD() << "" << index.row() << ", value: " << value << ", role: " << role;
    switch (role) {
    case ClipLeftRole: {
        return changeClipStartTime(index, value);
    } break;
    case ClipTitleRole: {
        return changeClipTitle(index, value);
    } break;
    default:
        break;
    }
    return false;
}

void ClipsListModel::onTimelineContextValuesChanged()
{
    // LOGDA() << "zoom: " << m_context->zoom()
    //         << " frameStartTime: " << m_context->frameStartTime()
    //         << " frameEndTime: " << m_context->frameEndTime();

    for (size_t i = 0; i < m_clipList.size(); ++i) {
        QModelIndex idx = this->index(int(i));
        emit dataChanged(idx, idx, { ClipWidthRole, ClipLeftRole, ClipMoveMaximumXRole, ClipMoveMinimumXRole });
    }
}

bool ClipsListModel::changeClipStartTime(const QModelIndex& index, const QVariant& value)
{
    au::processing::Clip& clip = m_clipList[index.row()];

    double sec = m_context->positionToTime(value.toDouble());

    bool ok = processingInteraction()->changeClipStartTime(clip.key, sec);
    return ok;
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

bool ClipsListModel::changeClipTitle(const QModelIndex& index, const QVariant& value)
{
    au::processing::Clip& clip = m_clipList[index.row()];
    muse::String newTitle = value.toString();
    if (clip.title == newTitle) {
        return false;
    }

    bool ok = processingInteraction()->changeClipTitle(clip.key, newTitle);
    return ok;
}

void ClipsListModel::selectClip(int index)
{
    selectionController()->setSelectedClip(processing::ClipKey(m_trackId, index));
}

void ClipsListModel::resetSelectedClip()
{
    selectionController()->resetSelectedClip();
}

void ClipsListModel::onSelectedClip(const processing::ClipKey& k)
{
    if (m_trackId != k.trackId) {
        setSelectedClipIdx(-1);
    } else {
        setSelectedClipIdx(k.index);
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
    m_context = newContext;
    emit timelineContextChanged();
}

int ClipsListModel::selectedClipIdx() const
{
    return m_selectedClipIdx;
}

void ClipsListModel::setSelectedClipIdx(int newSelectedClipIdx)
{
    if (m_selectedClipIdx == newSelectedClipIdx) {
        return;
    }
    m_selectedClipIdx = newSelectedClipIdx;
    emit selectedClipIdxChanged();
}
