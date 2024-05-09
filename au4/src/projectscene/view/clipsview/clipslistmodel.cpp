#include "clipslistmodel.h"

#include "types/projectscenetypes.h"

#include "log.h"

using namespace au::projectscene;
using namespace au::processing;

ClipsListModel::ClipsListModel(QObject* parent)
    : QAbstractListModel(parent)
{
}

void ClipsListModel::load()
{
    IF_ASSERT_FAILED(m_trackId >= 0) {
        return;
    }

    ProcessingProjectPtr prj = globalContext()->currentProcessingProject();
    if (!prj) {
        return;
    }

    connect(m_context, &TimelineContext::zoomChanged, this, &ClipsListModel::onTimelineContextChanged);
    connect(m_context, &TimelineContext::offsetChanged, this, &ClipsListModel::onTimelineContextChanged);

    beginResetModel();

    m_clipList = prj->clipList(m_trackId);

    m_clipList.onItemChanged(this, [this](const Clip& clip) {
        LOGD() << "onClipChanged, track: " << clip.key.trackId << ", index: " << clip.key.index;
        m_clipList[clip.key.index] = clip;
        QModelIndex idx = this->index(clip.key.index);
        emit dataChanged(idx, idx);
    });

    endResetModel();
}

int ClipsListModel::rowCount(const QModelIndex&) const
{
    return static_cast<int>(m_clipList.size());
}

QVariant ClipsListModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid()) {
        return QVariant();
    }

    const au::processing::Clip& clip = m_clipList.at(index.row());
    switch (role) {
    case ClipKeyRole: {
        ClipKey key;
        key.key = clip.key;
        return QVariant::fromValue(key);
    } break;
    case ClipTitleRole:
        return clip.title.toQString();
    case ClipWidthRole: {
        qint64 width = m_context->timeToPosition(clip.endTime - clip.startTime);
        return width;
    } break;
    case ClipLeftRole: {
        qint64 left = m_context->timeToPosition(clip.startTime);
        return left;
    } break;
    default:
        break;
    }

    return QVariant();
}

bool ClipsListModel::setData(const QModelIndex& index, const QVariant& value, int role)
{
    LOGD() << "" << index.row() << ", value: " << value << ", role: " << role;
    switch (role) {
    case ClipLeftRole: {
        return changeClipStartTime(index, value);
    } break;
    default:
        break;
    }
    return false;
}

void ClipsListModel::onTimelineContextChanged()
{
    for (size_t i = 0; i < m_clipList.size(); ++i) {
        QModelIndex idx = this->index(int(i));
        emit dataChanged(idx, idx, { ClipWidthRole, ClipLeftRole });
    }
}

bool ClipsListModel::changeClipStartTime(const QModelIndex& index, const QVariant& value)
{
    au::processing::Clip& clip = m_clipList[index.row()];
    double sec = m_context->positionToTime(value.toDouble());

    bool ok = processingInteraction()->changeClipStartTime(clip.key, sec);
    return ok;
}

QHash<int, QByteArray> ClipsListModel::roleNames() const
{
    static QHash<int, QByteArray> roles
    {
        { ClipKeyRole, "clipKeyData" },
        { ClipTitleRole, "clipTitleData" },
        { ClipWidthRole, "clipWidthData" },
        { ClipLeftRole, "clipLeftData" }
    };
    return roles;
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
    m_context = newContext;
}
