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

    if (m_context) {
        connect(m_context, &TimelineContext::zoomChanged, this, &ClipsListModel::onTimelineContextValuesChanged);
        connect(m_context, &TimelineContext::frameTimeChanged, this, &ClipsListModel::onTimelineContextValuesChanged);
    }

    muse::ValCh<processing::ClipKey> selectedClip = processingInteraction()->selectedClip();
    selectedClip.ch.onReceive(this, [this](const processing::ClipKey& k) {
        onSelectedClip(k);
    });
    onSelectedClip(selectedClip.val);

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

QHash<int, QByteArray> ClipsListModel::roleNames() const
{
    static QHash<int, QByteArray> roles
    {
        { ClipKeyRole, "clipKey" },
        { ClipTitleRole, "clipTitle" },
        { ClipColorRole, "clipColor" },
        { ClipWidthRole, "clipWidth" },
        { ClipLeftRole, "clipLeft" }
    };
    return roles;
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
    case ClipColorRole:
        return clip.color.toQColor();
    case ClipWidthRole: {
        qint64 width = (clip.endTime - clip.startTime) * m_context->zoom();
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
    //LOGD() << "" << index.row() << ", value: " << value << ", role: " << role;
    switch (role) {
    case ClipLeftRole: {
        return changeClipStartTime(index, value);
    } break;
    case ClipTitleRole: {
        LOGDA() << value.toString();
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

void ClipsListModel::selectClip(int index)
{
    processingInteraction()->selectClip(processing::ClipKey(m_trackId, index));
}

void ClipsListModel::onSelectedClip(const processing::ClipKey& k)
{
    if (m_trackId != k.trackId) {
        setSelectedClipIdx(-1);
    } else {
        setSelectedClipIdx(k.index);
    }
}

void ClipsListModel::onSelected(double x1, double x2)
{
    onSelectedTime(m_context->positionToTime(x1), m_context->positionToTime(x2));
}

void ClipsListModel::onSelectedTime(double startTime, double endTime)
{
    if (startTime > endTime) {
        std::swap(startTime, endTime);
    }

    LOGDA() << "m_trackId: " << m_trackId << ", startTime: " << startTime << ", endTime: " << endTime;
    NOT_IMPLEMENTED;
}

void ClipsListModel::resetSelection()
{
    NOT_IMPLEMENTED;
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
