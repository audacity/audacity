#include "clipslistmodel.h"

#include "clipkey.h"

#include "log.h"

using namespace au::projectscene;

ClipsListModel::ClipsListModel(QObject* parent)
    : QAbstractListModel(parent)
{
}

void ClipsListModel::load()
{
    IF_ASSERT_FAILED(m_trackId >= 0) {
        return;
    }

    au::processing::ProcessingProjectPtr prj = globalContext()->currentProcessingProject();
    if (!prj) {
        return;
    }

    beginResetModel();

    m_clipList = prj->clipList(m_trackId);

    //! TODO Subscribe

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
        key.au3WaveTrackPtr = clip.au3WaveTrackPtr;
        key.au3WaveClipPtr = clip.au3WaveClipPtr;
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
