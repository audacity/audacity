#include "trackgeometrymodel.h"

using namespace au::projectscene;

TrackGeometryModel::TrackGeometryModel(QObject* parent)
    : QObject(parent)
{
}

void TrackGeometryModel::setup(processing::TrackId trackId)
{
    m_trackHeight = geometryController()->trackHeight(trackId);
    m_trackHeight.ch.onReceive(this, [this](int h) {
        m_trackHeight.val = h;
        emit trackHeightChanged();
    });
}

void TrackGeometryModel::changeTrackHeight(int deltaY)
{
    geometryController()->changeTrackHeight(m_trackId, deltaY);
}

QVariant TrackGeometryModel::trackId() const
{
    return QVariant::fromValue(m_trackId);
}

void TrackGeometryModel::setTrackId(const QVariant& _newTrackId)
{
    processing::TrackId newTrackId = _newTrackId.toInt();
    if (m_trackId == newTrackId) {
        return;
    }
    m_trackId = newTrackId;
    emit trackIdChanged();

    setup(m_trackId);
}

int TrackGeometryModel::trackHeight() const
{
    return m_trackHeight.val;
}
