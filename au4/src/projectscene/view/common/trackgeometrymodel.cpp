#include "trackgeometrymodel.h"

using namespace au::projectscene;
using namespace au::project;

TrackViewStateModel::TrackViewStateModel(QObject* parent)
    : QObject(parent)
{
}

IProjectViewStatePtr TrackViewStateModel::viewState() const
{
    IAudacityProjectPtr prj = globalContext()->currentProject();
    return prj ? prj->viewState() : nullptr;
}

void TrackViewStateModel::setup(processing::TrackId trackId)
{
    IProjectViewStatePtr vs = viewState();
    if (!vs) {
        return;
    }

    m_trackHeight = vs->trackHeight(trackId);
    m_trackHeight.ch.onReceive(this, [this](int h) {
        m_trackHeight.val = h;
        emit trackHeightChanged();
    });
}

void TrackViewStateModel::changeTrackHeight(int deltaY)
{
    IProjectViewStatePtr vs = viewState();
    if (vs) {
        vs->changeTrackHeight(m_trackId, deltaY);
    }
}

QVariant TrackViewStateModel::trackId() const
{
    return QVariant::fromValue(m_trackId);
}

void TrackViewStateModel::setTrackId(const QVariant& _newTrackId)
{
    processing::TrackId newTrackId = _newTrackId.toInt();
    if (m_trackId == newTrackId) {
        return;
    }
    m_trackId = newTrackId;
    emit trackIdChanged();

    setup(m_trackId);
}

int TrackViewStateModel::trackHeight() const
{
    return m_trackHeight.val;
}
