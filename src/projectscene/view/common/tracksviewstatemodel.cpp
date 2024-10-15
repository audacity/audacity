/*
* Audacity: A Digital Audio Editor
*/
#include "tracksviewstatemodel.h"

using namespace au::projectscene;
using namespace au::project;

TracksViewStateModel::TracksViewStateModel(QObject* parent)
    : QObject(parent)
{
}

IProjectViewStatePtr TracksViewStateModel::viewState() const
{
    IAudacityProjectPtr prj = globalContext()->currentProject();
    return prj ? prj->viewState() : nullptr;
}

void TracksViewStateModel::init()
{
    globalContext()->currentProjectChanged().onNotify(this, [this]() {
        init();
    });

    IProjectViewStatePtr vs = viewState();
    if (!vs) {
        return;
    }

    m_tracksVericalY = vs->tracksVericalY();
    m_tracksVericalY.ch.onReceive(this, [this](int y) {
        m_tracksVericalY.val = y;
        emit tracksVericalYChanged();
    });

    if (m_trackId != -1) {
        m_trackHeight = vs->trackHeight(m_trackId);
        m_trackHeight.ch.onReceive(this, [this](int h) {
            m_trackHeight.val = h;
            emit trackHeightChanged();
        });

        m_isTrackCollapsed = vs->isTrackCollapsed(m_trackId);
        m_isTrackCollapsed.ch.onReceive(this, [this](bool v) {
            m_isTrackCollapsed.val = v;
            emit isTrackCollapsedChanged();
        });
    }
}

void TracksViewStateModel::changeTrackHeight(int deltaY)
{
    IProjectViewStatePtr vs = viewState();
    if (vs) {
        vs->changeTrackHeight(m_trackId, deltaY);
    }
}

void TracksViewStateModel::changeTracksVericalY(int deltaY)
{
    IProjectViewStatePtr vs = viewState();
    if (vs) {
        vs->changeTracksVericalY(deltaY);
    }
}

QVariant TracksViewStateModel::trackId() const
{
    return QVariant::fromValue(m_trackId);
}

void TracksViewStateModel::setTrackId(const QVariant& _newTrackId)
{
    trackedit::TrackId newTrackId = _newTrackId.toInt();
    if (m_trackId == newTrackId) {
        return;
    }
    m_trackId = newTrackId;
    emit trackIdChanged();

    init();
}

int TracksViewStateModel::tracksVericalY() const
{
    return m_tracksVericalY.val;
}

int TracksViewStateModel::trackHeight() const
{
    return m_trackHeight.val;
}

bool TracksViewStateModel::isTrackCollapsed() const
{
    return m_isTrackCollapsed.val;
}
