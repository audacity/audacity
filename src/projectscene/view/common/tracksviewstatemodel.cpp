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

    m_tracksVerticalOffset = vs->tracksVerticalOffset();
    m_tracksVerticalOffset.ch.onReceive(this, [this](int y) {
        m_tracksVerticalOffset.val = y;
        emit tracksVerticalOffsetChanged();
    });

    m_tracksVerticalScrollLocked = vs->tracksVerticalScrollLocked();
    m_tracksVerticalScrollLocked.ch.onReceive(this, [this](bool locked) {
        m_tracksVerticalScrollLocked.val = locked;
        emit tracksVerticalScrollLockedChanged();
    });

    m_snapEnabled = vs->isSnapEnabled();
    vs->snap().ch.onReceive(this, [this](const Snap& snap) {
        if (m_snapEnabled != snap.enabled) {
            m_snapEnabled = snap.enabled;
            emit snapEnabledChanged();
        }
    });

    m_altPressed = vs->altPressed();
    m_altPressed.ch.onReceive(this, [this](bool v) {
        m_altPressed.val = v;
        emit altPressedChanged();
    });

    m_ctrlPressed = vs->ctrlPressed();
    m_ctrlPressed.ch.onReceive(this, [this](bool v) {
        m_ctrlPressed.val = v;
        emit ctrlPressedChanged();
    });

    m_escPressed = vs->escPressed();
    m_escPressed.ch.onReceive(this, [this](bool v) {
        m_escPressed.val = v;
        emit escPressedChanged();
    });
}

bool TracksViewStateModel::snapEnabled() const
{
    IProjectViewStatePtr vs = viewState();
    if (vs) {
        return vs->isSnapEnabled();
    }

    return false;
}

au::trackedit::TrackId TracksViewStateModel::trackAtPosition(double x, double y) const
{
    IProjectViewStatePtr vs = viewState();
    if (vs) {
        return vs->trackAtPosition(y);
    }
    return {};
}

void TracksViewStateModel::changeTracksVerticalOffset(int deltaY)
{
    IProjectViewStatePtr vs = viewState();
    if (vs) {
        vs->changeTracksVerticalOffset(deltaY);
    }
}

void TracksViewStateModel::setMouseY(double y)
{
    IProjectViewStatePtr vs = viewState();
    if (vs) {
        vs->setMousePositionY(y);
    }
}

void TracksViewStateModel::requestVerticalScrollLock()
{
    IProjectViewStatePtr vs = viewState();
    if (vs) {
        vs->setTracksVerticalScrollLocked(true);
    }
}

void TracksViewStateModel::requestVerticalScrollUnlock()
{
    IProjectViewStatePtr vs = viewState();
    if (vs) {
        vs->setTracksVerticalScrollLocked(false);
    }
}

int TracksViewStateModel::tracksVerticalOffset() const
{
    return m_tracksVerticalOffset.val;
}

bool TracksViewStateModel::tracksVerticalScrollLocked() const
{
    return m_tracksVerticalScrollLocked.val;
}

int TracksViewStateModel::tracksVerticalScrollPadding() const
{
    return m_tracksVerticalScrollPadding;
}

int TracksViewStateModel::trackHeight(trackedit::TrackId trackId) const
{
    IProjectViewStatePtr vs = viewState();
    if (vs) {
        return vs->trackHeight(trackId).val;
    }
    return {};
}

int TracksViewStateModel::trackVerticalPosition(trackedit::TrackId trackId) const
{
    IProjectViewStatePtr vs = viewState();
    if (vs) {
        return vs->trackVerticalPosition(trackId);
    }
    return {};
}

bool TracksViewStateModel::altPressed() const
{
    return m_altPressed.val;
}

bool TracksViewStateModel::ctrlPressed() const
{
    return m_ctrlPressed.val;
}

bool TracksViewStateModel::escPressed() const
{
    return m_escPressed.val;
}
