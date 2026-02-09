/*
* Audacity: A Digital Audio Editor
*/
#include "tracksviewstatemodel.h"

using namespace au::projectscene;
using namespace au::project;

TracksViewStateModel::TracksViewStateModel(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
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
    }, muse::async::Asyncable::Mode::SetReplace);

    IProjectViewStatePtr vs = viewState();
    if (!vs) {
        return;
    }

    m_tracksVerticalOffset = vs->tracksVerticalOffset();
    m_tracksVerticalOffset.ch.onReceive(this, [this](int y) {
        m_tracksVerticalOffset.val = y;
        emit tracksVerticalOffsetChanged();
    }, muse::async::Asyncable::Mode::SetReplace);

    m_tracksVerticalScrollLocked = vs->tracksVerticalScrollLocked();
    m_tracksVerticalScrollLocked.ch.onReceive(this, [this](bool locked) {
        m_tracksVerticalScrollLocked.val = locked;
        emit tracksVerticalScrollLockedChanged();
    }, muse::async::Asyncable::Mode::SetReplace);

    m_snapEnabled = vs->isSnapEnabled();
    vs->snap().ch.onReceive(this, [this](const Snap& snap) {
        if (m_snapEnabled != snap.enabled) {
            m_snapEnabled = snap.enabled;
            emit snapEnabledChanged();
        }
    }, muse::async::Asyncable::Mode::SetReplace);

    m_altPressed = vs->altPressed();
    m_altPressed.ch.onReceive(this, [this](bool v) {
        m_altPressed.val = v;
        emit altPressedChanged();
    }, muse::async::Asyncable::Mode::SetReplace);

    m_ctrlPressed = vs->ctrlPressed();
    m_ctrlPressed.ch.onReceive(this, [this](bool v) {
        m_ctrlPressed.val = v;
        emit ctrlPressedChanged();
    }, muse::async::Asyncable::Mode::SetReplace);
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
    UNUSED(x);
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

void TracksViewStateModel::insureVerticallyVisible(int viewContentY, int viewHeight, int itemY, int itemHeight)
{
    static constexpr int PADDING = 10;

    int itemTop = itemY;
    int itemBottom = itemY + itemHeight;

    int viewTop = viewContentY;
    int viewBottom = viewContentY + viewHeight;

    int delta = 0;

    if (itemTop >= (viewTop + PADDING) && itemBottom <= (viewBottom - PADDING)) {
        delta = 0;
    } else if (itemTop < (viewTop + PADDING)) {
        delta = itemTop - (viewTop + PADDING);
    } else if (itemBottom > (viewBottom - PADDING)) {
        delta = itemBottom - (viewBottom - PADDING);
    }

    int newOffset = viewContentY + delta;
    if (newOffset < 0) {
        changeTracksVerticalOffset(0);
    } else {
        changeTracksVerticalOffset(newOffset);
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
