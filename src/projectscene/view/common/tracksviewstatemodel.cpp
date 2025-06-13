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

    if (m_trackId != -1) {
        m_trackHeight = vs->trackHeight(m_trackId);
        m_trackHeight.ch.onReceive(this, [this](int h) {
            if (m_trackHeight.val == h) {
                return;
            }
            m_trackHeight.val = h;
            emit trackHeightChanged();
        });

        m_isTrackCollapsed = vs->isTrackCollapsed(m_trackId);
        m_isTrackCollapsed.ch.onReceive(this, [this](bool v) {
            if (m_isTrackCollapsed.val == v) {
                return;
            }

            m_isTrackCollapsed.val = v;
            emit isTrackCollapsedChanged();
        });
    }

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

    playbackController()->isPlayingChanged().onNotify(this, [this]() {
        emit isPlayingChanged();
    });

    recordController()->isRecordingChanged().onNotify(this, [this]() {
        emit isRecordingChanged();
    });

    playbackConfiguration()->playbackMeterStyleChanged().onNotify(this, [this]() {
        emit meterStyleChanged();
    });

    m_meterModel = new playback::PlaybackMeterModel(this);
    emit meterModelChanged();
}

void TracksViewStateModel::changeTrackHeight(int deltaY)
{
    IProjectViewStatePtr vs = viewState();
    if (vs) {
        vs->changeTrackHeight(m_trackId, deltaY);
    }
}

bool TracksViewStateModel::snapEnabled()
{
    IProjectViewStatePtr vs = viewState();
    if (vs) {
        return vs->isSnapEnabled();
    }

    return false;
}

void TracksViewStateModel::changeTracksVericalY(int deltaY)
{
    IProjectViewStatePtr vs = viewState();
    if (vs) {
        vs->changeTracksVericalY(deltaY);
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

bool TracksViewStateModel::tracksVerticalScrollLocked() const
{
    return m_tracksVerticalScrollLocked.val;
}

int TracksViewStateModel::tracksVerticalScrollPadding() const
{
    return m_tracksVerticalScrollPadding;
}

int TracksViewStateModel::trackHeight() const
{
    return m_trackHeight.val;
}

bool TracksViewStateModel::isTrackCollapsed() const
{
    return m_isTrackCollapsed.val;
}

bool TracksViewStateModel::altPressed() const
{
    return m_altPressed.val;
}

bool TracksViewStateModel::ctrlPressed() const
{
    return m_ctrlPressed.val;
}

bool TracksViewStateModel::isPlaying() const
{
    return playbackController()->isPlaying();
}

bool TracksViewStateModel::isRecording() const
{
    return recordController()->isRecording();
}

au::playback::PlaybackMeterStyle::MeterStyle TracksViewStateModel::meterStyle() const
{
    return playbackConfiguration()->playbackMeterStyle();
}

au::playback::PlaybackMeterModel* TracksViewStateModel::meterModel() const
{
    return m_meterModel;
}
