/*
* Audacity: A Digital Audio Editor
*/
#include "playbackuistate.h"

using namespace au::playback;

static const QString PLAYBACK_METER_POSITION_KEY("playbackToolbar/playbackMeterPosition");

void PlaybackUiState::init()
{
    uiState()->uiItemStateChanged(PLAYBACK_METER_POSITION_KEY).onNotify(this, [this]() {
        m_playbackMeterPositionChanged.notify();
    });
}

PlaybackMeterPosition::MeterPosition PlaybackUiState::playbackMeterPosition() const
{
    return uiState()->uiItemState(PLAYBACK_METER_POSITION_KEY) == "1"
           ? PlaybackMeterPosition::MeterPosition::SideBar
           : PlaybackMeterPosition::MeterPosition::TopBar;
}

void PlaybackUiState::setPlaybackMeterPosition(PlaybackMeterPosition::MeterPosition position)
{
    const QString value = position == PlaybackMeterPosition::MeterPosition::SideBar ? "1" : "0";
    if (uiState()->uiItemState(PLAYBACK_METER_POSITION_KEY) == value) {
        return;
    }
    uiState()->setUiItemState(PLAYBACK_METER_POSITION_KEY, value);
}

muse::async::Notification PlaybackUiState::playbackMeterPositionChanged() const
{
    return m_playbackMeterPositionChanged;
}
