/*
* Audacity: A Digital Audio Editor
*/
#include "playbackcontroller.h"

#include "../playbacktypes.h"

using namespace muse;
using namespace au::audio;
using namespace au::playback;
using namespace muse::async;
using namespace muse::actions;

static const ActionCode PLAY_CODE("play");
static const ActionCode PAUSE_CODE("pause");
static const ActionCode STOP_CODE("stop");
static const ActionCode REWIND_START_CODE("rewind-start");
static const ActionCode REWIND_END_CODE("rewind-end");
static const ActionCode SEEK_CODE("playback-seek");
static const ActionCode LOOP_CODE("loop");
static const ActionCode LOOP_IN_CODE("loop-in");
static const ActionCode LOOP_OUT_CODE("loop-out");
static const ActionCode PAN_CODE("pan");
static const ActionCode REPEAT_CODE("repeat");

void PlaybackController::init()
{
    dispatcher()->reg(this, PLAY_CODE, this, &PlaybackController::togglePlay);
    dispatcher()->reg(this, PAUSE_CODE, this, &PlaybackController::pause);
    dispatcher()->reg(this, STOP_CODE, this, &PlaybackController::stop);
    dispatcher()->reg(this, REWIND_START_CODE, this, &PlaybackController::rewindToStart);
    dispatcher()->reg(this, REWIND_END_CODE, this, &PlaybackController::rewindToEnd);
    dispatcher()->reg(this, SEEK_CODE, this, &PlaybackController::onSeekAction);
    dispatcher()->reg(this, LOOP_CODE, this, &PlaybackController::toggleLoopPlayback);
    // dispatcher()->reg(this, LOOP_IN_CODE, [this]() { addLoopBoundary(LoopBoundaryType::LoopIn); });
    // dispatcher()->reg(this, LOOP_OUT_CODE, [this]() { addLoopBoundary(LoopBoundaryType::LoopOut); });
    dispatcher()->reg(this, REPEAT_CODE, this, &PlaybackController::togglePlayRepeats);
    dispatcher()->reg(this, PAN_CODE, this, &PlaybackController::toggleAutomaticallyPan);

    globalContext()->currentProjectChanged().onNotify(this, [this]() {
        onProjectChanged();
    });

    m_player = playback()->player();
    globalContext()->setPlayer(m_player);

    m_player->playbackStatusChanged().onReceive(this, [this](PlaybackStatus) {
        m_isPlayingChanged.notify();
    });

    m_player->playbackPositionChanged().onReceive(this, [this](const muse::secs_t& position) {
        project::IAudacityProjectPtr project = globalContext()->currentProject();
        if (!project) {
            return false;
        }

        if (totalPlayTime() == position) {
            //! NOTE: just stop, without seek
            player()->stop();
        }
    });

    recordController()->isRecordingChanged().onNotify(this, [this]() {
        m_isPlayAllowedChanged.notify();
    });
}

void PlaybackController::deinit()
{
}

IPlayerPtr PlaybackController::player() const
{
    return m_player;
}

bool PlaybackController::isPlayAllowed() const
{
    return !recordController()->isRecording();
}

Notification PlaybackController::isPlayAllowedChanged() const
{
    return m_isPlayAllowedChanged;
}

bool PlaybackController::isPlaying() const
{
    return player()->playbackStatus() == PlaybackStatus::Running;
}

bool PlaybackController::isPaused() const
{
    return player()->playbackStatus() == PlaybackStatus::Paused;
}

bool PlaybackController::isLoaded() const
{
    return m_loadingTrackCount == 0;
}

bool PlaybackController::isLoopEnabled() const
{
    NOT_IMPLEMENTED;
    return false;
}

bool PlaybackController::loopBoundariesSet() const
{
    NOT_IMPLEMENTED;
    return false;
}

Notification PlaybackController::isPlayingChanged() const
{
    return m_isPlayingChanged;
}

void PlaybackController::seek(const muse::secs_t secs)
{
    IF_ASSERT_FAILED(player()) {
        return;
    }

    player()->seek(secs);
}

void PlaybackController::reset()
{
    stop();
}

Channel<uint32_t> PlaybackController::midiTickPlayed() const
{
    return m_tickPlayed;
}

muse::async::Channel<TrackId> PlaybackController::trackAdded() const
{
    return m_trackAdded;
}

muse::async::Channel<TrackId> PlaybackController::trackRemoved() const
{
    return m_trackRemoved;
}

// ISoloMuteState::SoloMuteState PlaybackController::trackSoloMuteState(const TrackId& trackId) const
// {
// }

// void PlaybackController::setTrackSoloMuteState(const TrackId& trackId,
//                                                const ISoloMuteState::SoloMuteState& state) const
// {
// }

void PlaybackController::onProjectChanged()
{
    au::project::IAudacityProjectPtr prj = globalContext()->currentProject();
    if (prj) {
        prj->aboutCloseBegin().onNotify(this, [this]() {
            stop();
        });
    }
}

void PlaybackController::togglePlay()
{
    if (!isPlayAllowed()) {
        LOGW() << "playback not allowed";
        return;
    }

    bool isShiftPressed =  QApplication::keyboardModifiers().testFlag(Qt::ShiftModifier);
    if (isPlaying()) {
        if (isShiftPressed) {
            stop();
        } else {
            pause();
        }
    } else if (isPaused()) {
        if (isShiftPressed) {
            //! NOTE: set the current position as start position
            doSeek(m_player->playbackPosition());
            play();
        } else {
            resume();
        }
    } else {
        if (m_player->playbackPosition() == totalPlayTime()) {
            doSeek(0.0);
        }

        play();
    }
}

void PlaybackController::play()
{
    IF_ASSERT_FAILED(player()) {
        return;
    }

    if (isLoopEnabled()) {
        // msecs_t startMsecs = playbackStartMsecs();
        // seek(startMsecs);
    }

    player()->play();
}

void PlaybackController::rewindToStart()
{
    //! NOTE: In Audacity 3 we can't rewind while playing
    stop();
    IF_ASSERT_FAILED(player()) {
        return;
    }
    player()->rewind();
}

void PlaybackController::rewindToEnd()
{
    NOT_IMPLEMENTED;
    //! NOTE: In Audacity 3 we can't rewind while playing
    stop();
}

void PlaybackController::onSeekAction(const muse::actions::ActionData& args)
{
    IF_ASSERT_FAILED(args.count() > 0) {
        return;
    }

    muse::secs_t secs = args.arg<double>(0);
    bool triggerPlay = args.count() > 1 ? args.arg<bool>(1) : false;

    if (isPaused()) {
        player()->stop();
    }

    doSeek(secs, triggerPlay);

    if (triggerPlay && !isPlaying()) {
        player()->play();
    }
}

void PlaybackController::doSeek(const muse::secs_t secs, bool applyIfPlaying)
{
    player()->seek(secs, applyIfPlaying);
    m_lastPlaybackSeekTime = secs;
}

void PlaybackController::pause()
{
    IF_ASSERT_FAILED(player()) {
        return;
    }

    player()->pause();
}

void PlaybackController::stop()
{
    IF_ASSERT_FAILED(player()) {
        return;
    }

    player()->stop();

    seek(m_lastPlaybackSeekTime);
}

void PlaybackController::resume()
{
    IF_ASSERT_FAILED(player()) {
        return;
    }

    player()->resume();
}

void PlaybackController::togglePlayRepeats()
{
    NOT_IMPLEMENTED;

    // configuration()->setIsPlayRepeatsEnabled(!playRepeatsEnabled);

    notifyActionCheckedChanged(REPEAT_CODE);
}

void PlaybackController::toggleAutomaticallyPan()
{
    NOT_IMPLEMENTED;

    // configuration()->setIsAutomaticallyPanEnabled(!panEnabled);

    notifyActionCheckedChanged(PAN_CODE);
}

void PlaybackController::toggleLoopPlayback()
{
    NOT_IMPLEMENTED;
}

// void PlaybackController::addLoopBoundary(LoopBoundaryType type)
// {
// }

// void PlaybackController::addLoopBoundaryToTick(LoopBoundaryType type, int tick)
// {
// }

// void PlaybackController::updateLoop()
// {
// }

// void PlaybackController::enableLoop()
// {
// }

// void PlaybackController::disableLoop()
// {
// }

void PlaybackController::notifyActionCheckedChanged(const ActionCode& actionCode)
{
    m_actionCheckedChanged.send(actionCode);
}

void PlaybackController::subscribeOnAudioParamsChanges()
{
    NOT_IMPLEMENTED;
}

void PlaybackController::initMuteStates()
{
    NOT_IMPLEMENTED;
}

void PlaybackController::updateSoloMuteStates()
{
    NOT_IMPLEMENTED;
}

bool PlaybackController::actionChecked(const ActionCode& actionCode) const
{
    QMap<std::string, bool> isChecked {
        { LOOP_CODE, isLoopEnabled() },
        // { REPEAT_CODE, configuration()->isPlayRepeatsEnabled() },
        // { PAN_CODE, configuration()->isAutomaticallyPanEnabled() },
    };

    return isChecked[actionCode];
}

Channel<ActionCode> PlaybackController::actionCheckedChanged() const
{
    return m_actionCheckedChanged;
}

muse::secs_t PlaybackController::totalPlayTime() const
{
    project::IAudacityProjectPtr project = globalContext()->currentProject();
    if (!project) {
        return 0;
    }

    return project->trackeditProject()->totalTime();
}

Notification PlaybackController::totalPlayTimeChanged() const
{
    return m_totalPlayTimeChanged;
}

muse::Progress PlaybackController::loadingProgress() const
{
    return m_loadingProgress;
}

bool PlaybackController::canReceiveAction(const ActionCode& code) const
{
    if (globalContext()->currentProject() == nullptr) {
        return false;
    }

    if (code == PLAY_CODE || code == LOOP_CODE) {
        return !recordController()->isRecording();
    }

    if (code == REWIND_START_CODE || code == REWIND_END_CODE) {
        return !isPlaying() && !recordController()->isRecording();
    }

    return true;
}
