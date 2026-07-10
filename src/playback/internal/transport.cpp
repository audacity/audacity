/*
 * Audacity: A Digital Audio Editor
 */
#include "transport.h"

#include "framework/global/realfn.h"
#include "framework/global/translation.h"
#include "framework/global/log.h"

#include <QVariant>

static const muse::secs_t TIME_EPS = muse::secs_t(1 / 1000.0);

namespace au::playback {
void Transport::init()
{
    player()->playbackPositionChanged().onReceive(this, [this](const muse::secs_t&) {
        onPlaybackPositionChanged();
    });

    player()->loopRegionChanged().onNotify(this, [this]() {
        if (playbackConfiguration()->selectionFollowsLoopRegion()) {
            setSelectionToLoop();
        }
    });

    recordController()->isRecordingChanged().onNotify(this, [this]() {
        m_isPlayAllowedChanged.notify();
    }, Mode::SetReplace);

    selectionController()->clipsSelected().onReceive(this, [this](const trackedit::ClipKeyList& clipKeyList) {
        if (clipKeyList.empty()) {
            return;
        }
        if (!isPlaying()) {
            stop();
            PlaybackRegion selectionRegion = selectionPlaybackRegion();
            if (selectionRegion.isValid()) {
                doChangePlaybackRegion(selectionRegion);
            }
            doSeek(lastPlaybackSeekTime(), false);
        }
    });

    selectionController()->labelsSelected().onReceive(this, [this](const trackedit::LabelKeyList& labelKeyList) {
        if (labelKeyList.empty()) {
            return;
        }
        if (!isPlaying()) {
            stop();
            PlaybackRegion selectionRegion = selectionPlaybackRegion();
            if (selectionRegion.isValid()) {
                doChangePlaybackRegion(selectionRegion);
            }
            auto labelStartTime = selectionController()->selectedLabelStartTime();
            auto labelEndTime = selectionController()->selectedLabelEndTime();
            if (labelStartTime.has_value()) {
                doSeek(labelStartTime.value(), false);
                m_lastPlaybackRegion = { labelStartTime.value_or(0.0), labelEndTime.value_or(0.0) };
            }
        }
    });

    globalContext()->currentProjectChanged().onNotify(this, [this]() {
        onProjectChanged();
    });
}

bool Transport::isPlaying() const
{
    return player()->isPlaying();
}

bool Transport::isPaused() const
{
    return player()->isPaused();
}

bool Transport::isStopped() const
{
    return player()->isStopped();
}

bool Transport::isPlayAllowed() const
{
    return !recordController()->isRecording();
}

muse::async::Notification Transport::isPlayAllowedChanged() const
{
    return m_isPlayAllowedChanged;
}

muse::secs_t Transport::lastPlaybackSeekTime() const
{
    return m_lastPlaybackSeekTime;
}

void Transport::setLastPlaybackSeekTime(muse::secs_t secs)
{
    if (muse::RealIsEqual(lastPlaybackSeekTime(), secs)) {
        return;
    }

    m_lastPlaybackSeekTime = secs;
    m_pauseShouldStopPlayback = isPlaying();
    m_lastPlaybackSeekTimeChanged.notify();
}

muse::async::Notification Transport::lastPlaybackSeekTimeChanged() const
{
    return m_lastPlaybackSeekTimeChanged;
}

muse::secs_t Transport::totalPlayTime() const
{
    au::project::IAudacityProjectPtr project = globalContext()->currentProject();
    if (!project) {
        return 0;
    }

    return project->trackeditProject()->totalTime();
}

void Transport::togglePlay(bool ignoreSelection)
{
    if (!isPlayAllowed()) {
        LOGW() << "playback not allowed";
        return;
    }

    if (isPlaying()) {
        if (ignoreSelection) {
            stopSeekAndUpdatePlaybackRegion();
        } else {
            pause();
        }
    } else if (isPaused()) {
        if (isSelectionPlaybackRegionChanged()) {
            //! NOTE: just stop, without seek
            stop();
            doPlay(false);
        } else if (ignoreSelection) {
            //! NOTE: set the current position as start position
            doSeek(player()->playbackPosition(), false);
            doPlay(true /* ignoreSelection */);
        } else {
            player()->resume();
        }
    } else {
        if (isPlaybackPositionOnTheEndOfProject() || isPlaybackPositionOnTheEndOfPlaybackRegion()) {
            doSeek(0.0, false);
        }

        doPlay(ignoreSelection);
    }
}

void Transport::pause()
{
    if (m_pauseShouldStopPlayback && isPlaying()) {
        m_pauseShouldStopPlayback = false;
        stopSeekAndUpdatePlaybackRegion();
        return;
    }

    player()->pause();
}

void Transport::stop()
{
    m_pauseShouldStopPlayback = false;
    m_pausedResumePos.reset();
    player()->stop();
}

void Transport::doPlay(bool ignoreSelection)
{
    if (m_pausedResumePos.has_value()) {
        // Resuming a stream that a device change tore down while paused: start
        // the stream at the pause position. The play region, seek anchor and the
        // rest of the session state are deliberately untouched.
        const muse::secs_t pos = *m_pausedResumePos;
        m_pausedResumePos.reset();
        player()->play(pos);
        return;
    }

    if (!ignoreSelection) {
        PlaybackRegion selectionRegion = selectionPlaybackRegion();
        if (selectionRegion.isValid()) {
            doChangePlaybackRegion(selectionRegion);
        } else {
            //! NOTE: no selection — fall back to the user's cursor (lastPlaybackSeekTime)
            //! and the project end.
            const muse::secs_t end = totalPlayTime();
            const muse::secs_t start = lastPlaybackSeekTime();
            if (end > start) {
                doChangePlaybackRegion({ start, end });
            } else {
                LOGW() << "playback region is not valid";
                updatePlaybackRegion();
            }
        }
    } else {
        doChangePlaybackRegion({});
        doSeek(lastPlaybackSeekTime(), false);
    }

    if (!isPlaybackStartPositionValid()) {
        return;
    }

    player()->play();
}

void Transport::rewindToStart()
{
    //! NOTE: In Audacity 3 we can't rewind while playing
    stopSeekAndUpdatePlaybackRegion();

    doSeek(0.0, false);

    selectionController()->resetTimeSelection();
}

void Transport::rewindToEnd()
{
    //! NOTE: In Audacity 3 we can't rewind while playing
    setLastPlaybackSeekTime(totalPlayTime());
    m_lastPlaybackRegion = { totalPlayTime(), totalPlayTime() };
    stopSeekAndUpdatePlaybackRegion();

    selectionController()->resetTimeSelection();
}

void Transport::seekTo(const muse::secs_t secs, bool triggerPlay)
{
    if (recordController()->isRecording()) {
        return;
    }

    const bool isSeekStartPositionValid = isSeekPositionValid(secs);

    if (isPaused() || (!isSeekStartPositionValid)) {
        stop();
    }

    doSeek(secs, triggerPlay);

    if (triggerPlay) {
        if (isPlaying()) {
            return;
        }

        if (!isSeekStartPositionValid) {
            return;
        }

        player()->play();
    }
}

void Transport::seek(const muse::secs_t secs, bool applyIfPlaying)
{
    // Any explicit reposition supersedes a pending paused-stream resume.
    m_pausedResumePos.reset();
    player()->seek(secs, applyIfPlaying);
}

void Transport::doSeek(const muse::secs_t secs, bool applyIfPlaying)
{
    seek(secs, applyIfPlaying);
    setLastPlaybackSeekTime(secs);
    m_lastPlaybackRegion = { secs, secs };
    m_pauseShouldStopPlayback = false;
}

void Transport::changePlaybackRegion(const muse::secs_t start, const muse::secs_t end)
{
    doChangePlaybackRegion({ start, end });
}

void Transport::doChangePlaybackRegion(const PlaybackRegion& region)
{
    // A new region is just as much an explicit reposition as a seek: it
    // supersedes any pending paused-stream resume.
    m_pausedResumePos.reset();

    m_lastPlaybackRegion = region;

    if (isStopped()) {
        updatePlaybackRegion();
    }

    if (region.isValid()) {
        setLastPlaybackSeekTime(m_lastPlaybackRegion.start);
    }
}

void Transport::stopSeekAndUpdatePlaybackRegion()
{
    stop();

    seek(lastPlaybackSeekTime(), false);
    updatePlaybackRegion();
}

void Transport::toggleLoopPlayback()
{
    player()->setLoopRegionActive(!player()->isLoopRegionActive());
}

void Transport::setLoopRegionToSelection()
{
    double start = 0;
    double end = 0;

    if (!selectionController()->timeSelectionIsEmpty()) {
        start = selectionController()->dataSelectedStartTime();
        end = selectionController()->dataSelectedEndTime();
    } else {
        auto itemStart = selectionController()->leftMostSelectedItemStartTime();
        auto itemEnd = selectionController()->rightMostSelectedItemEndTime();
        if (itemStart.has_value() && itemEnd.has_value()) {
            start = itemStart.value();
            end = itemEnd.value();
        } else {
            player()->clearLoopRegion();
            return;
        }
    }

    player()->setLoopRegion({ start, end });
}

void Transport::setSelectionToLoop()
{
    const PlaybackRegion region = player()->loopRegion();

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    trackedit::TrackIdList tracks = prj->trackIdList();

    selectionController()->setSelectedTracks(tracks, false);
    selectionController()->setDataSelectedStartTime(region.start, false);
    selectionController()->setDataSelectedEndTime(region.end, true);
}

void Transport::setLoopRegionInOut()
{
    const PlaybackRegion region = player()->loopRegion();

    muse::UriQuery loopRegionInOutUri("audacity://playback/loop_region_in_out");
    loopRegionInOutUri.addParam("title", muse::Val(muse::trc("trackedit", "Set looping region in/out")));
    loopRegionInOutUri.addParam("start", muse::Val(static_cast<double>(region.start)));
    loopRegionInOutUri.addParam("end", muse::Val(static_cast<double>(region.end)));

    const muse::RetVal<muse::Val> rv = interactive()->openSync(loopRegionInOutUri);
    if (!rv.ret.success()) {
        return;
    }

    const QVariantMap vals = rv.val.toQVariant().toMap();

    player()->setLoopRegion({ vals["start"].toDouble(), vals["end"].toDouble() });
}

void Transport::setSelectionFollowsLoopRegion()
{
    playbackConfiguration()->setSelectionFollowsLoopRegion(!playbackConfiguration()->selectionFollowsLoopRegion());
}

void Transport::setAudioApi(const std::string& api)
{
    if (api != audioDevicesProvider()->currentApi()) {
        withStreamRestart([this, api]() {
            audioDevicesProvider()->setApi(api);
        });
    }
}

void Transport::setAudioOutputDevice(const std::string& device)
{
    if (device != audioDevicesProvider()->currentOutputDevice()) {
        withStreamRestart([this, device]() {
            audioDevicesProvider()->setOutputDevice(device);
        });
    }
}

void Transport::setAudioInputDevice(const std::string& device)
{
    if (device != audioDevicesProvider()->currentInputDevice()) {
        withStreamRestart([this, device]() {
            audioDevicesProvider()->setInputDevice(device);
        });
    }
}

void Transport::setInputChannels(int channels)
{
    IF_ASSERT_FAILED(channels > 0 && channels <= audioDevicesProvider()->inputChannelsAvailable()) {
        return;
    }
    if (channels != audioDevicesProvider()->inputChannelsSelected()) {
        withStreamRestart([this, channels]() {
            audioDevicesProvider()->setInputChannels(channels);
        });
    }
}

void Transport::setDefaultSampleRate(uint64_t rate)
{
    if (rate != audioDevicesProvider()->defaultSampleRate()) {
        withStreamRestart([this, rate]() {
            audioDevicesProvider()->setDefaultSampleRate(rate);
        });
    }
}

void Transport::setBufferLength(double duration)
{
    if (!muse::RealIsEqual(duration, audioDevicesProvider()->bufferLength())) {
        withStreamRestart([this, duration]() {
            audioDevicesProvider()->setBufferLength(duration);
        });
    }
}

void Transport::rescanAudioDevices()
{
    withStreamRestart([this]() {
        audioDevicesProvider()->rescan();
    });
}

void Transport::withStreamRestart(const std::function<void()>& change)
{
    const bool isRecording = recordController()->isRecording();
    const bool wasPlaying = !isRecording && isPlaying();
    const bool wasPaused = !isRecording && isPaused();
    const muse::secs_t resumePos = player()->playbackPosition();

    if (isRecording) {
        record()->stop();
    } else if (wasPlaying || wasPaused) {
        stop();
    }

    change();

    // Only resume when the user was actively playing. A paused or recording
    // transport is torn down for the switch and then left stopped.
    if (wasPlaying) {
        // Restart the stream at the interrupted position. The play region, the
        // seek anchor and the rest of the session state are deliberately
        // untouched: a bounded region still ends where it did, an active loop
        // still wraps within its bounds, and a stop still returns to where
        // playback originally started.
        player()->play(resumePos);
    } else if (wasPaused) {
        // The paused stream can't survive the switch, so we end up stopped.
        // Remember the pause position so the next play resumes from there; the
        // stop anchor (m_lastPlaybackSeekTime) is deliberately left untouched, so
        // a stop still returns to where playback originally started.
        m_pausedResumePos = resumePos;
    }
}

PlaybackRegion Transport::selectionPlaybackRegion() const
{
    // item selection has priority over time selection
    auto itemStart = selectionController()->leftMostSelectedItemStartTime();
    auto itemEnd = selectionController()->rightMostSelectedItemEndTime();
    if (itemStart.has_value() && itemEnd.has_value()) {
        return { itemStart.value(), itemEnd.value() };
    }

    if (!selectionController()->timeSelectionIsEmpty()) {
        return { selectionController()->dataSelectedStartTime(),
                 selectionController()->dataSelectedEndTime() };
    }

    return PlaybackRegion();
}

bool Transport::isSelectionPlaybackRegionChanged() const
{
    return m_lastPlaybackRegion.isValid() && m_lastPlaybackRegion != player()->playbackRegion();
}

void Transport::updatePlaybackRegion()
{
    player()->setPlaybackRegion(m_lastPlaybackRegion);
}

void Transport::onProjectChanged()
{
    au::project::IAudacityProjectPtr prj = globalContext()->currentProject();
    if (prj) {
        prj->aboutCloseBegin().onNotify(this, [this]() {
            stopSeekAndUpdatePlaybackRegion();
        });

        seek(0.0, false); // TODO: get the previous position from the project data
        setLastPlaybackSeekTime(player()->playbackPosition());
    }
}

void Transport::onPlaybackPositionChanged()
{
    if (isPlaybackPositionOnTheEndOfProject() || isPlaybackPositionOnTheEndOfPlaybackRegion()) {
        //! NOTE: just stop, without seek
        player()->stop();
        if (player()->playbackRegion() != m_lastPlaybackRegion && !isEqualToPlaybackPosition(m_lastPlaybackRegion.end)) {
            // we want to update the playback region in case user made new selection during playback
            updatePlaybackRegion();
        }
    }
}

bool Transport::isEqualToPlaybackPosition(const muse::secs_t position) const
{
    const muse::secs_t playbackPos = player()->playbackPosition();
    return playbackPos - TIME_EPS <= position && position <= playbackPos + TIME_EPS;
}

bool Transport::isPlaybackPositionOnTheEndOfProject() const
{
    return isEqualToPlaybackPosition(totalPlayTime());
}

bool Transport::isPlaybackPositionOnTheEndOfPlaybackRegion() const
{
    PlaybackRegion region = player()->playbackRegion();
    return region.isValid() && isEqualToPlaybackPosition(region.end) && !player()->isLoopRegionActive();
}

bool Transport::isPlaybackStartPositionValid() const
{
    muse::secs_t total = totalPlayTime();

    if (lastPlaybackSeekTime() >= total) {
        return false;
    }

    if (m_lastPlaybackRegion.start >= total) {
        return false;
    }

    return true;
}

bool Transport::isSeekPositionValid(const muse::secs_t& seekTime) const
{
    const auto region = player()->playbackRegion();
    return region.isValid() ? (seekTime <= region.end) : (seekTime <= totalPlayTime());
}
}
