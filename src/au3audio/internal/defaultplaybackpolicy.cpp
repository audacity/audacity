/*
* Audacity: A Digital Audio Editor
*/
#include "defaultplaybackpolicy.h"

#include "ProjectAudioIO.h"
#include "SampleCount.h"
#include "ViewInfo.h"

DefaultPlaybackPolicy::DefaultPlaybackPolicy(AudacityProject& project,
                                             double trackEndTime, double loopEndTime, std::optional<double> pStartTime,
                                             bool loopEnabled, bool variableSpeed)
    : mProject{project}
    , mTrackEndTime{trackEndTime}
    , mLoopEndTime{loopEndTime}
    , mpStartTime{pStartTime}
    , mLoopEnabled{loopEnabled}
    , mVariableSpeed{variableSpeed}
{}

void DefaultPlaybackPolicy::Initialize(
    PlaybackSchedule& schedule, double rate)
{
    PlaybackPolicy::Initialize(schedule, rate);
    mLastPlaySpeed = GetPlaySpeed();
    // mMessageChannel.Initialize()
    mMessageChannel.Write({ mLastPlaySpeed,
                            schedule.mT0, mLoopEndTime, mLoopEnabled });

    auto callback = [this](auto&){ WriteMessage(); };
    mRegionSubscription
        =ViewInfo::Get(mProject).playRegion.Subscribe(callback);
    if (mVariableSpeed) {
        mSpeedSubscription = ProjectAudioIO::Get(mProject).Subscribe(callback);
    }
}

Mixer::WarpOptions DefaultPlaybackPolicy::MixerWarpOptions(
    PlaybackSchedule& schedule)
{
    if (mVariableSpeed) {
        // Enable variable rate mixing
        return Mixer::WarpOptions(0.01, 32.0, GetPlaySpeed());
    } else {
        return PlaybackPolicy::MixerWarpOptions(schedule);
    }
}

PlaybackPolicy::BufferTimes
DefaultPlaybackPolicy::SuggestedBufferTimes(PlaybackSchedule&)
{
    // Shorter times than in the default policy so that responses to changes of
    // loop region or speed slider don't lag too much
    using namespace std::chrono;
    return { 0.05s, 0.05s, 0.25s };
}

bool DefaultPlaybackPolicy::RevertToOldDefault(const PlaybackSchedule& schedule) const
{
    return !mLoopEnabled
           ||// Even if loop is enabled, ignore it if right of looping region
           schedule.mTimeQueue.GetLastTime() > mLoopEndTime;
}

bool DefaultPlaybackPolicy::Done(
    PlaybackSchedule& schedule, unsigned long outputFrames)
{
    if (RevertToOldDefault(schedule)) {
        auto diff = schedule.GetSequenceTime() - schedule.mT1;
        if (schedule.ReversedTime()) {
            diff *= -1;
        }
        return sampleCount(floor(diff * mRate + 0.5)) >= 0;
    }
    return false;
}

double DefaultPlaybackPolicy::OffsetSequenceTime(
    PlaybackSchedule& schedule, double offset)
{
    auto time = schedule.GetSequenceTime();

    // Assuming that mpStartTime always has a value when this policy is used
    if (mpStartTime) {
        if (mLoopEnabled) {
            if (time < schedule.mT0) {
                time = std::clamp(time + offset, *mpStartTime, schedule.mT1);
            } else {
                time = std::clamp(time + offset, schedule.mT0, schedule.mT1);
            }
        } else {
            time += offset;
        }
    }

    schedule.RealTimeInit(time);
    return time;
}

PlaybackSlice
DefaultPlaybackPolicy::GetPlaybackSlice(
    PlaybackSchedule& schedule, size_t available)
{
    // How many samples to produce for each channel.
    const auto realTimeRemaining = std::max(0.0, schedule.RealTimeRemaining());
    mRemaining = realTimeRemaining * mRate / mLastPlaySpeed;

    auto frames = available;
    auto toProduce = frames;
    double deltat = (frames / mRate) * mLastPlaySpeed;

    if (deltat > realTimeRemaining) {
        toProduce = frames = 0.5 + (realTimeRemaining * mRate) / mLastPlaySpeed;
        auto realTime = realTimeRemaining;
        double extra = 0;
        if (RevertToOldDefault(schedule)) {
            // Produce some extra silence so that the time queue consumer can
            // satisfy its end condition
            const double extraRealTime
                =((TimeQueueGrainSize + 1) / mRate) * mLastPlaySpeed;
            extra = std::min(extraRealTime, deltat - realTimeRemaining);
            frames = ((realTimeRemaining + extra) * mRate) / mLastPlaySpeed;
        }
        schedule.RealTimeAdvance(realTimeRemaining + extra);
    } else {
        schedule.RealTimeAdvance(deltat);
    }

    // Don't fall into an infinite loop, if loop-playing a selection
    // that is so short, it has no samples: detect that case
    if (frames == 0) {
        bool progress = (schedule.mWarpedTime != 0.0);
        if (!progress) {
            // Cause FillPlayBuffers to make progress, filling all available with 0
            frames = available, toProduce = 0;
        }
    }
    return { available, frames, toProduce };
}

std::pair<double, double> DefaultPlaybackPolicy::AdvancedTrackTime(
    PlaybackSchedule& schedule, double trackTime, size_t nSamples)
{
    bool revert = RevertToOldDefault(schedule);
    if (!mVariableSpeed && revert) {
        return PlaybackPolicy::AdvancedTrackTime(schedule, trackTime, nSamples);
    }

    mRemaining -= std::min(mRemaining, nSamples);
    if (mRemaining == 0 && !revert) {
        // Wrap to start
        return { schedule.mT1, schedule.mT0 };
    }

    // Defense against cases that might cause loops not to terminate
    if (fabs(schedule.mT0 - schedule.mT1) < 1e-9) {
        return { schedule.mT0, schedule.mT0 };
    }

    auto realDuration = (nSamples / mRate) * mLastPlaySpeed;
    if (schedule.ReversedTime()) {
        realDuration *= -1.0;
    }

    if (schedule.mEnvelope) {
        trackTime
            =schedule.SolveWarpedLength(trackTime, realDuration);
    } else {
        trackTime += realDuration;
    }

    return { trackTime, trackTime };
}

bool DefaultPlaybackPolicy::RepositionPlayback(
    PlaybackSchedule& schedule, const Mixers& playbackMixers,
    size_t frames, size_t available)
{
    // This executes in the SequenceBufferExchange thread
    auto data = mMessageChannel.Read();

    bool speedChange = false;
    if (mVariableSpeed) {
        speedChange = (mLastPlaySpeed != data.mPlaySpeed);
        mLastPlaySpeed = data.mPlaySpeed;
    }

    bool empty = (data.mT0 >= data.mT1);
    bool kicked = false;

    // Amount in seconds by which right boundary can be moved left of the play
    // head, yet loop play in progress will still capture the head
    constexpr auto allowance = 0.5;

    // Looping may become enabled if the main thread said so, but require too
    // that the loop region is non-empty and the play head is not far to its
    // right
    bool loopWasEnabled = !RevertToOldDefault(schedule);
    mLoopEnabled = data.mLoopEnabled && !empty
                   && schedule.mTimeQueue.GetLastTime() <= data.mT1 + allowance;

    // Four cases:  looping transitions off, or transitions on, or stays on,
    // or stays off.
    // Besides which, the variable speed slider may have changed.

    // If looping transitions on, or remains on and the region changed,
    // adjust the schedule...
    auto mine = std::tie(schedule.mT0, mLoopEndTime);
    auto theirs = std::tie(data.mT0, data.mT1);
    if ((loopWasEnabled != mLoopEnabled) || (mine != theirs)) {
        kicked = true;
        if (!empty) {
            mine = theirs;
            schedule.mT1 = data.mT1;
        }
        if (!mLoopEnabled) {
            // Continue play to the end
            schedule.mT1 = std::max(schedule.mT0, mTrackEndTime);
        }
        schedule.mWarpedLength = schedule.RealDuration(schedule.mT1);

        auto newTime = schedule.mTimeQueue.GetLastTime();
#if 0
        // This would make play jump forward or backward into the adjusted
        // looping region if not already in it
        newTime = std::clamp(newTime, schedule.mT0, schedule.mT1);
#endif

        if (newTime >= schedule.mT1 && mLoopEnabled) {
            newTime = schedule.mT0;
        }

        // So that the play head will redraw in the right place:
        schedule.mTimeQueue.SetLastTime(newTime);

        schedule.RealTimeInit(newTime);
        const auto realTimeRemaining = std::max(0.0, schedule.RealTimeRemaining());
        mRemaining = realTimeRemaining * mRate / mLastPlaySpeed;
    } else if (speedChange) {
        // Don't return early
        kicked = true;
    } else {
        // ... else the region did not change, or looping is now off, in
        // which case we have nothing special to do
        if (RevertToOldDefault(schedule)) {
            return PlaybackPolicy::RepositionPlayback(schedule, playbackMixers,
                                                      frames, available);
        }
    }

    // msmeyer: If playing looped, check if we are at the end of the buffer
    // and if yes, restart from the beginning.
    if (mRemaining <= 0) {
        // Looping jumps left
        for (auto& pMixer : playbackMixers) {
            pMixer->SetTimesAndSpeed(
                schedule.mT0, schedule.mT1, mLastPlaySpeed, true);
        }
        schedule.RealTimeRestart();
    } else if (kicked) {
        // Play bounds need redefinition
        const auto time = schedule.mTimeQueue.GetLastTime();
        for (auto& pMixer : playbackMixers) {
            // So that the mixer will fetch the next samples from the right place:
            pMixer->SetTimesAndSpeed(time, schedule.mT1, mLastPlaySpeed);
            pMixer->Reposition(time, true);
        }
    }
    return false;
}

bool DefaultPlaybackPolicy::Looping(const PlaybackSchedule&) const
{
    return mLoopEnabled;
}

void DefaultPlaybackPolicy::WriteMessage()
{
    const auto& region = ViewInfo::Get(mProject).playRegion;
    mMessageChannel.Write({ GetPlaySpeed(),
                            region.GetStart(), region.GetEnd(), region.Active()
                          });
}

double DefaultPlaybackPolicy::GetPlaySpeed()
{
    return mVariableSpeed
           ? ProjectAudioIO::Get(mProject).GetPlaySpeed()
           : 1.0;
}
