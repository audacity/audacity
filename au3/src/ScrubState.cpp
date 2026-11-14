/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file ScrubState.cpp

 Paul Licameli split from AudioIO.cpp

 **********************************************************************/

#include "ScrubState.h"
#include "AudioIO.h"
#include "Mix.h"

namespace {
struct ScrubQueue : NonInterferingBase
{
    static ScrubQueue Instance;

    ScrubQueue() {}

    void Init(double t0,
              double rate,
              const ScrubbingOptions& options)
    {
        mRate = rate;
        mStartTime = t0;
        const double t1 = options.bySpeed ? options.initSpeed : t0;
        Update(t1, options);

        mStarted = false;
        mStopped = false;
        mAccumulatedSeekDuration = 0;
    }

    void Update(double end, const ScrubbingOptions& options)
    {
        // Called by another thread
        mMessage.Write({ end, options });
    }

    void Get(sampleCount& startSample, sampleCount& endSample,
             sampleCount inDuration, sampleCount& duration)
    {
        // Called by the thread that calls AudioIO::SequenceBufferExchange
        startSample = endSample = duration = -1LL;
        sampleCount s0Init;

        Message message(mMessage.Read());
        if (!mStarted) {
            s0Init = llrint(mRate
                            * std::max(message.options.minTime,
                                       std::min(message.options.maxTime, mStartTime)));

            // Make some initial silence. This is not needed in the case of
            // keyboard scrubbing or play-at-speed, because the initial speed
            // is known when this function is called the first time.
            if (!(message.options.isKeyboardScrubbing)) {
                mData.mS0 = mData.mS1 = s0Init;
                mData.mGoal = -1;
                mData.mDuration = duration = inDuration;
                mData.mSilence = 0;
            }
        }

        if (mStarted || message.options.isKeyboardScrubbing) {
            Data newData;
            inDuration += mAccumulatedSeekDuration;

            // If already started, use the previous end as NEW start.
            const auto s0 = mStarted ? mData.mS1 : s0Init;
            const sampleCount s1(message.options.bySpeed
                                 ? s0.as_double()
                                 + lrint(inDuration.as_double() * message.end) // end is a speed
                                 : lrint(message.end * mRate) // end is a time
                                 );
            auto success
                =newData.Init(mData, s0, s1, inDuration, message.options, mRate);
            if (success) {
                mAccumulatedSeekDuration = 0;
            } else {
                mAccumulatedSeekDuration += inDuration;
                return;
            }
            mData = newData;
        }

        mStarted = true;

        Data& entry = mData;
        if (mStopped.load(std::memory_order_relaxed)) {
            // We got the shut-down signal, or we discarded all the work.
            // Output the -1 values.
        } else if (entry.mDuration > 0) {
            // First use of the entry
            startSample = entry.mS0;
            endSample = entry.mS1;
            duration = entry.mDuration;
            entry.mDuration = 0;
        } else if (entry.mSilence > 0) {
            // Second use of the entry
            startSample = endSample = entry.mS1;
            duration = entry.mSilence;
            entry.mSilence = 0;
        }
    }

    void Stop()
    {
        mStopped.store(true, std::memory_order_relaxed);
        mStarted = false;
    }

    // Should make mS1 atomic?
    double LastTrackTime() const
    {
        // Needed by the main thread sometimes
        return mData.mS1.as_double() / mRate;
    }

    ~ScrubQueue() {}

    bool Started() const { return mStarted; }

private:
    struct Data
    {
        Data()
            : mS0(0)
            , mS1(0)
            , mGoal(0)
            , mDuration(0)
            , mSilence(0)
        {}

        bool Init(Data& rPrevious, sampleCount s0, sampleCount s1,
                  sampleCount duration,
                  const ScrubbingOptions& options, double rate)
        {
            auto previous = &rPrevious;
            auto origDuration = duration;
            mSilence = 0;

            const bool& adjustStart = options.adjustStart;

            wxASSERT(duration > 0);
            double speed
                =(std::abs((s1 - s0).as_long_long())) / duration.as_double();
            bool adjustedSpeed = false;

            auto minSpeed = std::min(options.minSpeed, options.maxSpeed);
            wxASSERT(minSpeed == options.minSpeed);

            // May change the requested speed and duration
            if (!adjustStart && speed > options.maxSpeed) {
                // Reduce speed to the maximum selected in the user interface.
                speed = options.maxSpeed;
                mGoal = s1;
                adjustedSpeed = true;
            } else if (!adjustStart
                       && previous->mGoal >= 0
                       && previous->mGoal == s1) {
                // In case the mouse has not moved, and playback
                // is catching up to the mouse at maximum speed,
                // continue at no less than maximum.  (Without this
                // the final catch-up can make a slow scrub interval
                // that drops the pitch and sounds wrong.)
                minSpeed = options.maxSpeed;
                mGoal = s1;
                adjustedSpeed = true;
            } else {
                mGoal = -1;
            }

            if (speed < minSpeed) {
                if (s0 != s1 && adjustStart) {
                    // Do not trim the duration.
                } else {
                    // Trim the duration.
                    duration
                        =std::max(0L, lrint(speed * duration.as_double() / minSpeed));
                }

                speed = minSpeed;
                adjustedSpeed = true;
            }

            if (speed < ScrubbingOptions::MinAllowedScrubSpeed()) {
                // Mixers were set up to go only so slowly, not slower.
                // This will put a request for some silence in the work queue.
                adjustedSpeed = true;
                speed = 0.0;
            }

            // May change s1 or s0 to match speed change or stay in bounds of the project

            if (adjustedSpeed && !adjustStart) {
                // adjust s1
                const sampleCount diff = lrint(speed * duration.as_double());
                if (s0 < s1) {
                    s1 = s0 + diff;
                } else {
                    s1 = s0 - diff;
                }
            }

            bool silent = false;

            // Adjust s1 (again), and duration, if s1 is out of bounds,
            // or abandon if a stutter is too short.
            // (Assume s0 is in bounds, because it equals the last scrub's s1 which was checked.)
            if (s1 != s0) {
                // When playback follows a fast mouse movement by "stuttering"
                // at maximum playback, don't make stutters too short to be useful.
                if (options.adjustStart
                    && duration < llrint(options.minStutterTime.count() * rate)) {
                    return false;
                }

                sampleCount minSample { llrint(options.minTime * rate) };
                sampleCount maxSample { llrint(options.maxTime * rate) };
                auto newDuration = duration;
                const auto newS1 = std::max(minSample, std::min(maxSample, s1));
                if (s1 != newS1) {
                    newDuration = std::max(sampleCount { 0 },
                                           sampleCount(
                                               duration.as_double() * (newS1 - s0).as_double()
                                               / (s1 - s0).as_double()
                                               )
                                           );
                }
                if (newDuration == 0) {
                    // A silent scrub with s0 == s1
                    silent = true;
                    s1 = s0;
                } else if (s1 != newS1) {
                    // Shorten
                    duration = newDuration;
                    s1 = newS1;
                }
            }

            if (adjustStart && !silent) {
                // Limit diff because this is seeking.
                const sampleCount diff
                    =lrint(std::min(options.maxSpeed, speed) * duration.as_double());
                if (s0 < s1) {
                    s0 = s1 - diff;
                } else {
                    s0 = s1 + diff;
                }
            }

            mS0 = s0;
            mS1 = s1;
            mDuration = duration;
            if (duration < origDuration) {
                mSilence = origDuration - duration;
            }

            return true;
        }

        sampleCount mS0;
        sampleCount mS1;
        sampleCount mGoal;
        sampleCount mDuration;
        sampleCount mSilence;
    };

    double mStartTime{};
    bool mStarted{ false };
    std::atomic<bool> mStopped { false };
    Data mData;
    double mRate{};
    struct Message {
        Message() = default;
        Message(const Message&) = default;
        double end;
        ScrubbingOptions options;
    };
    MessageBuffer<Message> mMessage;
    sampleCount mAccumulatedSeekDuration{};
};

ScrubQueue ScrubQueue::Instance;
}

ScrubbingPlaybackPolicy::ScrubbingPlaybackPolicy(
    const ScrubbingOptions& options)
    : mOptions{options}
{}

ScrubbingPlaybackPolicy::~ScrubbingPlaybackPolicy() = default;

void ScrubbingPlaybackPolicy::Initialize(PlaybackSchedule& schedule,
                                         double rate)
{
    PlaybackPolicy::Initialize(schedule, rate);
    mScrubDuration = mStartSample = mEndSample = 0;
    mOldEndTime = mNewStartTime = 0;
    mScrubSpeed = 0;
    mSilentScrub = mReplenish = false;
    mUntilDiscontinuity = 0;
    ScrubQueue::Instance.Init(schedule.mT0, rate, mOptions);
}

void ScrubbingPlaybackPolicy::Finalize(PlaybackSchedule&)
{
    ScrubQueue::Instance.Stop();
}

Mixer::WarpOptions ScrubbingPlaybackPolicy::MixerWarpOptions(PlaybackSchedule&)
{
    return Mixer::WarpOptions{
        ScrubbingOptions::MinAllowedScrubSpeed(),
        ScrubbingOptions::MaxAllowedScrubSpeed() };
}

PlaybackPolicy::BufferTimes
ScrubbingPlaybackPolicy::SuggestedBufferTimes(PlaybackSchedule&)
{
    using namespace std::chrono;
    return {
        // For useful scrubbing, we can't run too far ahead without checking
        // mouse input, so make fillings more and shorter.
        // Specify a very short minimum batch for non-seek scrubbing, to allow
        // more frequent polling of the mouse
        mOptions.delay,

        // Specify enough playback RingBuffer latency so we can refill
        // once every seek stutter without falling behind the demand.
        // (Scrub might switch in and out of seeking with left mouse
        // presses in the ruler)
        2 * mOptions.minStutterTime,

        // Same as for default policy
        10.0s
    };
}

bool ScrubbingPlaybackPolicy::AllowSeek(PlaybackSchedule&)
{
    // While scrubbing, ignore seek requests
    return false;
}

bool ScrubbingPlaybackPolicy::Done(
    PlaybackSchedule& schedule, unsigned long)
{
    return false;
}

std::chrono::milliseconds
ScrubbingPlaybackPolicy::SleepInterval(PlaybackSchedule&)
{
    return ScrubPollInterval;
}

PlaybackSlice ScrubbingPlaybackPolicy::GetPlaybackSlice(
    PlaybackSchedule&, size_t available)
{
    if (mReplenish) {
        return { available, 0, 0 }
    }

    auto gAudioIO = AudioIO::Get();

    // How many samples to produce for each channel.
    auto frames = available;
    auto toProduce = frames;

    // scrubbing and play-at-speed are not limited by the real time
    // and length accumulators
    toProduce
        =frames = limitSampleBufferSize(frames, mScrubDuration);

    if (mSilentScrub) {
        toProduce = 0;
    }

    mScrubDuration -= frames;
    wxASSERT(mScrubDuration >= 0);

    mUntilDiscontinuity = 0;
    if (mScrubDuration <= 0) {
        mReplenish = true;
        auto oldEndSample = mEndSample;
        mOldEndTime = oldEndSample.as_long_long() / mRate;
        ScrubQueue::Instance.Get(
            mStartSample, mEndSample, available, mScrubDuration);
        mNewStartTime = mStartSample.as_long_long() / mRate;
        if (mScrubDuration >= 0 && oldEndSample != mStartSample) {
            mUntilDiscontinuity = frames;
        }
    }

    return { available, frames, toProduce };
}

std::pair<double, double> ScrubbingPlaybackPolicy::AdvancedTrackTime(
    PlaybackSchedule& schedule, double trackTime, size_t nSamples)
{
    auto realDuration = nSamples / mRate;
    auto result = trackTime + realDuration * mScrubSpeed;
    bool discontinuity = nSamples > 0
                         && mUntilDiscontinuity > 0
                         && 0 == (mUntilDiscontinuity -= std::min(mUntilDiscontinuity, nSamples));
    if (discontinuity) {
        return { mOldEndTime, mNewStartTime }
    } else {
        return { result, result }
    }
}

bool ScrubbingPlaybackPolicy::RepositionPlayback(
    PlaybackSchedule& schedule, const Mixers& playbackMixers,
    size_t frames, size_t available)
{
    auto gAudioIO = AudioIO::Get();

    if (available > 0 && mReplenish) {
        mReplenish = false;
        if (mScrubDuration < 0) {
            // Can't play anything
            // Stop even if we don't fill up available
            mScrubDuration = 0;

            // Force stop of filling of buffers
            return true;
        } else {
            mSilentScrub = (mEndSample == mStartSample);
            double startTime, endTime;
            startTime = mStartSample.as_double() / mRate;
            endTime = mEndSample.as_double() / mRate;
            auto diff = (mEndSample - mStartSample).as_long_long();
            if (mScrubDuration == 0) {
                mScrubSpeed = 0;
            } else {
                mScrubSpeed
                    =double(diff) / mScrubDuration.as_double();
            }
            if (!mSilentScrub) {
                for (auto& pMixer : playbackMixers) {
                    if (mOptions.isKeyboardScrubbing) {
                        pMixer->SetSpeedForKeyboardScrubbing(mScrubSpeed, startTime);
                    } else {
                        pMixer->SetTimesAndSpeed(
                            startTime, endTime, fabs(mScrubSpeed));
                    }
                }
            }
        }
    }

    return false;
}

void ScrubState::UpdateScrub
    (double endTimeOrSpeed, const ScrubbingOptions& options)
{
    auto& queue = ScrubQueue::Instance;
    queue.Update(endTimeOrSpeed, options);
}

void ScrubState::StopScrub()
{
    auto& queue = ScrubQueue::Instance;
    queue.Stop();
}

// Only for DRAG_SCRUB
double ScrubState::GetLastScrubTime()
{
    auto& queue = ScrubQueue::Instance;
    return queue.LastTrackTime();
}

bool ScrubState::IsScrubbing()
{
    auto gAudioIO = AudioIOBase::Get();
    auto& queue = ScrubQueue::Instance;
    return gAudioIO->IsBusy() && queue.Started();
}
