/**********************************************************************

 Audacity: A Digital Audio Editor

 @file PlaybackSchedule.cpp

 Paul Licameli split from AudioIOBase.cpp

 **********************************************************************/

#include "PlaybackSchedule.h"

#include "AudioIOBase.h"
#include "Envelope.h"
#include "Mix.h"
#include "Project.h"
#include "SampleCount.h"

#include <cmath>

PlaybackPolicy::~PlaybackPolicy() = default;

void PlaybackPolicy::Initialize(PlaybackSchedule&, double rate)
{
    mRate = rate;
}

void PlaybackPolicy::Finalize(PlaybackSchedule&) {}

Mixer::WarpOptions PlaybackPolicy::MixerWarpOptions(PlaybackSchedule& schedule)
{
    return Mixer::WarpOptions{ schedule.mEnvelope };
}

PlaybackPolicy::BufferTimes
PlaybackPolicy::SuggestedBufferTimes(PlaybackSchedule&)
{
    using namespace std::chrono;
#if 1
    // Shorter times than in the default policy so that responses, to changes of
    // loop region or speed slider or other such controls, don't lag too much
    return { 0.05s, 0.05s, 0.25s };
#else
/*
The old values, going very far back.

There are old comments in the code about larger batches of work filling the
queue with samples, to reduce CPU usage.  Maybe this doesn't matter with most
modern machines, or maybe there will prove to be a need to choose the numbers
more smartly than these hardcoded values.  Maybe we will need to figure out
adaptiveness of the buffer size by detecting how long the work takes.  Maybe
we can afford even smaller times.
*/
    return { 4.0s, 4.0s, 10.0s };
#endif
}

bool PlaybackPolicy::AllowSeek(PlaybackSchedule&)
{
    return true;
}

bool PlaybackPolicy::Done(PlaybackSchedule& schedule,
                          unsigned long outputFrames)
{
    // Called from portAudio thread, use GetSequenceTime()
    auto diff = schedule.GetSequenceTime() - schedule.mT1;
    if (schedule.ReversedTime()) {
        diff *= -1;
    }
    return sampleCount(floor(diff * mRate + 0.5)) >= 0
           &&// Require also that output frames are all consumed from ring buffer
           outputFrames == 0;
}

double PlaybackPolicy::OffsetSequenceTime(
    PlaybackSchedule& schedule, double offset)
{
    auto time = schedule.GetSequenceTime() + offset;
    time = std::clamp(time, schedule.mT0, schedule.mT1);
    schedule.RealTimeInit(time);
    return time;
}

std::chrono::milliseconds PlaybackPolicy::SleepInterval(PlaybackSchedule&)
{
    using namespace std::chrono;
    return 10ms;
}

PlaybackSlice
PlaybackPolicy::GetPlaybackSlice(PlaybackSchedule& schedule, size_t available)
{
    // How many samples to produce for each channel.
    const auto realTimeRemaining = schedule.RealTimeRemaining();
    auto frames = available;
    auto toProduce = frames;
    double deltat = frames / mRate;

    if (deltat > realTimeRemaining) {
        // Produce some extra silence so that the time queue consumer can
        // satisfy its end condition
        const double extraRealTime = (TimeQueueGrainSize + 1) / mRate;
        auto extra = std::min(extraRealTime, deltat - realTimeRemaining);
        auto realTime = realTimeRemaining + extra;
        frames = realTime * mRate + 0.5;
        toProduce = realTimeRemaining * mRate + 0.5;
        schedule.RealTimeAdvance(realTime);
    } else {
        schedule.RealTimeAdvance(deltat);
    }

    return { available, frames, toProduce };
}

std::pair<double, double>
PlaybackPolicy::AdvancedTrackTime(PlaybackSchedule& schedule,
                                  double trackTime, size_t nSamples)
{
    auto realDuration = nSamples / mRate;
    if (schedule.ReversedTime()) {
        realDuration *= -1.0;
    }

    if (schedule.mEnvelope) {
        trackTime = schedule.SolveWarpedLength(trackTime, realDuration);
    } else {
        trackTime += realDuration;
    }

    if (trackTime >= schedule.mT1) {
        return { schedule.mT1, std::numeric_limits<double>::infinity() };
    } else {
        return { trackTime, trackTime };
    }
}

bool PlaybackPolicy::RepositionPlayback(
    PlaybackSchedule&, const Mixers&, size_t, size_t)
{
    return true;
}

bool PlaybackPolicy::Looping(const PlaybackSchedule&) const
{
    return false;
}

namespace {
//! The old default playback policy plays once and consumes no messages
struct OldDefaultPlaybackPolicy final : PlaybackPolicy {
    ~OldDefaultPlaybackPolicy() override = default;
};
}

PlaybackPolicy& PlaybackSchedule::GetPolicy()
{
    if (mPolicyValid.load(std::memory_order_acquire) && mpPlaybackPolicy) {
        return *mpPlaybackPolicy;
    }

    static OldDefaultPlaybackPolicy defaultPolicy;
    return defaultPolicy;
}

const PlaybackPolicy& PlaybackSchedule::GetPolicy() const
{
    return const_cast<PlaybackSchedule&>(*this).GetPolicy();
}

void PlaybackSchedule::Init(
    const double t0, const double t1,
    const AudioIOStartStreamOptions& options,
    const RecordingSchedule* pRecordingSchedule)
{
    mpPlaybackPolicy.reset();

    if (pRecordingSchedule) {
        // It does not make sense to apply the time warp during overdub recording,
        // which defeats the purpose of making the recording synchronized with
        // the existing audio.  (Unless we figured out the inverse warp of the
        // captured samples in real time.)
        // So just quietly ignore the time track.
        mEnvelope = nullptr;
    } else {
        mEnvelope = options.envelope;
    }

    mT0      = t0;
    if (pRecordingSchedule) {
        mT0 -= pRecordingSchedule->mPreRoll;
    }

    mT1      = t1;
    if (pRecordingSchedule) {
        // adjust mT1 so that we don't give paComplete too soon to fill up the
        // desired length of recording
        mT1 -= pRecordingSchedule->mLatencyCorrection;
    }

    // Main thread's initialization of mTime
    SetSequenceTime(mT0);

    if (options.policyFactory) {
        mpPlaybackPolicy = options.policyFactory(options);
    }

    mWarpedTime = 0.0;
    mWarpedLength = RealDuration(mT1);

    mPolicyValid.store(true, std::memory_order_release);
}

double PlaybackSchedule::ComputeWarpedLength(double t0, double t1) const
{
    if (mEnvelope) {
        return mEnvelope->IntegralOfInverse(t0, t1);
    } else {
        return t1 - t0;
    }
}

double PlaybackSchedule::SolveWarpedLength(double t0, double length) const
{
    if (mEnvelope) {
        return mEnvelope->SolveIntegralOfInverse(t0, length);
    } else {
        return t0 + length;
    }
}

double PlaybackSchedule::RealDuration(double trackTime1) const
{
    return fabs(RealDurationSigned(trackTime1));
}

double PlaybackSchedule::RealDurationSigned(double trackTime1) const
{
    return ComputeWarpedLength(mT0, trackTime1);
}

double PlaybackSchedule::RealTimeRemaining() const
{
    return mWarpedLength - mWarpedTime;
}

void PlaybackSchedule::RealTimeAdvance(double increment)
{
    mWarpedTime += increment;
}

void PlaybackSchedule::RealTimeInit(double trackTime)
{
    mWarpedTime = RealDurationSigned(trackTime);
}

void PlaybackSchedule::RealTimeRestart()
{
    mWarpedTime = 0;
}

double RecordingSchedule::ToConsume() const
{
    return mDuration - Consumed();
}

double RecordingSchedule::Consumed() const
{
    return std::max(0.0, mPosition + TotalCorrection());
}

double RecordingSchedule::ToDiscard() const
{
    return std::max(0.0, -(mPosition + TotalCorrection()));
}

PlaybackSchedule::TimeQueue::TimeQueue() = default;

void PlaybackSchedule::TimeQueue::Clear()
{
    mNodePool.clear();
    mProducerNode = nullptr;
    mConsumerNode = nullptr;
}

void PlaybackSchedule::TimeQueue::Init(size_t size)
{
    auto node = std::make_unique<Node>();
    mProducerNode = mConsumerNode = node.get();
    mProducerNode->active.test_and_set();
    mProducerNode->records.resize(size);
    mNodePool.clear();
    mNodePool.emplace_back(std::move(node));
}

void PlaybackSchedule::TimeQueue::Producer(
    PlaybackSchedule& schedule, PlaybackSlice slice)
{
    auto& policy = schedule.GetPolicy();

    auto node = mProducerNode;

    if (node == nullptr) {
        // Recording only.  Don't fill the queue.
        return;
    }

    auto written = node->written;
    auto tail = node->tail.load(std::memory_order_acquire);
    auto head = node->head.load(std::memory_order_relaxed);
    auto time = mLastTime;

    auto frames = slice.toProduce;

    auto advanceTail = [&](double time)
    {
        auto newTail = (tail + 1) % static_cast<int>(node->records.size());
        if ((newTail > head && static_cast<size_t>(newTail - head) == node->records.size() - 1)
            || (newTail < head && static_cast<size_t>(head - newTail) == node->records.size() - 1)) {
            try
            {
                Node* next = nullptr;
                for (auto& p : mNodePool) {
                    if (p.get() == node || p->active.test_and_set()) {
                        continue;
                    }

                    next = p.get();
                    //next->offset = 0; set on consumer thread
                    next->next.store(nullptr);
                    next->head.store(0);
                    next->tail.store(0);
                    break;
                }
                if (next == nullptr) {
                    mNodePool.emplace_back(std::make_unique<Node>());
                    next = mNodePool.back().get();
                }
                //previous node had too low capacity to fit all slices,
                //try enlarge capacity to avoid more reallocaitons
                next->records.resize(node->records.size() * 2);
                next->records[0].timeValue = time;

                node->next.store(next);//make it visible to the consumer
                mProducerNode = node = next;
                head = 0;
                newTail = 0;
            }
            catch (...)
            {
                //overwrite last grain...
                newTail = tail;
            }
        } else {
            node->records[newTail].timeValue = time;
        }
        tail = newTail;
        node->written = 0;
    };

    //inv: space > 0
    auto space = TimeQueueGrainSize - written;
    while (frames >= space)
    {
        const auto times = policy.AdvancedTrackTime(schedule, time, space);
        time = times.second;
        if (!std::isfinite(time)) {
            time = times.first;
        }
        advanceTail(time);
        written = 0;
        frames -= space;
        space = TimeQueueGrainSize;
    }
    // Last odd lot
    if (frames > 0) {
        const auto times = policy.AdvancedTrackTime(schedule, time, frames);
        time = times.second;
        if (!std::isfinite(time)) {
            time = times.first;
        }
        written += frames;
        space -= frames;
    }
    // Produce constant times if there is also some silence in the slice
    frames = slice.frames - slice.toProduce;
    while (frames > 0 && frames >= space)
    {
        advanceTail(time);

        frames -= space;
        written = 0;
        space = TimeQueueGrainSize;
    }

    mLastTime = time;
    node->written = written + frames;
    node->tail.store(tail, std::memory_order_release);
}

double PlaybackSchedule::TimeQueue::GetLastTime() const
{
    return mLastTime;
}

void PlaybackSchedule::TimeQueue::SetLastTime(double time)
{
    mLastTime = time;
}

double PlaybackSchedule::TimeQueue::Consumer(size_t nSamples, double rate)
{
    auto node = mConsumerNode;

    if (node == nullptr) {
        // Recording only.  No scrub or playback time warp.  Don't use the queue.
        return mLastTime += nSamples / rate;
    }

    auto head = node->head.load(std::memory_order_acquire);
    auto tail = node->tail.load(std::memory_order_relaxed);

    auto offset = node->offset;
    auto available = TimeQueueGrainSize - offset;

    if (nSamples >= available) {
        do{
            offset = 0;
            nSamples -= available;
            if (head == tail) {
                //Check if circular buffer was reallocated
                if (const auto next = node->next.load()) {
                    node->offset = 0;
                    node->active.clear();

                    mConsumerNode = node = next;
                    head = 0;
                    tail = node->tail.load(std::memory_order_relaxed);
                    available = TimeQueueGrainSize;
                } else {
                    //consumer is ahead of producer...
                    return node->records[head].timeValue;
                }
            } else {
                head = (head + 1) % static_cast<int>(node->records.size());
                available = TimeQueueGrainSize;
            }
        } while (nSamples >= available);
        node->head.store(head, std::memory_order_release);
    }
    node->offset = offset + nSamples;
    return node->records[head].timeValue;
}

void PlaybackSchedule::TimeQueue::Prime(double time)
{
    //TODO: check that consumer and producer indeed suspended when called from AudioIoCallback
    mLastTime = time;
    if (mProducerNode != nullptr) {
        mConsumerNode = mProducerNode;
        mConsumerNode->next.store(nullptr);
        mConsumerNode->head.store(0);
        mConsumerNode->tail.store(0);
        mConsumerNode->written = 0;
        mConsumerNode->offset = 0;
        mConsumerNode->records[0].timeValue = time;
    }
}
