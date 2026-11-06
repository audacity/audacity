/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3-mixer/AudioIOSequences.h"
#include "au3-math/SampleFormat.h"
#include "PlaybackSchedule.h"
#include "RingBuffer.h"

#include <wx/atomic.h>

#include <functional>
#include <memory>
#include <utility>
#include <vector>

class AudacityProject;
class AudioIOListener;
class Resample;

//! Moves captured device input into the RecordableSequences of a recording.
/*!
 Device-agnostic: the owner of the audio device pushes interleaved input from its
 callback, a worker thread drains it into the sequences, and the main thread
 finalizes the recording once the device is stopped.

 Thread usage mirrors the former AudioIO members it replaces:
 - main thread: everything but Push() and Drain(), while no stream runs
 - device callback: Push()
 - buffer-exchange thread: Drain()
 */
class AUDIO_IO_API CaptureSink
{
public:
    CaptureSink();
    ~CaptureSink();

    //! Main thread, before a stream starts
    void Reset(RecordableSequences sequences, bool detectDropouts);
    void ConfigureRouting(size_t inputChannelsCount);
    void ResetRouting();

    //! Main thread, before buffer allocation attempts
    void InitBufferTimes(size_t numChannels);
    //! Main thread, before a stream starts; may throw std::bad_alloc
    /*!
     @param captureFormat device sample format
     @param deviceRate rate of the device stream
     @param factor ratio of sequence rate to device rate
     @param sequenceRate rate of the capture sequences
     @return false if the buffers would be unreasonably small
     */
    bool AllocateBuffers(size_t numChannels, sampleFormat captureFormat, double deviceRate, double factor, double sequenceRate);
    void ClearBuffers();
    //! For retrying allocation with less memory
    void HalveBufferTimes();
    double RingBufferSecs() const { return mRingBufferSecs; }

    //! Device callback
    /*!
     @param sequenceTime current playback schedule time, used to locate dropouts
     @param inputError the device reported an input overflow
     @param simulateError drop this buffer, for testing
     @param scratch room for at least `framesPerBuffer` floats
     @return the number of frames that could be buffered
     */
    size_t Push(constSamplePtr interleaved, size_t numChannels, size_t framesPerBuffer, double sequenceTime, bool inputError,
                 bool simulateError, float* scratch);

    //! Buffer-exchange thread
    /*!
     @param force drain even if less than the minimum batch is available
     @param streamActive false makes resamplers flush their remainder
     @param stopTransport called on the main thread if appending failed
     */
    void Drain(bool force, bool streamActive, AudioIOListener* listener, const std::function<void()>& stopTransport);

    //! Main thread, once the device is stopped and the last Drain() is done:
    //! flushes the sequences and fills dropouts with silence.
    void Finish(AudacityProject* project, AudioIOListener* listener);

    //! Main thread, after Finish()
    void Clear();

    const RecordableSequences& Sequences() const { return mSequences; }
    RecordingSchedule& Schedule() { return mSchedule; }
    const RecordingSchedule& Schedule() const { return mSchedule; }

    bool HasRecordingException() const { return mRecordingException; }
    void ClearRecordingException();

    // Pairs of starting time and duration
    const std::vector<std::pair<double, double> >& LostCaptureIntervals() const { return mLostCaptureIntervals; }

private:
    void SetRecordingException() { wxAtomicInc(mRecordingException); }
    size_t CommonlyAvail() const;

    using RingBuffers = std::vector<std::unique_ptr<RingBuffer> >;
    RingBuffers mBuffers;
    std::vector<std::unique_ptr<Resample> > mResample;

    RecordableSequences mSequences;
    struct TrackChannelInfo {
        size_t sequenceIndex{};
        size_t channelIndex{};
    };
    std::vector<TrackChannelInfo> mChannelLayout;
    std::vector<std::vector<size_t> > mTrackChannelSourceMap;
    bool mNeedsMixdown{ false };

    RecordingSchedule mSchedule{};

    /*! Read by worker threads but unchanging during a stream */
    sampleFormat mFormat{ floatSample };
    double mRate{ 0.0 };
    double mFactor{ 1.0 };
    double mSequenceRate{ 0.0 };
    bool mDetectDropouts{ true };

    double mRingBufferSecs{ 0.0 };
    double mMinSecsToCopy{ 0.0 };

    unsigned long long mLostSamples{ 0 };
    std::vector<std::pair<double, double> > mLostCaptureIntervals;

    // A flag tested and set in one thread, cleared in another.
    wxAtomicInt mRecordingException {};
};
