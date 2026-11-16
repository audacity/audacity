/*
 * Audacity: A Digital Audio Editor
 */
#include "CaptureSink.h"

#include "AudioIOListener.h"

#include "au3-exceptions/AudacityException.h"
#include "au3-math/Resample.h"
#include "au3-project/Project.h"
#include "au3-transactions/TransactionScope.h"
#include "au3-wave-track/Sequence.h"

#include <wx/crt.h>

#include <algorithm>
#include <cmath>
#include <limits>
#include <mutex>
#include <numeric>
#include <optional>

CaptureSink::CaptureSink() = default;
CaptureSink::~CaptureSink() = default;

void CaptureSink::Reset(RecordableSequences sequences, bool detectDropouts)
{
    mSequences = std::move(sequences);
    mLostSamples = 0;
    mLostCaptureIntervals.clear();
    mDetectDropouts = detectDropouts;
    ClearBuffers();
    ResetRouting();
}

void CaptureSink::ResetRouting()
{
    mChannelLayout.clear();
    mTrackChannelSourceMap.clear();
    mNeedsMixdown = false;
}

void CaptureSink::ConfigureRouting(size_t inputChannelsCount)
{
    ResetRouting();

    if (mSequences.empty() || inputChannelsCount == 0) {
        return;
    }

    for (size_t trackIdx = 0; trackIdx < mSequences.size(); ++trackIdx) {
        const auto nChannels = mSequences[trackIdx]->NChannels();
        for (size_t channelIdx = 0; channelIdx < nChannels; ++channelIdx) {
            mChannelLayout.push_back({ trackIdx, channelIdx });
        }
    }

    if (mChannelLayout.empty()) {
        return;
    }

    const auto captureChannelsCount = mChannelLayout.size();

    mTrackChannelSourceMap.resize(captureChannelsCount);

    // Only allow up/down mixing for mono<->stereo cases
    const bool canMix = (inputChannelsCount <= 2 && captureChannelsCount <= 2);

    if (canMix && inputChannelsCount == 2 && captureChannelsCount == 1) {
        // Stereo input -> Mono track: mix both input channels to mono
        mNeedsMixdown = true;
        mTrackChannelSourceMap[0].push_back(0);
        mTrackChannelSourceMap[0].push_back(1);
    } else if (canMix && inputChannelsCount == 1 && captureChannelsCount == 2) {
        // Mono input -> Stereo track: duplicate mono to both channels
        mNeedsMixdown = false;
        mTrackChannelSourceMap[0].push_back(0);
        mTrackChannelSourceMap[1].push_back(0);
    } else {
        // 1 to 1 mapping
        mNeedsMixdown = false;
        for (size_t t = 0; t < std::min(captureChannelsCount, inputChannelsCount); ++t) {
            mTrackChannelSourceMap[t].push_back(t);
        }
    }
}

void CaptureSink::InitBufferTimes(size_t numChannels)
{
    mRingBufferSecs = 4.5 + 0.5 * std::min(size_t(16), numChannels);
    mMinSecsToCopy = 0.2;
}

void CaptureSink::HalveBufferTimes()
{
    mRingBufferSecs *= 0.5;
    mMinSecsToCopy *= 0.5;
}

bool CaptureSink::AllocateBuffers(size_t numChannels, sampleFormat captureFormat, double deviceRate, double factor,
                                  double sequenceRate)
{
    mFormat = captureFormat;
    mRate = deviceRate;
    mFactor = factor;
    mSequenceRate = sequenceRate;

    // For every input channel we allocate a ring buffer of a few seconds
    const auto captureBufferSize = (size_t)(mRate * mRingBufferSecs + 0.5);

    // In the extraordinarily rare case that we can't even afford
    // 100 samples, just give up.
    if (captureBufferSize < 100) {
        return false;
    }

    mBuffers.resize(0);
    mBuffers.resize(numChannels);
    mResample.resize(0);
    mResample.resize(numChannels);

    for (unsigned int i = 0; i < numChannels; ++i) {
        mBuffers[i] = std::make_unique<RingBuffer>(mFormat, captureBufferSize);
        // constant rate resampling
        mResample[i] = std::make_unique<Resample>(true, mFactor, mFactor);
    }
    return true;
}

void CaptureSink::ClearBuffers()
{
    mBuffers.clear();
    mResample.clear();
}

void CaptureSink::Clear()
{
    mSequences.clear();
    ResetRouting();
}

void CaptureSink::ClearRecordingException()
{
    if (mRecordingException) {
        wxAtomicDec(mRecordingException);
    }
}

size_t CaptureSink::CommonlyAvail() const
{
    return std::accumulate(mBuffers.begin(), mBuffers.end(),
                           std::numeric_limits<size_t>::max(),
                           [](auto value, auto& pBuffer){
        return std::min(value, pBuffer->AvailForGet());
    });
}

size_t CaptureSink::Push(constSamplePtr inputBuffer, size_t numCaptureChannels, size_t framesPerBuffer, double sequenceTime,
                         bool inputError, bool simulateError, float* tempFloats)
{
    numCaptureChannels = std::min(numCaptureChannels, mBuffers.size());
    if (numCaptureChannels == 0) {
        return 0;
    }

    size_t len = framesPerBuffer;
    for (unsigned t = 0; t < numCaptureChannels; t++) {
        len = std::min(len, mBuffers[t]->AvailForPut());
    }

    if (simulateError) {
        len = 0;
    }

    // A different symptom is that len < framesPerBuffer because
    // the other thread, executing Drain, isn't consuming fast
    // enough from mBuffers; maybe it's CPU-bound, or maybe the
    // storage device it writes is too slow
    if (mDetectDropouts && (inputError || len < framesPerBuffer)) {
        // Assume that any good partial buffer should be written leftmost
        // and zeroes will be padded after; label the zeroes.
        auto start = sequenceTime + len / mRate + mSchedule.mLatencyCompensation;
        auto duration = (framesPerBuffer - len) / mRate;
        auto pLast = mLostCaptureIntervals.empty()
                     ? nullptr : &mLostCaptureIntervals.back();
        if (pLast
            && fabs(pLast->first + pLast->second - start) < 0.5 / mRate) {
            // Make one bigger interval, not two butting intervals
            pLast->second = start + duration - pLast->first;
        } else {
            mLostCaptureIntervals.emplace_back(start, duration);
        }
    }

    if (len < framesPerBuffer) {
        mLostSamples += (framesPerBuffer - len);
        wxPrintf(wxT("lost %d samples\n"), (int)(framesPerBuffer - len));
    }

    if (len <= 0) {
        return 0;
    }

    for (unsigned t = 0; t < numCaptureChannels; t++) {
        // dmazzoni:
        // Un-interleave.  Ugly special-case code required because the
        // capture channels could be in three different sample formats;
        // it'd be nice to be able to call CopySamples, but it can't
        // handle multiplying by the gain and then clipping.  Bummer.

        switch (mFormat) {
        case floatSample: {
            auto inputFloats = (const float*)inputBuffer;
            for (unsigned i = 0; i < len; i++) {
                tempFloats[i]
                    =inputFloats[numCaptureChannels * i + t];
            }
        } break;
        case int24Sample:
            // We should never get here. Audacity's int24Sample format
            // is different from PortAudio's sample format and so we
            // make PortAudio return float samples when recording in
            // 24-bit samples.
            wxASSERT(false);
            break;
        case int16Sample: {
            auto inputShorts = (const short*)inputBuffer;
            short* tempShorts = (short*)tempFloats;
            for ( unsigned i = 0; i < len; i++) {
                float tmp = inputShorts[numCaptureChannels * i + t];
                tmp = std::clamp(tmp, -32768.0f, 32767.0f);
                tempShorts[i] = (short)(tmp);
            }
        } break;
        } // switch

        // JKC: mFormat must be for samples with sizeof(float) or
        // fewer bytes (because tempFloats is sized for floats).  All
        // formats are 2 or 4 bytes, so we are OK.
        const auto put
            =mBuffers[t]->Put(
                  (samplePtr)tempFloats, mFormat, len);
        // wxASSERT(put == len);
        // but we can't assert in this thread
        wxUnusedVar(put);
        mBuffers[t]->Flush();
    }

    return len;
}

void CaptureSink::Drain(bool force, bool streamActive, AudioIOListener* listener, const std::function<void()>& stopTransport)
{
    if (mRecordingException || mSequences.empty()) {
        return;
    }

    // The sequences might be locked during project serialization (e.g. on autosave): skip this turn
    std::unique_lock recordingLock(Sequence::RecordingMutex(), std::try_to_lock);
    if (!recordingLock.owns_lock()) {
        return;
    }

    if (mTrackChannelSourceMap.empty() && !mBuffers.empty()) {
        ConfigureRouting(mBuffers.size());
    }

    auto delayedHandler = [this, stopTransport] ( AudacityException* pException ) {
        // In the main thread, stop recording
        // This is one place where the application handles disk
        // exhaustion exceptions from RecordableSequence operations, without
        // rolling back to the last pushed undo state.  Instead, partial recording
        // results are pushed as a NEW undo state.  For this reason, as
        // commented elsewhere, we want an exception safety guarantee for
        // the output RecordableSequences, after the failed append operation, that
        // the sequences remain as they were after the previous successful
        // (block-level) appends.

        // Note that the Flush in StopStream() may throw another exception,
        // but StopStream() contains that exception, and the logic in
        // AudacityException::DelayedHandlerAction prevents redundant message
        // boxes.
        stopTransport();

        DefaultDelayedHandlerAction(pException);
        for (auto& pSequence: mSequences) {
            pSequence->RepairChannels();
        }
    };

    GuardedCall([&] {
        // start record buffering
        const auto avail = CommonlyAvail(); // samples
        const auto remainingTime = std::max(0.0, mSchedule.ToConsume());
        // This may be a very big double number:
        const auto remainingSamples = remainingTime * mRate;
        bool latencyCorrected = true;

        const double deltat = avail / mRate;

        if (force || deltat >= mMinSecsToCopy) {
            // Append captured samples to the end of the RecordableSequences.
            // (WaveTracks have their own buffering for efficiency.)
            const size_t hardwareChannels = mBuffers.size();
            if (hardwareChannels == 0 || mChannelLayout.empty()) {
                return;
            }

            struct CapturedChannelData {
                SampleBuffer buffer;
                size_t size { 0 };
                sampleFormat format { floatSample };
            };

            std::vector<CapturedChannelData> captured(hardwareChannels);
            const bool forceFloatCapture = mNeedsMixdown
                                           || !mSchedule.mCrossfadeData.empty();

            for (size_t i = 0; i < hardwareChannels; ++i) {
                size_t discarded = 0;

                if (!mSchedule.mLatencyCorrected) {
                    const auto correction = mSchedule.TotalCorrection();
                    // Leftward shift
                    // discard some samples from the ring buffers.
                    if (correction < 0) {
                        size_t size = floor(mSchedule.ToDiscard() * mRate);
                        // The ring buffer might have grown concurrently -- don't discard more
                        // than the "avail" value noted above.
                        discarded = mBuffers[i]->Discard(std::min(avail, size));

                        if (discarded < size) {
                            // We need to visit this again to complete the
                            // discarding.
                            latencyCorrected = false;
                        }
                    }
                }

                if (avail <= discarded) {
                    captured[i].size = 0;
                    continue;
                }

                size_t toGet = avail - discarded;
                SampleBuffer temp;
                size_t size = 0;
                sampleFormat format = mFormat;

                if (mFactor == 1.0) {
                    // Take captured samples directly
                    size = toGet;
                    // Change to float for crossfade/mix calculation
                    format = forceFloatCapture ? floatSample : mFormat;
                    temp.Allocate(size, format);
                    const auto got = mBuffers[i]->Get(temp.ptr(), format, toGet);
                    wxUnusedVar(got);
                    if (double(size) > remainingSamples) {
                        size = floor(remainingSamples);
                    }
                } else {
                    size = lrint(toGet * mFactor);
                    format = floatSample;
                    SampleBuffer temp1(toGet, floatSample);
                    temp.Allocate(size, format);
                    const auto got = mBuffers[i]->Get(temp1.ptr(), floatSample, toGet);
                    wxUnusedVar(got);
                    /* we are re-sampling on the fly. The last resampling call
                     * must flush any samples left in the rate conversion buffer
                     * so that they get recorded
                     */
                    if (toGet > 0) {
                        if (double(toGet) > remainingSamples) {
                            toGet = floor(remainingSamples);
                        }
                        const auto results = mResample[i]->Process(mFactor,
                                                                   (float*)temp1.ptr(), toGet,
                                                                   !streamActive,
                                                                   (float*)temp.ptr(), size);
                        size = results.second;
                    }
                }

                captured[i].buffer = std::move(temp);
                captured[i].size = size;
                captured[i].format = format;
            }

            const auto ensureFloatView = [&](size_t srcIndex, size_t sampleCount) -> const float* {
                auto& src = captured[srcIndex];
                if (src.format != floatSample) {
                    SampleBuffer converted(sampleCount, floatSample);
                    SamplesToFloats(src.buffer.ptr(), src.format,
                                    reinterpret_cast<float*>(converted.ptr()),
                                    sampleCount, 1, 1);
                    src.buffer = std::move(converted);
                    src.format = floatSample;
                    src.size = sampleCount;
                }
                return reinterpret_cast<const float*>(src.buffer.ptr());
            };

            bool newBlocks = false;
            size_t trackChannelIndex = 0;
            const auto correction = mSchedule.TotalCorrection();

            for (const auto& info : mChannelLayout) {
                if (trackChannelIndex >= mTrackChannelSourceMap.size()) {
                    break;
                }

                const auto& sources = mTrackChannelSourceMap[trackChannelIndex];
                if (sources.empty()) {
                    ++trackChannelIndex;
                    continue;
                }

                const auto firstSource = sources.front();
                if (firstSource >= captured.size()) {
                    ++trackChannelIndex;
                    continue;
                }

                size_t size = captured[firstSource].size;
                sampleFormat format = captured[firstSource].format;
                constSamplePtr dataPtr = captured[firstSource].buffer.ptr();
                SampleBuffer mixBuffer;
                SampleBuffer floatConversionBuffer;

                if (sources.size() > 1) {
                    size = captured[firstSource].size;
                    for (auto srcIndex : sources) {
                        if (srcIndex >= captured.size()) {
                            size = 0;
                            break;
                        }
                        size = std::min(size, captured[srcIndex].size);
                    }

                    if (size > 0) {
                        mixBuffer.Allocate(size, floatSample);
                        auto dest = reinterpret_cast<float*>(mixBuffer.ptr());
                        std::fill(dest, dest + size, 0.0f);

                        for (auto srcIndex : sources) {
                            const auto* srcPtr = ensureFloatView(srcIndex, size);
                            for (size_t s = 0; s < size; ++s) {
                                dest[s] += srcPtr[s];
                            }
                        }

                        const float gain = 1.0f / static_cast<float>(sources.size());
                        for (size_t s = 0; s < size; ++s) {
                            dest[s] *= gain;
                        }

                        dataPtr = mixBuffer.ptr();
                        format = floatSample;
                    }
                }

                if (!dataPtr || size == 0) {
                    ++trackChannelIndex;
                    continue;
                }

                if (double(size) > remainingSamples) {
                    size = floor(remainingSamples);
                }

                auto& sequence = mSequences[info.sequenceIndex];
                if (!sequence) {
                    ++trackChannelIndex;
                    continue;
                }

                if (!mSchedule.mLatencyCorrected && correction >= 0) {
                    // Rightward shift
                    // Once only (per sequence per recording), insert some initial
                    // silence.
                    const auto silenceSamples = static_cast<size_t>(floor(correction * mRate * mFactor));
                    if (silenceSamples > 0) {
                        SampleBuffer silence(silenceSamples, mFormat);
                        ClearSamples(silence.ptr(), mFormat, 0, silenceSamples);
                        newBlocks = sequence->Append(info.channelIndex, silence.ptr(), mFormat,
                                                     silenceSamples, 1, narrowestSampleFormat) || newBlocks;
                    }
                }

                const bool needsCrossfade
                    = trackChannelIndex < mSchedule.mCrossfadeData.size()
                      && !mSchedule.mCrossfadeData[trackChannelIndex].empty();

                if (needsCrossfade && format != floatSample) {
                    floatConversionBuffer.Allocate(size, floatSample);
                    SamplesToFloats(dataPtr, format,
                                    reinterpret_cast<float*>(floatConversionBuffer.ptr()),
                                    size, 1, 1);
                    dataPtr = floatConversionBuffer.ptr();
                    format = floatSample;
                }

                if (needsCrossfade) {
                    auto* dst = reinterpret_cast<float*>(const_cast<samplePtr>(dataPtr));
                    const auto& data = mSchedule.mCrossfadeData[trackChannelIndex];
                    auto crossfadeStart = static_cast<size_t>(floor(mSchedule.Consumed() * mSequenceRate));
                    if (crossfadeStart < data.size()) {
                        const auto crossfadeLength = std::min<size_t>(size, data.size() - crossfadeStart);
                        auto ratio = double(crossfadeStart) / data.size();
                        const auto ratioStep = 1.0 / data.size();
                        const float* src = data.data() + crossfadeStart;
                        for (size_t s = 0; s < crossfadeLength; ++s) {
                            dst[s] = ratio * dst[s] + (1.0 - ratio) * src[s];
                            ratio += ratioStep;
                        }
                    }
                }

                // Now append
                // see comment in second handler about guarantee
                newBlocks = sequence->Append(info.channelIndex, dataPtr, format,
                                             size, 1, narrowestSampleFormat) || newBlocks;

                ++trackChannelIndex;
            } // end loop over capture channels

            // Now update the recording schedule position
            mSchedule.mPosition += avail / mRate;
            mSchedule.mLatencyCorrected = latencyCorrected;

            if (listener && newBlocks) {
                listener->OnAudioIONewBlocks();
            }
        }
        // end of record buffering
    },
                // handler
                [this] ( AudacityException* pException ) {
        if (pException) {
            // So that we don't attempt to fill the recording buffer again
            // before the main thread stops recording
            SetRecordingException();
            return;
        } else {
            // Don't want to intercept other exceptions (?)
            throw;
        }
    },
                delayedHandler);
}

void CaptureSink::Finish(AudacityProject* project, AudioIOListener* listener)
{
    if (mSequences.empty()) {
        return;
    }

    ClearBuffers();

    //
    // We only apply latency correction when we actually played back
    // sequences during the recording. If we did not play back sequences,
    // there's nothing we could be out of sync with. This also covers the
    // case that we do not apply latency correction when recording the
    // first sequence in a project.
    //

    for (auto& sequence : mSequences) {
        // The calls to Flush
        // may cause exceptions because of exhaustion of disk space.
        // Stop those exceptions here, or else they propagate through too
        // many parts of Audacity that are not effects or editing
        // operations.  GuardedCall ensures that the user sees a warning.

        // Also be sure to Flush each sequence, at the top of the
        // guarded call, relying on the guarantee that the sequence will be
        // left in a flushed state, though the append buffer may be lost.

        GuardedCall([&] {
            // use No-fail-guarantee that sequence is flushed,
            // Partial-guarantee that some initial length of the recording
            // is saved.
            // See comments in Drain().
            sequence->Flush();
        });
    }

    if (!mLostCaptureIntervals.empty()) {
        // This scope may combine many insertions of silence
        // into one transaction, lessening the number of checkpoints
        std::optional<TransactionScope> pScope;
        if (project) {
            pScope.emplace(*project, "Dropouts");
        }
        for (auto& interval : mLostCaptureIntervals) {
            auto& start = interval.first;
            auto duration = interval.second;
            for (auto& sequence : mSequences) {
                GuardedCall([&] {
                    sequence->InsertSilence(start, duration);
                });
            }
        }
        if (pScope) {
            pScope->Commit();
        }
    }

    if (listener) {
        listener->OnCommitRecording();
    }
}
