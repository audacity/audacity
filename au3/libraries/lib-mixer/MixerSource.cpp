/**********************************************************************

  Audacity: A Digital Audio Editor

  @file MixerSource.cpp

  Dominic Mazzoni
  Markus Meyer
  Vaughan Johnson

  Paul Licameli split from Mix.cpp

**********************************************************************/
#include "MixerSource.h"

#include "AudioGraphBuffers.h"
#include "Envelope.h"
#include "Resample.h"
#include "WideSampleSequence.h"
#include "float_cast.h"

namespace {
template<typename T, typename F> std::vector<T>
initVector(size_t dim1, const F& f)
{
    std::vector<T> result(dim1);
    for (auto& row : result) {
        f(row);
    }
    return result;
}

template<typename T> std::vector<std::vector<T> >
initVector(size_t dim1, size_t dim2)
{
    return initVector<std::vector<T> >(dim1,
                                       [dim2](auto& row){ row.resize(dim2); });
}
}

void MixerSource::MakeResamplers()
{
    for (size_t j = 0; j < mnChannels; ++j) {
        mResample[j] = std::make_unique<Resample>(
            mResampleParameters.mHighQuality,
            mResampleParameters.mMinFactor, mResampleParameters.mMaxFactor);
    }
}

namespace {
//Note: The meaning of this function has changed (December 2012)
//Previously this function did something that was close to the opposite (but not entirely accurate).
/** @brief Compute the integral warp factor between two non-warped time points
    *
    * Calculate the relative length increase of the chosen segment from the original sound.
    * So if this time track has a low value (i.e. makes the sound slower), the NEW warped
    * sound will be *longer* than the original sound, so the return value of this function
    * is larger.
    * @param t0 The starting time to calculate from
    * @param t1 The ending time to calculate to
    * @return The relative length increase of the chosen segment from the original sound.
    */
double ComputeWarpFactor(const Envelope& env, double t0, double t1)
{
    return env.AverageOfInverse(t0, t1);
}
}

size_t MixerSource::MixVariableRates(
    unsigned nChannels, const size_t maxOut, float* floatBuffers[])
{
    const auto&[mT0, mT1, mSpeed, _] = *mTimesAndSpeed;
    const bool backwards = (mT1 < mT0);

    const double sequenceRate = mpSeq->GetRate();
    const double initialWarp = mRate / mSpeed / sequenceRate;
    const double tstep = 1.0 / sequenceRate;
    const auto sampleSize = SAMPLE_SIZE(floatSample);
    // Find the last sample
    const auto endPos = [mpSeq = mpSeq, mT1 = mT1, backwards]{
        double endTime = mpSeq->GetEndTime();
        double startTime = mpSeq->GetStartTime();
        const double tEnd = backwards
                            ? std::max(startTime, mT1)
                            : std::min(endTime, mT1);
        return mpSeq->TimeToLongSamples(tEnd);
    }();

    auto pos = mSamplePos;
    auto queueStart = mQueueStart;
    auto queueLen = mQueueLen;

    size_t out = 0;

    /* time is floating point. Sample rate is integer. The number of samples
     * has to be integer, but the multiplication gives a float result, which we
     * round to get an integer result. TODO: is this always right or can it be
     * off by one sometimes? Can we not get this information directly from the
     * clip (which must know) rather than convert the time?
     *
     * LLL:  Not at this time.  While WaveClips provide methods to retrieve the
     *       start and end sample, they do the same float->sampleCount conversion
     *       to calculate the position.
     */

    // Find the time corresponding to the start of the queue, for use with time track
    double t = ((pos).as_long_long()
                + (backwards ? queueLen : -queueLen)) / sequenceRate;

    while (out < maxOut) {
        if (queueLen < (int)sProcessLen) {
            // Shift pending portion to start of the buffer
            for (size_t iChannel = 0; iChannel < nChannels; ++iChannel) {
                const auto queue = mSampleQueue[iChannel].data();
                memmove(queue, &queue[queueStart], (queueLen) * sampleSize);
            }
            queueStart = 0;

            // How far to advance depends on endPos,
            // which is independent of channel
            auto getLen = limitSampleBufferSize(
                sQueueMaxLen - queueLen,
                backwards ? pos - endPos : endPos - pos
                );

            // Nothing to do if past end of play interval
            if (getLen > 0) {
                std::vector<float*> dst;
                for (auto& queue : mSampleQueue) {
                    dst.push_back(queue.data() + queueLen);
                }
                constexpr auto iChannel = 0u;
                if (!mpSeq->GetFloats(
                        iChannel, nChannels, dst.data(), pos, getLen, backwards,
                        FillFormat::fillZero, mMayThrow)) {
                    // Now redundant in case of failure
                    // for (size_t iChannel = 0; iChannel < nChannels; ++iChannel)
                    // memset(dst[i], 0, sizeof(float) * getLen);
                }
                mpSeq->GetEnvelopeValues(
                    mEnvValues.data(), getLen, (pos).as_double() / sequenceRate,
                    backwards);
                for (size_t iChannel = 0; iChannel < nChannels; ++iChannel) {
                    const auto queue = mSampleQueue[iChannel].data();
                    for (decltype(getLen) i = 0; i < getLen; i++) {
                        queue[(queueLen) + i] *= mEnvValues[i];
                    }
                }

                if (backwards) {
                    pos -= getLen;
                } else {
                    pos += getLen;
                }
                queueLen += getLen;
            }
        }

        auto thisProcessLen = sProcessLen;
        bool last = (queueLen < (int)sProcessLen);
        if (last) {
            thisProcessLen = queueLen;
        }

        double factor = initialWarp;
        if (mEnvelope) {
            //TODO-MB: The end time is wrong when the resampler doesn't use all input samples,
            //         as a result of this the warp factor may be slightly wrong, so AudioIO will stop too soon
            //         or too late (resulting in missing sound or inserted silence). This can't be fixed
            //         without changing the way the resampler works, because the number of input samples that will be used
            //         is unpredictable. Maybe it can be compensated later though.
            if (backwards) {
                factor *= ComputeWarpFactor(*mEnvelope,
                                            t - (double)thisProcessLen / sequenceRate + tstep, t + tstep);
            } else {
                factor *= ComputeWarpFactor(*mEnvelope,
                                            t, t + (double)thisProcessLen / sequenceRate);
            }
        }

        std::pair<size_t, size_t> results;
        for (size_t iChannel = 0; iChannel < nChannels; ++iChannel) {
            const auto queue = mSampleQueue[iChannel].data();
            const auto pResample = mResample[iChannel].get();
            const auto pFloat = floatBuffers[iChannel];
            // Assuming that progress of the Resampler depends only
            // on the size of its inputs, this assignment has the same effect on
            // each pass.
            results = pResample->Process(factor,
                                         &queue[queueStart],
                                         thisProcessLen,
                                         last,
                                         // PRL:  Bug2536: crash in soxr happened on Mac, sometimes, when
                                         // maxOut - out == 1 and &pFloat[out + 1] was an unmapped
                                         // address, because soxr, strangely, fetched an 8-byte (misaligned!)
                                         // value from &pFloat[out], but did nothing with it anyway,
                                         // in soxr_output_no_callback.
                                         // Now we make the bug go away by allocating a little more space in
                                         // the buffer than we need.
                                         &pFloat[out],
                                         maxOut - out);
        }

        const auto input_used = results.first;
        queueStart += input_used;
        queueLen -= input_used;
        out += results.second;
        t += (input_used / sequenceRate) * (backwards ? -1 : 1);

        if (last) {
            break;
        }
    }

    assert(out <= maxOut);

    mSamplePos = pos;
    mQueueStart = queueStart;
    mQueueLen = queueLen;
    return out;
}

size_t MixerSource::MixSameRate(unsigned nChannels, const size_t maxOut,
                                float* floatBuffers[])
{
    const auto&[mT0, mT1, _, __] = *mTimesAndSpeed;
    const bool backwards = (mT1 < mT0);
    const auto sequenceRate = mpSeq->GetRate();
    const double tEnd = [mpSeq = mpSeq, mT1 = mT1, backwards]{
        const double sequenceEndTime = mpSeq->GetEndTime();
        const double sequenceStartTime = mpSeq->GetStartTime();
        return backwards
               ? std::max(sequenceStartTime, mT1)
               : std::min(sequenceEndTime, mT1);
    }();

    // This function fetches samples from the input sequences, whatever their
    // formats, as floats; it may also apply envelope values.

    auto pos = mSamplePos;

    const double t = (pos).as_double() / sequenceRate;

    //don't process if we're at the end of the selection or sequence.
    if ((backwards ? t <= tEnd : t >= tEnd)) {
        return 0;
    }
    //if we're about to approach the end of the sequence or selection, figure out how much we need to grab
    const auto slen = limitSampleBufferSize(
        maxOut,
        // PRL: maybe t and tEnd should be given as sampleCount instead to
        // avoid trouble subtracting one large value from another for a small
        // difference
        sampleCount { (backwards ? t - tEnd : tEnd - t) * sequenceRate + 0.5 }
        );

    constexpr auto iChannel = 0u;
    if (!mpSeq->GetFloats(
            iChannel, nChannels, floatBuffers, pos, slen, backwards,
            FillFormat::fillZero, mMayThrow)
        ) {
        // Now redundant in case of failure
        // for (size_t iChannel = 0; iChannel < nChannels; ++iChannel)
        // memset(floatBuffers[iChannel], 0, sizeof(float) * slen);
    }

    mpSeq->GetEnvelopeValues(mEnvValues.data(), slen, t, backwards);

    for (size_t iChannel = 0; iChannel < nChannels; ++iChannel) {
        const auto pFloat = floatBuffers[iChannel];
        for (size_t i = 0; i < slen; i++) {
            pFloat[i] *= mEnvValues[i]; // Track gain control will go here?
        }
    }

    if (backwards) {
        pos -= slen;
    } else {
        pos += slen;
    }

    assert(slen <= maxOut);
    mSamplePos = pos;
    return slen;
}

void MixerSource::ZeroFill(
    size_t produced, size_t max, float& floatBuffer)
{
    assert(produced <= max);
    const auto pFloat = &floatBuffer;
    std::fill(pFloat + produced, pFloat + max, 0);
}

MixerSource::MixerSource(
    const std::shared_ptr<const WideSampleSequence>& seq, size_t bufferSize,
    double rate, const MixerOptions::Warp& options, bool highQuality,
    bool mayThrow, std::shared_ptr<TimesAndSpeed> pTimesAndSpeed)
    : mpSeq{seq}
    , mnChannels{mpSeq->NChannels()}
    , mRate{rate}
    , mEnvelope{options.envelope}
    , mMayThrow{mayThrow}
    , mTimesAndSpeed{move(pTimesAndSpeed)}
    , mSampleQueue{initVector<float>(mnChannels, sQueueMaxLen)}
    , mQueueStart{0}
    , mQueueLen{0}
    , mResampleParameters{highQuality, mpSeq->GetRate(), rate, options}
    , mResample(mnChannels)
    , mEnvValues(std::max(sQueueMaxLen, bufferSize))
{
    assert(mTimesAndSpeed);
    auto t0 = mTimesAndSpeed->mT0;
    mSamplePos = GetSequence().TimeToLongSamples(t0);
    MakeResamplers();
}

MixerSource::~MixerSource() = default;

const WideSampleSequence& MixerSource::GetSequence() const
{
    return *mpSeq;
}

bool MixerSource::AcceptsBuffers(const Buffers& buffers) const
{
    return AcceptsBlockSize(buffers.BufferSize());
}

bool MixerSource::AcceptsBlockSize(size_t blockSize) const
{
    return blockSize <= mEnvValues.size();
}

#define stackAllocate(T, count) static_cast<T*>(alloca(count * sizeof(T)))

std::optional<size_t> MixerSource::Acquire(Buffers& data, size_t bound)
{
    assert(AcceptsBuffers(data));
    assert(AcceptsBlockSize(data.BlockSize()));
    assert(bound <= data.BlockSize());
    assert(data.BlockSize() <= data.Remaining());

    auto&[mT0, mT1, _, mTime] = *mTimesAndSpeed;
    const bool backwards = (mT1 < mT0);
    // TODO: more-than-two-channels
    const auto maxChannels = mMaxChannels = data.Channels();
    const auto limit = std::min<size_t>(mnChannels, maxChannels);
    size_t maxTrack = 0;
    const auto mixed = stackAllocate(size_t, maxChannels);
    const auto pFloats = stackAllocate(float*, limit);
    for (size_t j = 0; j < limit; ++j) {
        pFloats[j] = &data.GetWritePosition(j);
    }
    const auto rate = GetSequence().GetRate();
    auto result = (mResampleParameters.mVariableRates || rate != mRate)
                  ? MixVariableRates(limit, bound, pFloats)
                  : MixSameRate(limit, bound, pFloats);
    maxTrack = std::max(maxTrack, result);
    auto newT = mSamplePos.as_double() / rate;
    if (backwards) {
        mTime = std::min(mTime, newT);
    } else {
        mTime = std::max(mTime, newT);
    }
    for (size_t j = 0; j < limit; ++j) {
        mixed[j] = result;
    }
    // Another pass in case channels of a track did not produce equal numbers
    for (size_t j = 0; j < limit; ++j) {
        const auto pFloat = &data.GetWritePosition(j);
        const auto result = mixed[j];
        ZeroFill(result, maxTrack, *pFloat);
    }

    mLastProduced = maxTrack;
    assert(maxTrack <= bound);
    assert(maxTrack <= data.Remaining());
    assert(maxTrack <= Remaining());
    assert(data.Remaining() > 0);
    assert(bound == 0 || Remaining() == 0 || maxTrack > 0);
    return { mLastProduced };
}

// Does not return a strictly decreasing sequence of values such as to
// provide proof of termination.  Just an indication of whether done or not.
sampleCount MixerSource::Remaining() const
{
    // TODO:  make a more exact calculation of total remaining; see Terminates()
    return mLastProduced;
}

bool MixerSource::Release()
{
    mLastProduced = 0;
    return true;
}

bool MixerSource::Terminates() const
{
    // Not always terminating
    // TODO: return true sometimes, for mixers that never reposition
    // because they are not used in playback.  But then an exact calculation of
    // Remaining() is needed to satisfy the contract, and that is complicated
    // when there is resampling or a time warp.
    return false;
}

void MixerSource::Reposition(double time, bool skipping)
{
    mSamplePos = GetSequence().TimeToLongSamples(time);
    mQueueStart = 0;
    mQueueLen = 0;

    // Bug 2025:  libsoxr 0.1.3, first used in Audacity 2.3.0, crashes with
    // constant rate resampling if you try to reuse the resampler after it has
    // flushed.  Should that be considered a bug in sox?  This works around it.
    // (See also bug 1887, and the same work around in Mixer::Restart().)
    if (skipping) {
        MakeResamplers();
    }
}
