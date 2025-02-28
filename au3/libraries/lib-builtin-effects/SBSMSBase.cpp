/**********************************************************************

Audacity: A Digital Audio Editor

SBSMSBase.cpp

Clayton Otey

This class contains all of the common code for an
effect that uses SBSMS to do its processing (TimeScale)

**********************************************************************/
#if USE_SBSMS
#include "SBSMSBase.h"
#include "EffectOutputTracks.h"

#include <math.h>

#include "LabelTrack.h"
#include "SyncLock.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include "TimeWarper.h"

#include <cassert>

enum {
    SBSMSOutBlockSize = 512
};

class ResampleBuf
{
public:
    ResampleBuf()
    {
        processed = 0;
    }

    ~ResampleBuf()
    {
    }

    bool bPitch;
    ArrayOf<audio> buf;
    double ratio;
    sampleCount processed;
    size_t blockSize;
    long SBSMSBlockSize;
    sampleCount offset;
    sampleCount end;
    ArrayOf<float> leftBuffer;
    ArrayOf<float> rightBuffer;
    WaveChannel* leftTrack;
    WaveChannel* rightTrack;
    std::unique_ptr<SBSMS> sbsms;
    std::unique_ptr<SBSMSInterface> iface;
    ArrayOf<audio> SBSMSBuf;

    // Not required by callbacks, but makes for easier cleanup
    std::unique_ptr<Resampler> resampler;
    std::unique_ptr<SBSMSQuality> quality;
    WaveTrack* outputTrack{};
    WaveChannel* outputLeftChannel{};
    WaveChannel* outputRightChannel{};

    std::exception_ptr mpException {};
};

class SBSMSEffectInterface final : public SBSMSInterfaceSliding
{
public:
    SBSMSEffectInterface(Resampler* resampler,
                         Slide* rateSlide, Slide* pitchSlide,
                         bool bReferenceInput,
                         const SampleCountType samples, long preSamples,
                         SBSMSQuality* quality)
        : SBSMSInterfaceSliding(rateSlide, pitchSlide, bReferenceInput, samples, preSamples, quality)
    {
        this->resampler = resampler;
    }

    virtual ~SBSMSEffectInterface() {}

    long samples(audio* buf, long n)
    {
        return resampler->read(buf, n);
    }

protected:
    Resampler* resampler;
};

long resampleCB(void* cb_data, SBSMSFrame* data)
{
    ResampleBuf* r = (ResampleBuf*)cb_data;

    auto blockSize = limitSampleBufferSize(
        r->leftTrack->GetBestBlockSize(r->offset),
        r->end - r->offset
        );

    // Get the samples from the tracks and put them in the buffers.
    // I don't know if we can safely propagate errors through sbsms, and it
    // does not seem to let us report error codes, so use this roundabout to
    // stop the effect early.
    try {
        r->leftTrack->GetFloats(
            (r->leftBuffer.get()), r->offset, blockSize);
        r->rightTrack->GetFloats(
            (r->rightBuffer.get()), r->offset, blockSize);
    }
    catch (...) {
        // Save the exception object for re-throw when out of the library
        r->mpException = std::current_exception();
        data->size = 0;
        return 0;
    }

    // convert to sbsms audio format
    for (decltype(blockSize) i=0; i < blockSize; i++) {
        r->buf[i][0] = r->leftBuffer[i];
        r->buf[i][1] = r->rightBuffer[i];
    }

    data->buf = r->buf.get();
    data->size = blockSize;
    if (r->bPitch) {
        float t0 = r->processed.as_float() / r->iface->getSamplesToInput();
        float t1 = (r->processed + blockSize).as_float() / r->iface->getSamplesToInput();
        data->ratio0 = r->iface->getStretch(t0);
        data->ratio1 = r->iface->getStretch(t1);
    } else {
        data->ratio0 = r->ratio;
        data->ratio1 = r->ratio;
    }
    r->processed += blockSize;
    r->offset += blockSize;
    return blockSize;
}

long postResampleCB(void* cb_data, SBSMSFrame* data)
{
    ResampleBuf* r = (ResampleBuf*)cb_data;
    auto count = r->sbsms->read(r->iface.get(), r->SBSMSBuf.get(), r->SBSMSBlockSize);
    data->buf = r->SBSMSBuf.get();
    data->size = count;
    data->ratio0 = 1.0 / r->ratio;
    data->ratio1 = 1.0 / r->ratio;
    return count;
}

void SBSMSBase :: setParameters(double rateStartIn, double rateEndIn, double pitchStartIn, double pitchEndIn,
                                SlideType rateSlideTypeIn, SlideType pitchSlideTypeIn,
                                bool bLinkRatePitchIn, bool bRateReferenceInputIn, bool bPitchReferenceInputIn)
{
    this->rateStart = rateStartIn;
    this->rateEnd = rateEndIn;
    this->pitchStart = pitchStartIn;
    this->pitchEnd = pitchEndIn;
    this->bLinkRatePitch = bLinkRatePitchIn;
    this->rateSlideType = rateSlideTypeIn;
    this->pitchSlideType = pitchSlideTypeIn;
    this->bRateReferenceInput = bRateReferenceInputIn;
    this->bPitchReferenceInput = bPitchReferenceInputIn;
}

void SBSMSBase::setParameters(double tempoRatio, double pitchRatio)
{
    setParameters(tempoRatio, tempoRatio, pitchRatio, pitchRatio,
                  SlideConstant, SlideConstant, false, false, false);
}

std::unique_ptr<TimeWarper> createTimeWarper(double t0, double t1, double duration,
                                             double rateStart, double rateEnd, SlideType rateSlideType)
{
    if (rateStart == rateEnd || rateSlideType == SlideConstant) {
        return std::make_unique<LinearTimeWarper>(
            t0, t0, t1, t0 + duration);
    } else if (rateSlideType == SlideLinearInputRate) {
        return std::make_unique<LinearInputRateTimeWarper>(
            t0, t1, rateStart, rateEnd);
    } else if (rateSlideType == SlideLinearOutputRate) {
        return std::make_unique<LinearOutputRateTimeWarper>(
            t0, t1, rateStart, rateEnd);
    } else if (rateSlideType == SlideLinearInputStretch) {
        return std::make_unique<LinearInputStretchTimeWarper>(
            t0, t1, rateStart, rateEnd);
    } else if (rateSlideType == SlideLinearOutputStretch) {
        return std::make_unique<LinearOutputStretchTimeWarper>(
            t0, t1, rateStart, rateEnd);
    } else if (rateSlideType == SlideGeometricInput) {
        return std::make_unique<GeometricInputTimeWarper>(
            t0, t1, rateStart, rateEnd);
    } else if (rateSlideType == SlideGeometricOutput) {
        return std::make_unique<GeometricOutputTimeWarper>(
            t0, t1, rateStart, rateEnd);
    } else {
        return std::make_unique<IdentityTimeWarper>();
    }
}

EffectType SBSMSBase::GetType() const
{
    return EffectTypeProcess;
}

// Labels inside the affected region are moved to match the audio; labels after
// it are shifted along appropriately.
bool SBSMSBase::ProcessLabelTrack(LabelTrack* lt)
{
    auto warper1 = createTimeWarper(
        mT0, mT1, (mT1 - mT0) * mTotalStretch, rateStart, rateEnd, rateSlideType);
    RegionTimeWarper warper{ mT0, mT1, std::move(warper1) };
    lt->WarpLabels(warper);
    return true;
}

double SBSMSBase::getInvertedStretchedTime(double rateStart, double rateEnd, SlideType slideType, double outputTime)
{
    Slide slide(slideType, rateStart, rateEnd, 0);
    return slide.getInverseStretchedTime(outputTime);
}

double SBSMSBase::getRate(double rateStart, double rateEnd, SlideType slideType, double t)
{
    Slide slide(slideType, rateStart, rateEnd, 0);
    return slide.getRate(t);
}

bool SBSMSBase::Process(EffectInstance&, EffectSettings&)
{
    bool bGoodResult = true;

    //Iterate over each track
    //all needed because this effect needs to introduce silence in the group tracks to keep sync
    EffectOutputTracks outputs { *mTracks, GetType(), { { mT0, mT1 } }, true };
    mCurTrackNum = 0;

    double maxDuration = 0.0;

    Slide rateSlide(rateSlideType, rateStart, rateEnd);
    Slide pitchSlide(pitchSlideType, pitchStart, pitchEnd);
    mTotalStretch = rateSlide.getTotalStretch();

    outputs.Get().Any().VisitWhile(bGoodResult,
                                   [&](auto&& fallthrough){
        return [&](LabelTrack& lt) {
            if (!(lt.GetSelected() || SyncLock::IsSyncLockSelected(lt))) {
                return fallthrough();
            }
            if (!ProcessLabelTrack(&lt)) {
                bGoodResult = false;
            }
        };
    },
                                   [&](auto&& fallthrough){
        return [&](WaveTrack& track) {
            if (!track.GetSelected()) {
                return fallthrough();
            }

            // Process only if the right marker is to the right of the left marker
            if (mT1 > mT0) {
                const auto start = track.TimeToLongSamples(mT0);
                const auto end = track.TimeToLongSamples(mT1);

                // TODO: more-than-two-channels
                auto channels = track.Channels();
                const auto leftTrack = (*channels.begin()).get();
                const auto rightTrack = (channels.size() > 1)
                                        ? (*++channels.first).get()
                                        : nullptr;
                if (rightTrack) {
                    mCurTrackNum++; // Increment for rightTrack, too.
                }
                // SBSMS has a fixed sample rate - we just convert to its sample
                // rate and then convert back
                const float srTrack = track.GetRate();
                const float srProcess = bLinkRatePitch ? srTrack : 44100.0;

                // the resampler needs a callback to supply its samples
                ResampleBuf rb;
                const auto maxBlockSize = track.GetMaxBlockSize();
                rb.blockSize = maxBlockSize;
                rb.buf.reinit(rb.blockSize, true);
                rb.leftTrack = leftTrack;
                rb.rightTrack = rightTrack ? rightTrack : leftTrack;
                rb.leftBuffer.reinit(maxBlockSize, true);
                rb.rightBuffer.reinit(maxBlockSize, true);

                // Samples in selection
                const auto samplesIn = end - start;

                // Samples for SBSMS to process after resampling
                const auto samplesToProcess = static_cast<sampleCount>(
                    samplesIn.as_float() * (srProcess / srTrack));

                SlideType outSlideType;
                SBSMSResampleCB outResampleCB;

                if (bLinkRatePitch) {
                    rb.bPitch = true;
                    outSlideType = rateSlideType;
                    outResampleCB = resampleCB;
                    rb.offset = start;
                    rb.end = end;
                    // Third party library has its own type alias, check it
                    static_assert(sizeof(sampleCount::type)
                                  <= sizeof(_sbsms_::SampleCountType),
                                  "Type _sbsms_::SampleCountType is too narrow to hold a sampleCount");
                    rb.iface = std::make_unique<SBSMSInterfaceSliding>(
                        &rateSlide, &pitchSlide, bPitchReferenceInput,
                        static_cast<_sbsms_::SampleCountType>(
                            samplesToProcess.as_long_long()),
                        0, nullptr);
                } else {
                    rb.bPitch = false;
                    outSlideType
                        =(srProcess == srTrack ? SlideIdentity : SlideConstant);
                    outResampleCB = postResampleCB;
                    rb.ratio = srProcess / srTrack;
                    rb.quality = std::make_unique<SBSMSQuality>(&SBSMSQualityStandard);
                    rb.resampler = std::make_unique<Resampler>(resampleCB, &rb,
                                                               srProcess == srTrack ? SlideIdentity : SlideConstant);
                    rb.sbsms = std::make_unique<SBSMS>(
                        rightTrack ? 2 : 1, rb.quality.get(), true);
                    rb.SBSMSBlockSize = rb.sbsms->getInputFrameSize();
                    rb.SBSMSBuf.reinit(static_cast<size_t>(rb.SBSMSBlockSize), true);
                    rb.offset = start;
                    rb.end = end;
                    rb.iface = std::make_unique<SBSMSEffectInterface>(
                        rb.resampler.get(), &rateSlide, &pitchSlide,
                        bPitchReferenceInput,
                        static_cast<_sbsms_::SampleCountType>(
                            samplesToProcess.as_long_long()),
                        0,
                        rb.quality.get());
                }

                Resampler resampler(outResampleCB, &rb, outSlideType);

                audio outBuf[SBSMSOutBlockSize];
                float outBufLeft[2 * SBSMSOutBlockSize];
                float outBufRight[2 * SBSMSOutBlockSize];

                // Samples in output after SBSMS
                const sampleCount samplesToOutput = rb.iface->getSamplesToOutput();

                // Samples in output after resampling back
                const auto samplesOut = static_cast<sampleCount>(
                    samplesToOutput.as_float() * (srTrack / srProcess));

                // Duration in track time
                const double duration = (mT1 - mT0) * mTotalStretch;

                if (duration > maxDuration) {
                    maxDuration = duration;
                }

                const auto warper = createTimeWarper(
                    mT0, mT1, maxDuration, rateStart, rateEnd, rateSlideType);

                WaveTrack::Holder outputTrack = track.EmptyCopy();
                auto iter = outputTrack->Channels().begin();
                rb.outputTrack = outputTrack.get();
                rb.outputLeftChannel = (*iter++).get();
                if (rightTrack) {
                    rb.outputRightChannel = (*iter).get();
                }

                long pos = 0;
                long outputCount = -1;

                // process
                while (pos < samplesOut && outputCount) {
                    const auto frames
                        =limitSampleBufferSize(SBSMSOutBlockSize, samplesOut - pos);

                    outputCount = resampler.read(outBuf, frames);
                    for (int i = 0; i < outputCount; ++i) {
                        outBufLeft[i] = outBuf[i][0];
                        if (rightTrack) {
                            outBufRight[i] = outBuf[i][1];
                        }
                    }
                    pos += outputCount;
                    rb.outputLeftChannel->Append(
                        (samplePtr)outBufLeft, floatSample, outputCount);
                    if (rightTrack) {
                        rb.outputRightChannel->Append(
                            (samplePtr)outBufRight, floatSample, outputCount);
                    }

                    double frac = static_cast<double>(pos) / samplesOut.as_double();
                    int nWhichTrack = mCurTrackNum;
                    if (rightTrack) {
                        nWhichTrack = 2 * (mCurTrackNum / 2);
                        if (frac < 0.5) {
                            // Show twice as far for each track,
                            // because we're doing 2 at once.
                            frac *= 2.0;
                        } else {
                            nWhichTrack++;
                            frac -= 0.5;
                            // Show twice as far for each track,
                            // because we're doing 2 at once.
                            frac *= 2.0;
                        }
                    }
                    if (TrackProgress(nWhichTrack, frac)) {
                        bGoodResult = false;
                        return;
                    }
                }

                {
                    auto pException = rb.mpException;
                    rb.mpException = {};
                    if (pException) {
                        std::rethrow_exception(pException);
                    }
                }

                rb.outputTrack->Flush();
                Finalize(track, *outputTrack, *warper);
            }
            mCurTrackNum++;
        };
    },
                                   [&](Track& t) {
        if (SyncLock::IsSyncLockSelected(t)) {
            t.SyncLockAdjust(mT1, mT0 + (mT1 - mT0) * mTotalStretch);
        }
    }
                                   );

    if (bGoodResult) {
        outputs.Commit();
    }

    return bGoodResult;
}

void SBSMSBase::Finalize(
    WaveTrack& orig, const WaveTrack& out, const TimeWarper& warper)
{
    assert(orig.NChannels() == out.NChannels());
    // Silenced samples will be inserted in gaps between clips, so capture where these
    // gaps are for later deletion
    std::vector<std::pair<double, double> > gaps;
    double last = mT0;
    auto clips = orig.SortedIntervalArray();
    auto front = clips.front();
    auto back = clips.back();
    for (auto& clip : clips) {
        auto st = clip->GetPlayStartTime();
        auto et = clip->GetPlayEndTime();

        if (st >= mT0 || et < mT1) {
            if (mT0 < st && clip == front) {
                gaps.push_back(std::make_pair(mT0, st));
            } else if (last < st && mT0 <= last) {
                gaps.push_back(std::make_pair(last, st));
            }

            if (et < mT1 && clip == back) {
                gaps.push_back(std::make_pair(et, mT1));
            }
        }
        last = et;
    }

    // Take the output track and insert it in place of the original sample data
    orig.ClearAndPaste(mT0, mT1, out, true, true, &warper);

    // Finally, recreate the gaps
    for (auto gap : gaps) {
        const auto st = orig.SnapToSample(gap.first);
        const auto et = orig.SnapToSample(gap.second);
        if (st >= mT0 && et <= mT1 && st != et) {
            orig.SplitDelete(warper.Warp(st), warper.Warp(et));
        }
    }
}

#endif
