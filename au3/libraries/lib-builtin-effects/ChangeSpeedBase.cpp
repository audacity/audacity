#include "ChangeSpeedBase.h"
#include "EffectOutputTracks.h"
#include "LabelTrack.h"
#include "NumericConverterFormats.h"
#include "Prefs.h"
#include "Resample.h"
#include "ShuttleAutomation.h"
#include "SyncLock.h"
#include "TimeWarper.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include <cmath>

// Soundtouch is not reasonable below -99% or above 3000%.

const EffectParameterMethods& ChangeSpeedBase::Parameters() const
{
    static CapturedParameters<ChangeSpeedBase, Percentage> parameters;
    return parameters;
}

const ComponentInterfaceSymbol ChangeSpeedBase::Symbol { XO(
                                                             "Change Speed and Pitch") };

ChangeSpeedBase::ChangeSpeedBase()
{
    Parameters().Reset(*this);

    mFromVinyl = kVinyl_33AndAThird;
    mToVinyl = kVinyl_33AndAThird;
    mFromLength = 0.0;
    mToLength = 0.0;
    mFormat = NumericConverterFormats::DefaultSelectionFormat().Internal();
    mbLoopDetect = false;

    SetLinearEffectFlag(true);
}

ChangeSpeedBase::~ChangeSpeedBase()
{
}

ComponentInterfaceSymbol ChangeSpeedBase::GetSymbol() const
{
    return Symbol;
}

TranslatableString ChangeSpeedBase::GetDescription() const
{
    return XO("Changes the speed of a track, also changing its pitch");
}

ManualPageID ChangeSpeedBase::ManualPage() const
{
    return L"Change_Speed";
}

EffectType ChangeSpeedBase::GetType() const
{
    return EffectTypeProcess;
}

OptionalMessage
ChangeSpeedBase::LoadFactoryDefaults(EffectSettings& settings) const
{
    // To do: externalize state so const_cast isn't needed
    return const_cast<ChangeSpeedBase&>(*this).DoLoadFactoryDefaults(settings);
}

OptionalMessage ChangeSpeedBase::DoLoadFactoryDefaults(EffectSettings& settings)
{
    mFromVinyl = kVinyl_33AndAThird;
    mFormat = NumericConverterFormats::DefaultSelectionFormat().Internal();

    return Effect::LoadFactoryDefaults(settings);
}

bool ChangeSpeedBase::CheckWhetherSkipEffect(const EffectSettings&) const
{
    return m_PercentChange == 0.0;
}

double ChangeSpeedBase::CalcPreviewInputLength(
    const EffectSettings&, double previewLength) const
{
    return previewLength * (100.0 + m_PercentChange) / 100.0;
}

bool ChangeSpeedBase::Init()
{
    // The selection might have changed since the last time ChangeSpeedBase
    // was invoked, so recalculate the Length parameters.
    mFromLength = mT1 - mT0;
    return true;
}

auto ChangeSpeedBase::FindGaps(
    const WaveTrack& track, const double curT0, const double curT1) -> Gaps
{
    // Silenced samples will be inserted in gaps between clips, so capture where
    // these gaps are for later deletion
    Gaps gaps;
    const auto newGap = [&](double st, double et) {
        gaps.emplace_back(track.SnapToSample(st), track.SnapToSample(et));
    };
    double last = curT0;
    auto clips = track.SortedIntervalArray();
    auto front = clips.front();
    auto back = clips.back();
    for (auto& clip : clips) {
        auto st = clip->GetPlayStartTime();
        auto et = clip->GetPlayEndTime();
        if (st >= curT0 || et < curT1) {
            if (curT0 < st && clip == front) {
                newGap(curT0, st);
            } else if (last < st && curT0 <= last) {
                newGap(last, st);
            }
            if (et < curT1 && clip == back) {
                newGap(et, curT1);
            }
        }
        last = et;
    }
    return gaps;
}

bool ChangeSpeedBase::Process(EffectInstance&, EffectSettings&)
{
    // Similar to SoundTouchBase::Process()

    // Iterate over each track.
    // All needed because this effect needs to introduce
    // silence in the sync-lock group tracks to keep sync
    EffectOutputTracks outputs { *mTracks, GetType(), { { mT0, mT1 } }, true };
    bool bGoodResult = true;

    mCurTrackNum = 0;

    mFactor = 100.0 / (100.0 + m_PercentChange);

    outputs.Get().Any().VisitWhile(
        bGoodResult,
        [&](LabelTrack& lt) {
        if (SyncLock::IsSelectedOrSyncLockSelected(lt)) {
            if (!ProcessLabelTrack(&lt)) {
                bGoodResult = false;
            }
        }
    },
        [&](auto&& fallthrough) {
        return [&](WaveTrack& outWaveTrack) {
            if (!outWaveTrack.GetSelected()) {
                return fallthrough();
            }

            // Get start and end times from track
            mCurT0 = outWaveTrack.GetStartTime();
            mCurT1 = outWaveTrack.GetEndTime();

            // Set the current bounds to whichever left marker is
            // greater and whichever right marker is less:
            mCurT0 = std::max(mT0, mCurT0);
            mCurT1 = std::min(mT1, mCurT1);

            // Process only if the right marker is to the right of the left
            // marker
            if (mCurT1 > mCurT0) {
                // Transform the marker timepoints to samples
                auto start = outWaveTrack.TimeToLongSamples(mCurT0);
                auto end = outWaveTrack.TimeToLongSamples(mCurT1);

                const auto gaps = FindGaps(outWaveTrack, mCurT0, mCurT1);

                auto pNewTrack = outWaveTrack.EmptyCopy();
                auto iter = pNewTrack->Channels().begin();
                for (const auto pChannel : outWaveTrack.Channels()) {
                    // ProcessOne() (implemented below) processes a single channel
                    if (ProcessOne(*pChannel, **iter++, start, end)) {
                        ++mCurTrackNum;
                    } else {
                        pNewTrack.reset();
                        break;
                    }
                }
                if (!pNewTrack) {
                    bGoodResult = false;
                    return;
                }
                pNewTrack->Flush();

                const double newLength = pNewTrack->GetEndTime();
                const LinearTimeWarper warper { mCurT0, mCurT0, mCurT1,
                                                mCurT0 + newLength };

                outWaveTrack.ClearAndPaste(
                    mCurT0, mCurT1, *pNewTrack, true, true, &warper);

                // Finally, recreate the gaps
                for (const auto [st, et] : gaps) {
                    if (st >= mCurT0 && et <= mCurT1 && st != et) {
                        outWaveTrack.SplitDelete(warper.Warp(st), warper.Warp(et));
                    }
                }
            } else {
                mCurTrackNum += outWaveTrack.NChannels();
            }
        };
    },
        [&](Track& t) {
        if (SyncLock::IsSyncLockSelected(t)) {
            t.SyncLockAdjust(mT1, mT0 + (mT1 - mT0) * mFactor);
        }
    });

    if (bGoodResult) {
        outputs.Commit();
    }

    // Update selection.
    mT1 = mT0 + (((mT1 - mT0) * 100.0) / (100.0 + m_PercentChange));

    return bGoodResult;
}

// Labels are time-scaled linearly inside the affected region, and labels after
// the region are shifted along according to how the region size changed.
bool ChangeSpeedBase::ProcessLabelTrack(LabelTrack* lt)
{
    RegionTimeWarper warper { mT0, mT1,
                              std::make_unique<LinearTimeWarper>(
                                  mT0, mT0, mT1, mT0 + (mT1 - mT0) * mFactor) };
    lt->WarpLabels(warper);
    return true;
}

// ProcessOne() takes a track, transforms it to bunch of buffer-blocks,
// and calls libsamplerate code on these blocks.
bool ChangeSpeedBase::ProcessOne(
    const WaveChannel& track, WaveChannel& outputTrack, sampleCount start,
    sampleCount end)
{
    // Get the length of the selection (as double). len is
    // used simple to calculate a progress meter, so it is easier
    // to make it a double now than it is to do it later
    auto len = (end - start).as_double();

    // Initiate processing buffers, most likely shorter than
    // the length of the selection being processed.
    auto inBufferSize = track.GetMaxBlockSize();

    Floats inBuffer { inBufferSize };

    // mFactor is at most 100-fold so this shouldn't overflow size_t
    auto outBufferSize = size_t(mFactor * inBufferSize + 10);
    Floats outBuffer { outBufferSize };

    // Set up the resampling stuff for this track.
    Resample resample(true, mFactor, mFactor); // constant rate resampling

    // Go through the track one buffer at a time. samplePos counts which
    // sample the current buffer starts at.
    bool bResult = true;
    auto samplePos = start;
    while (samplePos < end)
    {
        // Get a blockSize of samples (smaller than the size of the buffer)
        auto blockSize = limitSampleBufferSize(
            track.GetBestBlockSize(samplePos), end - samplePos);

        // Get the samples from the track and put them in the buffer
        track.GetFloats(inBuffer.get(), samplePos, blockSize);

        const auto results = resample.Process(
            mFactor, inBuffer.get(), blockSize, ((samplePos + blockSize) >= end),
            outBuffer.get(), outBufferSize);
        const auto outgen = results.second;

        if (outgen > 0) {
            outputTrack.Append((samplePtr)outBuffer.get(), floatSample, outgen);
        }

        // Increment samplePos
        samplePos += results.first;

        // Update the Progress meter
        if (TrackProgress(mCurTrackNum, (samplePos - start).as_double() / len)) {
            bResult = false;
            break;
        }
    }

    return bResult;
}
