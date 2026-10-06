/**********************************************************************

  Audacity: A Digital Audio Editor

  autoduckeffect.cpp

  Markus Meyer

*******************************************************************//**

\class AutoDuckEffect
\brief Implements the Auto Ducking effect

\class AutoDuckRegion
\brief a struct that holds a start and end time.

*******************************************************************/
#include "autoduckeffect.h"

#include "au3-effects/EffectOutputTracks.h"
#include "au3-command-parameters/ShuttleAutomation.h"
#include "au3-wave-track/TimeStretching.h"
#include "au3-exceptions/UserException.h"
#include "au3-wave-track/WaveClip.h"
#include "au3-wave-track/WaveTrack.h"

#include <cmath>

using namespace au::effects;

const ComponentInterfaceSymbol AutoDuckEffect::Symbol { "Auto Duck", TranslatableString("effects-autoduck", "Auto duck") };

const EffectParameterMethods& AutoDuckEffect::Parameters() const
{
    static CapturedParameters<
        AutoDuckEffect, DuckAmountDb, InnerFadeDownLen, InnerFadeUpLen,
        OuterFadeDownLen, OuterFadeUpLen, ThresholdDb, MaximumPause>
    parameters;
    return parameters;
}

/*
 * Common constants
 */

static const size_t kBufSize = 131072u; // number of samples to process at once
static const size_t kRMSWindowSize
    =100u; // samples in circular RMS window buffer

/*
 * A auto duck region and an array of auto duck regions
 */

struct AutoDuckRegion
{
    AutoDuckRegion(double t0, double t1)
    {
        this->t0 = t0;
        this->t1 = t1;
    }

    double t0;
    double t1;
};

AutoDuckEffect::AutoDuckEffect()
{
    Parameters().Reset(*this);
    SetLinearEffectFlag(true);
}

AutoDuckEffect::~AutoDuckEffect()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol AutoDuckEffect::GetSymbol() const
{
    return Symbol;
}

TranslatableString AutoDuckEffect::GetDescription() const
{
    return TranslatableString("effects-autoduck",
                              "Reduces (ducks) the volume of one or more tracks whenever the volume of a specified “control” track reaches a particular level");
}

ManualPageID AutoDuckEffect::ManualPage() const
{
    return L"Auto_Duck";
}

// EffectDefinitionInterface implementation

::EffectType AutoDuckEffect::GetType() const
{
    return EffectTypeProcess;
}

// Effect implementation

namespace {
bool isIn(::TrackId id, const std::vector<AutoDuckEffect::ControlTrackCandidate>& candidates)
{
    return std::ranges::any_of(candidates, [id](const AutoDuckEffect::ControlTrackCandidate& c) { return c.id == id; });
}
}

bool AutoDuckEffect::Init()
{
    // Any wave track that is not processed may serve as control track. The
    // default is AU3's choice, i.e., the non-selected wave track immediately
    // after the last selected wave track.
    mControlTrackCandidates.clear();
    std::optional<::TrackId> waveTrackJustBelowSelection;
    bool lastWasSelectedWaveTrack = false;
    for (const Track* t : *inputTracks()) {
        const auto waveTrack = dynamic_cast<const WaveTrack*>(t);
        if (t->GetSelected()) {
            lastWasSelectedWaveTrack = waveTrack != nullptr;
            if (waveTrack) {
                waveTrackJustBelowSelection.reset();
            }
            continue;
        }
        if (waveTrack) {
            mControlTrackCandidates.push_back({ waveTrack->GetId(), waveTrack->GetName().ToStdString() });
            if (lastWasSelectedWaveTrack) {
                waveTrackJustBelowSelection = waveTrack->GetId();
            }
        }
        lastWasSelectedWaveTrack = false;
    }

    if (!mControlTrackId || !isIn(*mControlTrackId, mControlTrackCandidates)) {
        // User hasn't made a choice or it doesn't apply anymore.
        if (waveTrackJustBelowSelection) {
            mControlTrackId = waveTrackJustBelowSelection;
        } else if (!mControlTrackCandidates.empty()) {
            mControlTrackId = mControlTrackCandidates.front().id;
        } else {
            mControlTrackId.reset();
        }
    }

    // Do not fail if there is no control track: the dialog tells the user.
    return true;
}

const std::vector<AutoDuckEffect::ControlTrackCandidate>& AutoDuckEffect::ControlTrackCandidates() const
{
    return mControlTrackCandidates;
}

std::optional<::TrackId> AutoDuckEffect::ControlTrackId() const
{
    return mControlTrackId;
}

void AutoDuckEffect::SetControlTrackId(::TrackId id)
{
    mControlTrackId = id;
}

const WaveTrack* AutoDuckEffect::FindControlTrack() const
{
    if (!mControlTrackId || !inputTracks()) {
        return nullptr;
    }
    // During preview, inputTracks() only contains the preview tracks, but they
    // share the owning project.
    const auto project = inputTracks()->GetOwner();
    if (!project) {
        return nullptr;
    }
    return dynamic_cast<const WaveTrack*>(TrackList::Get(*project).FindById(*mControlTrackId));
}

bool AutoDuckEffect::Process(::EffectInstance&, EffectSettings&)
{
    const WaveTrack* controlTrack = FindControlTrack();
    if (GetNumWaveTracks() == 0 || !controlTrack) {
        return false;
    }

    bool cancel = false;

    const auto controlTrackStart = controlTrack->TimeToLongSamples(mT0 + mOuterFadeDownLen);
    const auto controlTrackEnd = controlTrack->TimeToLongSamples(mT1 - mOuterFadeUpLen);

    if (controlTrackEnd <= controlTrackStart) {
        return false;
    }

    WaveTrack::Holder pFirstTrack;
    auto pControlTrack = controlTrack;
    // If there is any stretch in the control track, substitute a temporary
    // rendering before trying to use GetFloats
    {
        const auto t0 = pControlTrack->LongSamplesToTime(controlTrackStart);
        const auto t1 = pControlTrack->LongSamplesToTime(controlTrackEnd);
        if (TimeStretching::HasPitchOrSpeed(*pControlTrack, t0, t1)) {
            pFirstTrack = pControlTrack->Duplicate()->SharedPointer<WaveTrack>();
            if (pFirstTrack) {
                UserException::WithCancellableProgress(
                    [&](const ProgressReporter& reportProgress) {
                    pFirstTrack->ApplyPitchAndSpeed(
                        { { t0, t1 } }, reportProgress);
                },
                    TimeStretching::defaultStretchRenderingTitle,
                    TranslatableString("effects-autoduck", "Rendering Control-Track Time-Stretched Audio"));
                pControlTrack = pFirstTrack.get();
            }
        }
    }

    // the minimum number of samples we have to wait until the maximum
    // pause has been exceeded
    double maxPause = mMaximumPause;

    // We don't fade in until we have time enough to actually fade out again
    if (maxPause < mOuterFadeDownLen + mOuterFadeUpLen) {
        maxPause = mOuterFadeDownLen + mOuterFadeUpLen;
    }

    auto minSamplesPause = pControlTrack->TimeToLongSamples(maxPause);

    double threshold = DB_TO_LINEAR(mThresholdDb);

    // adjust the threshold so we can compare it to the rmsSum value
    threshold = threshold * threshold * kRMSWindowSize;

    int rmsPos = 0;
    double rmsSum = 0;
    // to make the progress bar appear more natural, we first look for all
    // duck regions and apply them all at once afterwards
    std::vector<AutoDuckRegion> processedTracksRegions;
    bool inDuckRegion = false;
    {
        Floats rmsWindow { kRMSWindowSize, true };

        Floats buf { kBufSize };

        // initialize the following two variables to prevent compiler warning
        double duckRegionStart = 0;
        sampleCount curSamplesPause = 0;

        auto pos = controlTrackStart;

        const auto pControlChannel = *pControlTrack->Channels().begin();
        while (pos < controlTrackEnd)
        {
            const auto len = limitSampleBufferSize(kBufSize, controlTrackEnd - pos);

            pControlChannel->GetFloats(buf.get(), pos, len);

            for (auto i = pos; i < pos + len; i++) {
                rmsSum -= rmsWindow[rmsPos];
                // i - pos is bounded by len:
                auto index = (i - pos).as_size_t();
                rmsWindow[rmsPos] = buf[index] * buf[index];
                rmsSum += rmsWindow[rmsPos];
                rmsPos = (rmsPos + 1) % kRMSWindowSize;

                bool thresholdExceeded = rmsSum > threshold;

                if (thresholdExceeded) {
                    // everytime the threshold is exceeded, reset our count for
                    // the number of pause samples
                    curSamplesPause = 0;

                    if (!inDuckRegion) {
                        // the threshold has been exceeded for the first time, so
                        // let the duck region begin here
                        inDuckRegion = true;
                        duckRegionStart = pControlTrack->LongSamplesToTime(i);
                    }
                }

                if (!thresholdExceeded && inDuckRegion) {
                    // the threshold has not been exceeded and we are in a duck
                    // region, but only fade in if the maximum pause has been
                    // exceeded
                    curSamplesPause += 1;

                    if (curSamplesPause >= minSamplesPause) {
                        // do the actual duck fade and reset all values
                        double duckRegionEnd = pControlTrack->LongSamplesToTime(i - curSamplesPause);
                        processedTracksRegions.push_back(AutoDuckRegion(
                                                             duckRegionStart - mOuterFadeDownLen,
                                                             duckRegionEnd + mOuterFadeUpLen));
                        inDuckRegion = false;
                    }
                }
            }

            pos += len;

            if (TotalProgress(
                    (pos - controlTrackStart).as_double() / (controlTrackEnd - controlTrackStart).as_double()
                    / (GetNumWaveTracks() + 1))) {
                cancel = true;
                break;
            }
        }

        // apply last duck fade, if any
        if (inDuckRegion) {
            double duckRegionEnd = pControlTrack->LongSamplesToTime(controlTrackEnd - curSamplesPause);
            processedTracksRegions.push_back(AutoDuckRegion(
                                                 duckRegionStart - mOuterFadeDownLen,
                                                 duckRegionEnd + mOuterFadeUpLen));
        }
    }

    if (!cancel) {
        EffectOutputTracks outputs { *mTracks, GetType(), { { mT0, mT1 } } };

        int trackNum = 0;

        for (auto iterTrack : outputs.Get().Selected<WaveTrack>()) {
            for (const auto pChannel : iterTrack->Channels()) {
                for (size_t i = 0; i < processedTracksRegions.size(); ++i) {
                    const AutoDuckRegion& region = processedTracksRegions[i];
                    if (ApplyDuckFade(trackNum++, *pChannel, region.t0, region.t1)) {
                        cancel = true;
                        goto done;
                    }
                }
            }

done:
            if (cancel) {
                break;
            }
        }

        if (!cancel) {
            outputs.Commit();
        }
    }

    return !cancel;
}

// AutoDuckEffect implementation

// this currently does an exponential fade
bool AutoDuckEffect::ApplyDuckFade(
    int trackNum, WaveChannel& track, double t0, double t1)
{
    bool cancel = false;

    auto start = track.TimeToLongSamples(t0);
    auto end = track.TimeToLongSamples(t1);

    Floats buf { kBufSize };
    auto pos = start;

    auto fadeDownSamples
        =track.TimeToLongSamples(mOuterFadeDownLen + mInnerFadeDownLen);
    if (fadeDownSamples < 1) {
        fadeDownSamples = 1;
    }

    auto fadeUpSamples
        =track.TimeToLongSamples(mOuterFadeUpLen + mInnerFadeUpLen);
    if (fadeUpSamples < 1) {
        fadeUpSamples = 1;
    }

    float fadeDownStep = mDuckAmountDb / fadeDownSamples.as_double();
    float fadeUpStep = mDuckAmountDb / fadeUpSamples.as_double();

    while (pos < end)
    {
        const auto len = limitSampleBufferSize(kBufSize, end - pos);
        track.GetFloats(buf.get(), pos, len);
        for (auto i = pos; i < pos + len; ++i) {
            float gainDown = fadeDownStep * (i - start).as_float();
            float gainUp = fadeUpStep * (end - i).as_float();

            float gain;
            if (gainDown > gainUp) {
                gain = gainDown;
            } else {
                gain = gainUp;
            }
            if (gain < mDuckAmountDb) {
                gain = mDuckAmountDb;
            }

            // i - pos is bounded by len:
            buf[(i - pos).as_size_t()] *= DB_TO_LINEAR(gain);
        }

        if (!track.SetFloats(buf.get(), pos, len)) {
            cancel = true;
            break;
        }

        pos += len;

        float curTime = track.LongSamplesToTime(pos);
        float fractionFinished = (curTime - mT0) / (mT1 - mT0);
        if (TotalProgress(
                (trackNum + 1 + fractionFinished) / (GetNumWaveTracks() + 1))) {
            cancel = true;
            break;
        }
    }

    return cancel;
}
