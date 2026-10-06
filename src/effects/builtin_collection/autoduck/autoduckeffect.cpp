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

#include "au3-basic-ui/BasicUI.h"
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

bool AutoDuckEffect::Init()
{
    mControlTrack = nullptr;

    // Find the control track, which is the non-selected wave track immediately
    // after the last selected wave track.  Fail if there is no such track or if
    // any selected track is not a wave track.
    bool lastWasSelectedWaveTrack = false;
    const WaveTrack* controlTrackCandidate = nullptr;
    for (auto t : *inputTracks()) {
        if (lastWasSelectedWaveTrack && !t->GetSelected()) {
            // This could be the control track, so remember it
            controlTrackCandidate = dynamic_cast<const WaveTrack*>(t);
        }

        lastWasSelectedWaveTrack = false;
        if (t->GetSelected()) {
            bool ok = t->TypeSwitch<bool>(
                [&](const WaveTrack&) {
                lastWasSelectedWaveTrack = true;
                controlTrackCandidate = nullptr;
                return true;
            },
                [&](const Track&) {
                using namespace BasicUI;
                ShowMessageBox(
                    /*: Auto duck is the name of an effect that 'ducks'
                     (reduces the volume) of the audio automatically when there is
                     sound on another track.  Not as in 'Donald-Duck'!*/
                    TranslatableString("effects-autoduck", "You selected a track which does not contain audio. AutoDuck can only process audio tracks."),
                    MessageBoxOptions {}.IconStyle(Icon::Error));
                return false;
            });
            if (!ok) {
                return false;
            }
        }
    }

    if (!controlTrackCandidate) {
        using namespace BasicUI;
        ShowMessageBox(
            /*: Auto duck is the name of an effect that 'ducks' (reduces
             the volume) of the audio automatically when there is sound on another
             track.  Not as in 'Donald-Duck'!*/
            TranslatableString("effects-autoduck", "Auto Duck needs a control track which must be placed below the selected track(s)."),
            MessageBoxOptions {}.IconStyle(Icon::Error));
        return false;
    }

    mControlTrack = controlTrackCandidate;
    return true;
}

bool AutoDuckEffect::Process(::EffectInstance&, EffectSettings&)
{
    if (GetNumWaveTracks() == 0 || !mControlTrack) {
        return false;
    }

    bool cancel = false;

    const auto controlTrackStart = mControlTrack->TimeToLongSamples(mT0 + mOuterFadeDownLen);
    const auto controlTrackEnd = mControlTrack->TimeToLongSamples(mT1 - mOuterFadeUpLen);

    if (controlTrackEnd <= controlTrackStart) {
        return false;
    }

    WaveTrack::Holder pFirstTrack;
    auto pControlTrack = mControlTrack;
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
