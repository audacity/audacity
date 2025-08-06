/*
 * Audacity: A Digital Audio Editor
 */

/**********************************************************************

  Audacity: A Digital Audio Editor

  ClickRemovalEffect.cpp

  Craig DeForest

*******************************************************************//**

\class ClickRemovalEffect
\brief An Effect for removing clicks.

  Clicks are identified as small regions of high amplitude compared
  to the surrounding chunk of sound.  Anything sufficiently tall compared
  to a large (2048 sample) window around it, and sufficiently narrow,
  is considered to be a click.

  The structure was largely stolen from Domonic Mazzoni's NoiseRemoval
  module, and reworked for the NEW effect.

  This file is intended to become part of Audacity.  You may modify
  and/or distribute it under the same terms as Audacity itself.

*//*******************************************************************/
#include "clickremovaleffect.h"
#include "EffectOutputTracks.h"
#include "Prefs.h"
#include "ShuttleAutomation.h"
#include "WaveTrack.h"
#include <cmath>

namespace au::effects {
namespace {
constexpr size_t windowSize = 8192;
}

const EffectParameterMethods& ClickRemovalEffect::Parameters() const
{
    static CapturedParameters<ClickRemovalEffect, Threshold, Width> parameters;
    return parameters;
}

const ComponentInterfaceSymbol ClickRemovalEffect::Symbol { XO("Click Removal") };

ClickRemovalEffect::ClickRemovalEffect()
{
    Parameters().Reset(*this);

    SetLinearEffectFlag(false);

    sep = 2049;
}

ClickRemovalEffect::~ClickRemovalEffect()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol ClickRemovalEffect::GetSymbol() const
{
    return Symbol;
}

TranslatableString ClickRemovalEffect::GetDescription() const
{
    return XO("Click Removal is designed to remove clicks on audio tracks");
}

ManualPageID ClickRemovalEffect::ManualPage() const
{
    return L"Click_Removal";
}

// EffectDefinitionInterface implementation

EffectType ClickRemovalEffect::GetType() const
{
    return EffectTypeProcess;
}

// Effect implementation

bool ClickRemovalEffect::CheckWhetherSkipEffect(const EffectSettings&) const
{
    return (mClickWidth == 0) || (mThresholdLevel == 0);
}

bool ClickRemovalEffect::Process(::EffectInstance&, EffectSettings&)
{
    EffectOutputTracks outputs { *mTracks, GetType(), { { mT0, mT1 } } };
    bool bGoodResult = true;
    mbDidSomething = false;

    int count = 0;
    for (auto track : outputs.Get().Selected<WaveTrack>()) {
        double trackStart = track->GetStartTime();
        double trackEnd = track->GetEndTime();
        double t0 = std::max(mT0, trackStart);
        double t1 = std::min(trackEnd, mT1);

        if (t1 > t0) {
            auto start = track->TimeToLongSamples(t0);
            auto end = track->TimeToLongSamples(t1);
            auto len = end - start;
            for (const auto pChannel : track->Channels()) {
                if (!ProcessOne(count++, *pChannel, start, len)) {
                    bGoodResult = false;
                    goto done;
                }
            }
        }
    }
done:
    if (bGoodResult && !mbDidSomething) { // Processing successful, but
                                          // ineffective.
        mLastError = XO("Algorithm not effective on this audio. Nothing changed.").Translation().ToStdString();
        return false;
    }

    if (bGoodResult && mbDidSomething) {
        outputs.Commit();
    }

    return bGoodResult && mbDidSomething;
}

bool ClickRemovalEffect::ProcessOne(
    int count, WaveChannel& track, sampleCount start, sampleCount len)
{
    if (len <= windowSize / 2) {
        static_assert(windowSize / 2 == 4096);
        mLastError = XO("Selection must be larger than 4096 samples.").Translation().ToStdString();
        return false;
    }

    auto idealBlockLen = track.GetMaxBlockSize() * 4;
    if (idealBlockLen % windowSize != 0) {
        idealBlockLen += (windowSize - (idealBlockLen % windowSize));
    }

    bool bResult = true;
    decltype(len) s = 0;
    Floats buffer { idealBlockLen };
    Floats datawindow { windowSize };
    while ((len - s) > windowSize / 2)
    {
        auto block = limitSampleBufferSize(idealBlockLen, len - s);
        track.GetFloats(buffer.get(), start + s, block);
        for (decltype(block) i = 0; i + windowSize / 2 < block;
             i += windowSize / 2) {
            auto wcopy = std::min(windowSize, block - i);
            for (decltype(wcopy) j = 0; j < wcopy; ++j) {
                datawindow[j] = buffer[i + j];
            }
            for (auto j = wcopy; j < windowSize; ++j) {
                datawindow[j] = 0;
            }
            mbDidSomething |= RemoveClicks(windowSize, datawindow.get());
            for (decltype(wcopy) j = 0; j < wcopy; ++j) {
                buffer[i + j] = datawindow[j];
            }
        }

        if (mbDidSomething) {
            // RemoveClicks() actually did something.
            if (!track.SetFloats(buffer.get(), start + s, block)) {
                bResult = false;
                break;
            }
        }
        s += block;
        if (TrackProgress(count, s.as_double() / len.as_double())) {
            bResult = false;
            break;
        }
    }
    return bResult;
}

bool ClickRemovalEffect::RemoveClicks(size_t len, float* buffer)
{
    bool bResult = false; // This effect usually does nothing.
    size_t i;
    size_t j;
    int left = 0;

    float msw;
    int ww;
    int s2 = sep / 2;
    Floats ms_seq { len };
    Floats b2 { len };

    for (i = 0; i < len; i++) {
        b2[i] = buffer[i] * buffer[i];
    }

    /* Shortcut for rms - multiple passes through b2, accumulating
     * as we go.
     */
    for (i = 0; i < len; i++) {
        ms_seq[i] = b2[i];
    }

    for (i = 1; (int)i < sep; i *= 2) {
        for (j = 0; j < len - i; j++) {
            ms_seq[j] += ms_seq[j + i];
        }
    }

    /* Cheat by truncating sep to next-lower power of two... */
    sep = i;

    for (i = 0; i < len - sep; i++) {
        ms_seq[i] /= sep;
    }
    /* ww runs from about 4 to mClickWidth.  wrc is the reciprocal;
     * chosen so that integer roundoff doesn't clobber us.
     */
    int wrc;
    for (wrc = mClickWidth / 4; wrc >= 1; wrc /= 2) {
        ww = mClickWidth / wrc;

        for (i = 0; i < len - sep; i++) {
            msw = 0;
            for (j = 0; (int)j < ww; j++) {
                msw += b2[i + s2 + j];
            }
            msw /= ww;

            if (msw >= mThresholdLevel * ms_seq[i] / 10) {
                if (left == 0) {
                    left = i + s2;
                }
            } else {
                if (left != 0 && ((int)i - left + s2) <= ww * 2) {
                    float lv = buffer[left];
                    float rv = buffer[i + ww + s2];
                    for (j = left; j < i + ww + s2; j++) {
                        bResult = true;
                        buffer[j] = (rv * (j - left) + lv * (i + ww + s2 - j))
                                    / (float)(i + ww + s2 - left);
                        b2[j] = buffer[j] * buffer[j];
                    }
                    left = 0;
                } else if (left != 0) {
                    left = 0;
                }
            }
        }
    }
    return bResult;
}
}
