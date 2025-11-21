/**********************************************************************

  Audacity: A Digital Audio Editor

  LegacyCompressorBase.cpp

  Dominic Mazzoni
  Martyn Shaw
  Steve Jolly

*******************************************************************//**

\class LegacyCompressorBase
\brief An Effect derived from EffectTwoPassSimpleMono

 - Martyn Shaw made it inherit from EffectTwoPassSimpleMono 10/2005.
 - Steve Jolly made it inherit from EffectSimpleMono.
 - GUI added and implementation improved by Dominic Mazzoni, 5/11/2003.

*//*******************************************************************/
#include "LegacyCompressorBase.h"
#include "Prefs.h"
#include "WaveTrack.h"
#include <cmath>

const EffectParameterMethods& LegacyCompressorBase::Parameters() const
{
    static CapturedParameters<
        LegacyCompressorBase, Threshold, NoiseFloor,
        Ratio,     // positive number > 1.0
        AttackTime, // seconds
        ReleaseTime, // seconds
        Normalize, UsePeak>
    parameters;
    return parameters;
}

//----------------------------------------------------------------------------
// LegacyCompressorBase
//----------------------------------------------------------------------------

const ComponentInterfaceSymbol LegacyCompressorBase::Symbol { XO(
                                                                  "Legacy Compressor") };

LegacyCompressorBase::LegacyCompressorBase()
{
    Parameters().Reset(*this);

    mThreshold = 0.25;
    mNoiseFloor = 0.01;
    mCompression = 0.5;
    mFollowLen = 0;

    SetLinearEffectFlag(false);
}

LegacyCompressorBase::~LegacyCompressorBase()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol LegacyCompressorBase::GetSymbol() const
{
    return Symbol;
}

TranslatableString LegacyCompressorBase::GetDescription() const
{
    return XO("Compresses the dynamic range of audio");
}

ManualPageID LegacyCompressorBase::ManualPage() const
{
    return L"Compressor";
}

// EffectDefinitionInterface implementation

EffectType LegacyCompressorBase::GetType() const
{
    return EffectTypeProcess;
}

// EffectTwoPassSimpleMono implementation

bool LegacyCompressorBase::NewTrackPass1()
{
    mThreshold = DB_TO_LINEAR(mThresholdDB);
    mNoiseFloor = DB_TO_LINEAR(mNoiseFloorDB);
    mNoiseCounter = 100;

    mAttackInverseFactor = exp(log(mThreshold) / (mCurRate * mAttackTime + 0.5));
    mAttackFactor = 1.0 / mAttackInverseFactor;
    mDecayFactor = exp(log(mThreshold) / (mCurRate * mDecayTime + 0.5));

    if (mRatio > 1) {
        mCompression = 1.0 - 1.0 / mRatio;
    } else {
        mCompression = 0.0;
    }

    mLastLevel = mThreshold;

    mCircleSize = 100;
    mCircle.reinit(mCircleSize, true);
    mCirclePos = 0;
    mRMSSum = 0.0;

    return true;
}

bool LegacyCompressorBase::InitPass1()
{
    mMax = 0.0;
    if (!mNormalize) {
        DisableSecondPass();
    }

    // Find the maximum block length required for any track
    size_t maxlen = inputTracks()->Selected<const WaveTrack>().max(
        &WaveTrack::GetMaxBlockSize);
    mFollow1.reset();
    mFollow2.reset();
    // Allocate buffers for the envelope
    if (maxlen > 0) {
        mFollow1.reinit(maxlen);
        mFollow2.reinit(maxlen);
    }
    mFollowLen = maxlen;

    return true;
}

bool LegacyCompressorBase::InitPass2()
{
    // Actually, this should not even be called, because we call
    // DisableSecondPass() before, if mNormalize is false.
    return mNormalize;
}

// Process the input with 2 buffers available at a time
// buffer1 will be written upon return
// buffer2 will be passed as buffer1 on the next call
bool LegacyCompressorBase::TwoBufferProcessPass1(
    float* buffer1, size_t len1, float* buffer2, size_t len2)
{
    // If buffers are bigger than allocated, then abort
    // (this should never happen, but if it does, we don't want to crash)
    if ((len1 > mFollowLen) || (len2 > mFollowLen)) {
        return false;
    }

    // This makes sure that the initial value is well-chosen
    // buffer1 == NULL on the first and only the first call
    if (buffer1 == NULL) {
        // Initialize the mLastLevel to the peak level in the first buffer
        // This avoids problems with large spike events near the beginning of the
        // track
        mLastLevel = mThreshold;
        for (size_t i = 0; i < len2; i++) {
            if (mLastLevel < fabs(buffer2[i])) {
                mLastLevel = fabs(buffer2[i]);
            }
        }
    }

    // buffer2 is NULL on the last and only the last call
    if (buffer2 != NULL) {
        Follow(buffer2, mFollow2.get(), len2, mFollow1.get(), len1);
    }

    if (buffer1 != NULL) {
        for (size_t i = 0; i < len1; i++) {
            buffer1[i] = DoCompression(buffer1[i], mFollow1[i]);
        }
    }

#if 0
    // Copy the envelope over the track data (for debug purposes)
    memcpy(buffer1, mFollow1, len1 * sizeof(float));
#endif

    // Rotate the buffer pointers
    mFollow1.swap(mFollow2);

    return true;
}

bool LegacyCompressorBase::ProcessPass2(float* buffer, size_t len)
{
    if (mMax != 0) {
        for (size_t i = 0; i < len; i++) {
            buffer[i] /= mMax;
        }
    }

    return true;
}

void LegacyCompressorBase::FreshenCircle()
{
    // Recompute the RMS sum periodically to prevent accumulation of rounding
    // errors during long waveforms
    mRMSSum = 0;
    for (size_t i = 0; i < mCircleSize; i++) {
        mRMSSum += mCircle[i];
    }
}

float LegacyCompressorBase::AvgCircle(float value)
{
    float level;

    // Calculate current level from root-mean-squared of
    // circular buffer ("RMS")
    mRMSSum -= mCircle[mCirclePos];
    mCircle[mCirclePos] = value * value;
    mRMSSum += mCircle[mCirclePos];
    level = sqrt(mRMSSum / mCircleSize);
    mCirclePos = (mCirclePos + 1) % mCircleSize;

    return level;
}

void LegacyCompressorBase::Follow(
    float* buffer, float* env, size_t len, float* previous, size_t previous_len)
{
    /*

    "Follow"ing algorithm by Roger B. Dannenberg, taken from
    Nyquist.  His description follows.  -DMM

    Description: this is a sophisticated envelope follower.
     The input is an envelope, e.g. something produced with
     the AVG function. The purpose of this function is to
     generate a smooth envelope that is generally not less
     than the input signal. In other words, we want to "ride"
     the peaks of the signal with a smooth function. The
     algorithm is as follows: keep a current output value
     (called the "value"). The value is allowed to increase
     by at most rise_factor and decrease by at most fall_factor.
     Therefore, the next value should be between
     value * rise_factor and value * fall_factor. If the input
     is in this range, then the next value is simply the input.
     If the input is less than value * fall_factor, then the
     next value is just value * fall_factor, which will be greater
     than the input signal. If the input is greater than value *
     rise_factor, then we compute a rising envelope that meets
     the input value by working bacwards in time, changing the
     previous values to input / rise_factor, input / rise_factor^2,
     input / rise_factor^3, etc. until this NEW envelope intersects
     the previously computed values. There is only a limited buffer
     in which we can work backwards, so if the NEW envelope does not
     intersect the old one, then make yet another pass, this time
     from the oldest buffered value forward, increasing on each
     sample by rise_factor to produce a maximal envelope. This will
     still be less than the input.

     The value has a lower limit of floor to make sure value has a
     reasonable positive value from which to begin an attack.
    */
    double level, last;

    if (!mUsePeak) {
        // Update RMS sum directly from the circle buffer
        // to avoid accumulation of rounding errors
        FreshenCircle();
    }
    // First apply a peak detect with the requested decay rate
    last = mLastLevel;
    for (size_t i = 0; i < len; i++) {
        if (mUsePeak) {
            level = fabs(buffer[i]);
        } else { // use RMS
            level = AvgCircle(buffer[i]);
        }
        // Don't increase gain when signal is continuously below the noise floor
        if (level < mNoiseFloor) {
            mNoiseCounter++;
        } else {
            mNoiseCounter = 0;
        }
        if (mNoiseCounter < 100) {
            last *= mDecayFactor;
            if (last < mThreshold) {
                last = mThreshold;
            }
            if (level > last) {
                last = level;
            }
        }
        env[i] = last;
    }
    mLastLevel = last;

    // Next do the same process in reverse direction to get the requested attack
    // rate
    last = mLastLevel;
    for (size_t i = len; i--;) {
        last *= mAttackInverseFactor;
        if (last < mThreshold) {
            last = mThreshold;
        }
        if (env[i] < last) {
            env[i] = last;
        } else {
            last = env[i];
        }
    }

    if ((previous != NULL) && (previous_len > 0)) {
        // If the previous envelope was passed, propagate the rise back until we
        // intersect
        for (size_t i = previous_len; i--;) {
            last *= mAttackInverseFactor;
            if (last < mThreshold) {
                last = mThreshold;
            }
            if (previous[i] < last) {
                previous[i] = last;
            } else { // Intersected the previous envelope buffer, so we are finished
                return;
            }
        }
        // If we can't back up far enough, project the starting level forward
        // until we intersect the desired envelope
        last = previous[0];
        for (size_t i = 1; i < previous_len; i++) {
            last *= mAttackFactor;
            if (previous[i] > last) {
                previous[i] = last;
            } else { // Intersected the desired envelope, so we are finished
                return;
            }
        }
        // If we still didn't intersect, then continue ramp up into current buffer
        for (size_t i = 0; i < len; i++) {
            last *= mAttackFactor;
            if (buffer[i] > last) {
                buffer[i] = last;
            } else { // Finally got an intersect
                return;
            }
        }
        // If we still didn't intersect, then reset mLastLevel
        mLastLevel = last;
    }
}

float LegacyCompressorBase::DoCompression(float value, double env)
{
    float out;
    if (mUsePeak) {
        // Peak values map 1.0 to 1.0 - 'upward' compression
        out = value * pow(1.0 / env, mCompression);
    } else {
        // With RMS-based compression don't change values below mThreshold -
        // 'downward' compression
        out = value * pow(mThreshold / env, mCompression);
    }

    // Retain the maximum value for use in the normalization pass
    if (mMax < fabs(out)) {
        mMax = fabs(out);
    }

    return out;
}
