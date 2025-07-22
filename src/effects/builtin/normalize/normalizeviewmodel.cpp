/*
* Audacity: A Digital Audio Editor
*/
#include "normalizeviewmodel.h"

#include "normalizeeffect.h"

#include "log.h"

namespace au::effects {
NormalizeEffect* NormalizeViewModel::effect() const
{
    const EffectId effectId = this->effectId();
    if (effectId.isEmpty()) {
        return nullptr;
    }
    Effect* const e = effectsProvider()->effect(effectId);
    return dynamic_cast<NormalizeEffect*>(e);
}

bool NormalizeViewModel::removeDC() const
{
    const NormalizeEffect* const fx = effect();
    if (!fx) {
        return false;
    }
    return fx->mDC;
}

void NormalizeViewModel::setRemoveDC(bool removeDC)
{
    NormalizeEffect* const fx = effect();
    IF_ASSERT_FAILED(fx) {
        return;
    }

    if (fx->mDC == removeDC) {
        return;
    }

    fx->mDC = removeDC;
    emit removeDCChanged();
}

bool NormalizeViewModel::normalizePeakAmplitude() const
{
    const NormalizeEffect* const fx = effect();
    if (!fx) {
        return false;
    }
    return fx->mGain;
}

void NormalizeViewModel::setNormalizePeakAmplitude(bool normalizePeakAmplitude)
{
    NormalizeEffect* const fx = effect();
    IF_ASSERT_FAILED(fx) {
        return;
    }

    if (fx->mGain == normalizePeakAmplitude) {
        return;
    }

    fx->mGain = normalizePeakAmplitude;
    emit normalizePeakAmplitudeChanged();
}

double NormalizeViewModel::peakAmplitudeTarget() const
{
    const NormalizeEffect* const fx = effect();
    if (!fx) {
        return 0.0f;
    }
    return fx->mPeakLevel;
}

void NormalizeViewModel::setPeakAmplitudeTarget(double peakAmplitudeTarget)
{
    NormalizeEffect* const fx = effect();
    IF_ASSERT_FAILED(fx) {
        return;
    }

    const double newLevel = muse::check_valid(peakAmplitudeTarget);
    if (muse::is_equal(fx->mPeakLevel, newLevel)) {
        return;
    }

    fx->mPeakLevel = newLevel;
    emit peakAmplitudeTargetChanged();
}

bool NormalizeViewModel::normalizeStereoChannelsIndependently() const
{
    const NormalizeEffect* const fx = this->effect();
    if (!fx) {
        return false;
    }
    return fx->mStereoInd;
}

void NormalizeViewModel::setNormalizeStereoChannelsIndependently(bool normalizeStereoChannelsIndependently)
{
    NormalizeEffect* const fx = effect();
    IF_ASSERT_FAILED(fx) {
        return;
    }

    if (fx->mStereoInd == normalizeStereoChannelsIndependently) {
        return;
    }

    fx->mStereoInd = normalizeStereoChannelsIndependently;
    emit normalizeStereoChannelsIndependentlyChanged();
}

void NormalizeViewModel::doReload()
{
    emit removeDCChanged();
    emit normalizePeakAmplitudeChanged();
    emit normalizeStereoChannelsIndependentlyChanged();
    emit peakAmplitudeTargetChanged();
}
}
