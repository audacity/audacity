/*
* Audacity: A Digital Audio Editor
*/
#include "normalizeviewmodel.h"

#include "libraries/lib-builtin-effects/NormalizeBase.h"

#include "log.h"

namespace au::effects {
NormalizeBase* NormalizeViewModel::effect() const
{
    const EffectId effectId = this->effectId();
    if (effectId.isEmpty()) {
        return nullptr;
    }
    Effect* const e = effectsProvider()->effect(effectId);
    return dynamic_cast<NormalizeBase*>(e);
}

bool NormalizeViewModel::removeDC() const
{
    const NormalizeBase* const fx = effect();
    if (!fx) {
        return false;
    }
    return fx->mDC;
}

void NormalizeViewModel::setRemoveDC(bool removeDC)
{
    NormalizeBase* const fx = effect();
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
    const NormalizeBase* const fx = effect();
    if (!fx) {
        return false;
    }
    return fx->mGain;
}

void NormalizeViewModel::setNormalizePeakAmplitude(bool normalizePeakAmplitude)
{
    NormalizeBase* const fx = effect();
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
    const NormalizeBase* const fx = effect();
    if (!fx) {
        return 0.0f;
    }
    return fx->mPeakLevel;
}

void NormalizeViewModel::setPeakAmplitudeTarget(double peakAmplitudeTarget)
{
    NormalizeBase* const fx = effect();
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
    const NormalizeBase* const fx = this->effect();
    if (!fx) {
        return false;
    }
    return fx->mStereoInd;
}

void NormalizeViewModel::setNormalizeStereoChannelsIndependently(bool normalizeStereoChannelsIndependently)
{
    NormalizeBase* const fx = effect();
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
