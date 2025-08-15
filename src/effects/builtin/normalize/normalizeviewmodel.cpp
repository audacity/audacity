/*
* Audacity: A Digital Audio Editor
*/
#include "normalizeviewmodel.h"

#include "normalizeeffect.h"

#include "log.h"

namespace au::effects {
bool NormalizeViewModel::removeDC() const
{
    const auto& fx = effect<NormalizeEffect>();
    return fx.mDC;
}

void NormalizeViewModel::setRemoveDC(bool removeDC)
{
    auto& fx = effect<NormalizeEffect>();

    if (fx.mDC == removeDC) {
        return;
    }

    fx.mDC = removeDC;
    emit removeDCChanged();
}

bool NormalizeViewModel::normalizePeakAmplitude() const
{
    const auto& fx = effect<NormalizeEffect>();
    return fx.mGain;
}

void NormalizeViewModel::setNormalizePeakAmplitude(bool normalizePeakAmplitude)
{
    auto& fx = effect<NormalizeEffect>();

    if (fx.mGain == normalizePeakAmplitude) {
        return;
    }

    fx.mGain = normalizePeakAmplitude;
    emit normalizePeakAmplitudeChanged();
}

double NormalizeViewModel::peakAmplitudeTarget() const
{
    const auto& fx = effect<NormalizeEffect>();
    return fx.mPeakLevel;
}

void NormalizeViewModel::setPeakAmplitudeTarget(double peakAmplitudeTarget)
{
    auto& fx = effect<NormalizeEffect>();

    const double newLevel = muse::check_valid(peakAmplitudeTarget);
    if (muse::is_equal(fx.mPeakLevel, newLevel)) {
        return;
    }

    fx.mPeakLevel = newLevel;
    emit peakAmplitudeTargetChanged();
}

bool NormalizeViewModel::normalizeStereoChannelsIndependently() const
{
    const auto& fx = effect<NormalizeEffect>();
    return fx.mStereoInd;
}

void NormalizeViewModel::setNormalizeStereoChannelsIndependently(bool normalizeStereoChannelsIndependently)
{
    auto& fx = effect<NormalizeEffect>();

    if (fx.mStereoInd == normalizeStereoChannelsIndependently) {
        return;
    }

    fx.mStereoInd = normalizeStereoChannelsIndependently;
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
