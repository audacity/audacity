/*
 * Audacity: A Digital Audio Editor
 */
#include "normalizeloudnessviewmodel.h"
#include "normalizeloudnesseffect.h"

#include "global/log.h"
#include "global/translation.h"

namespace au::effects {
void NormalizeLoudnessViewModel::doReload()
{
    emit useRmsAlgorithmChanged();
    emit perceivedLoudnessTargetChanged();
    emit rmsTargetChanged();
    emit normalizeStereoChannelsIndependentlyChanged();
    emit useDualMonoChanged();
}

QString NormalizeLoudnessViewModel::effectTitle() const
{
    return muse::qtrc("effects/loudness", "Normalize loudness");
}

QStringList NormalizeLoudnessViewModel::algorithmOptions() const
{
    return {
        muse::qtrc("effects/loudness", "RMS"),
        muse::qtrc("effects/loudness", "Perceived loudness")
    };
}

bool NormalizeLoudnessViewModel::useRmsAlgorithm() const
{
    const auto& e = effect<NormalizeLoudnessEffect>();
    return e.mNormalizeTo == NormalizeLoudnessEffect::kRMS;
}

void NormalizeLoudnessViewModel::setUseRmsAlgorithm(bool useRmsAlgorithm)
{
    auto& e = effect<NormalizeLoudnessEffect>();
    modifySettings([&e, useRmsAlgorithm](EffectSettings&) {
        e.mNormalizeTo = useRmsAlgorithm ? NormalizeLoudnessEffect::kRMS : NormalizeLoudnessEffect::kLoudness;
    });
    emit useRmsAlgorithmChanged();
}

QString NormalizeLoudnessViewModel::toLabel() const
{
    return muse::qtrc("effects/loudness", "to");
}

QString NormalizeLoudnessViewModel::currentMeasureUnitsSymbol() const
{
    return useRmsAlgorithm() ? "dB" : "LUFS";
}

double NormalizeLoudnessViewModel::perceivedLoudnessTarget() const
{
    const auto& e = effect<NormalizeLoudnessEffect>();
    return e.mLUFSLevel;
}

void NormalizeLoudnessViewModel::setPerceivedLoudnessTarget(double perceivedLoudnessTarget)
{
    auto& e = effect<NormalizeLoudnessEffect>();
    modifySettings([&e, perceivedLoudnessTarget](EffectSettings&) {
        e.mLUFSLevel = perceivedLoudnessTarget;
    });
    emit perceivedLoudnessTargetChanged();
}

double NormalizeLoudnessViewModel::rmsTarget() const
{
    const auto& e = effect<NormalizeLoudnessEffect>();
    return e.mRMSLevel;
}

void NormalizeLoudnessViewModel::setRmsTarget(double rmsTarget)
{
    auto& e = effect<NormalizeLoudnessEffect>();
    modifySettings([&e, rmsTarget](EffectSettings&) {
        e.mRMSLevel = rmsTarget;
    });
    emit rmsTargetChanged();
}

double NormalizeLoudnessViewModel::targetMin() const
{
    // Doesn't matter which target we use, they have the same min
    static_assert(NormalizeLoudnessEffect::LUFSLevel.min == NormalizeLoudnessEffect::RMSLevel.min);
    return NormalizeLoudnessEffect::LUFSLevel.min;
}

double NormalizeLoudnessViewModel::targetMax() const
{
    // Doesn't matter which target we use, they have the same max
    static_assert(NormalizeLoudnessEffect::LUFSLevel.max == NormalizeLoudnessEffect::RMSLevel.max);
    return NormalizeLoudnessEffect::LUFSLevel.max;
}

int NormalizeLoudnessViewModel::targetStep() const
{
    return 1;
}

int NormalizeLoudnessViewModel::targetDecimals() const
{
    return 1;
}

QString NormalizeLoudnessViewModel::normalizeLabel() const
{
    return muse::qtrc("effects/loudness", "Normalize");
}

bool NormalizeLoudnessViewModel::normalizeStereoChannelsIndependently() const
{
    const auto& e = effect<NormalizeLoudnessEffect>();
    return e.mStereoInd;
}

void NormalizeLoudnessViewModel::setNormalizeStereoChannelsIndependently(bool normalizeStereoChannelsIndependently)
{
    auto& e = effect<NormalizeLoudnessEffect>();
    modifySettings([&e, normalizeStereoChannelsIndependently](EffectSettings&) {
        e.mStereoInd = normalizeStereoChannelsIndependently;
    });
    emit normalizeStereoChannelsIndependentlyChanged();
}

QString NormalizeLoudnessViewModel::independentStereoLabel() const
{
    return muse::qtrc("effects/loudness", "Normalize stereo channels independently");
}

QString NormalizeLoudnessViewModel::useDualMonoLabel() const
{
    return muse::qtrc("effects/loudness", "Treat mono as dual mono (recommended)");
}

bool NormalizeLoudnessViewModel::useDualMono() const
{
    const auto& e = effect<NormalizeLoudnessEffect>();
    return e.mDualMono;
}

void NormalizeLoudnessViewModel::setUseDualMono(bool newUseDualMono)
{
    auto& e = effect<NormalizeLoudnessEffect>();
    modifySettings([&e, newUseDualMono](EffectSettings&) {
        e.mDualMono = newUseDualMono;
    });
    emit useDualMonoChanged();
}
}
