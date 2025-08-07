/*
 * Audacity: A Digital Audio Editor
 */
#include "normalizeloudnessviewmodel.h"

#include "normalizeloudnesseffect.h"

#include "log.h"

namespace au::effects {
NormalizeLoudnessEffect* NormalizeLoudnessViewModel::effect() const
{
    const EffectId effectId = this->effectId();
    if (effectId.isEmpty()) {
        return nullptr;
    }
    Effect* const e = effectsProvider()->effect(effectId);
    return dynamic_cast<NormalizeLoudnessEffect*>(e);
}

void NormalizeLoudnessViewModel::doReload()
{
    emit useRmsAlgorithmChanged();
    emit perceivedLoudnessTargetChanged();
    emit rmsTargetChanged();
    emit normalizeStereoChannelsIndependentlyChanged();
    emit dualMonoChanged();
}

bool NormalizeLoudnessViewModel::useRmsAlgorithm() const
{
    const NormalizeLoudnessEffect* e = effect();
    if (!e) {
        return false;
    }
    return e->mNormalizeTo == NormalizeLoudnessEffect::kRMS;
}

void NormalizeLoudnessViewModel::setUseRmsAlgorithm(bool useRmsAlgorithm)
{
    NormalizeLoudnessEffect* const e = effect();
    if (!e) {
        return;
    }
    modifySettings([e, useRmsAlgorithm](EffectSettings&) {
        e->mNormalizeTo = useRmsAlgorithm ? NormalizeLoudnessEffect::kRMS : NormalizeLoudnessEffect::kLoudness;
    });
    emit useRmsAlgorithmChanged();
}

double NormalizeLoudnessViewModel::perceivedLoudnessTarget() const
{
    const NormalizeLoudnessEffect* e = effect();
    if (!e) {
        return 0.0;
    }
    return e->mLUFSLevel;
}

void NormalizeLoudnessViewModel::setPerceivedLoudnessTarget(double perceivedLoudnessTarget)
{
    NormalizeLoudnessEffect* const e = effect();
    if (!e) {
        return;
    }
    modifySettings([e, perceivedLoudnessTarget](EffectSettings&) {
        e->mLUFSLevel = perceivedLoudnessTarget;
    });
    emit perceivedLoudnessTargetChanged();
}

double NormalizeLoudnessViewModel::rmsTarget() const
{
    const NormalizeLoudnessEffect* e = effect();
    if (!e) {
        return 0.0;
    }
    return e->mRMSLevel;
}

void NormalizeLoudnessViewModel::setRmsTarget(double rmsTarget)
{
    NormalizeLoudnessEffect* const e = effect();
    if (!e) {
        return;
    }
    modifySettings([e, rmsTarget](EffectSettings&) {
        e->mRMSLevel = rmsTarget;
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

bool NormalizeLoudnessViewModel::normalizeStereoChannelsIndependently() const
{
    const NormalizeLoudnessEffect* e = effect();
    if (!e) {
        return false;
    }
    return e->mStereoInd;
}

void NormalizeLoudnessViewModel::setNormalizeStereoChannelsIndependently(bool normalizeStereoChannelsIndependently)
{
    NormalizeLoudnessEffect* const e = effect();
    if (!e) {
        return;
    }
    modifySettings([e, normalizeStereoChannelsIndependently](EffectSettings&) {
        e->mStereoInd = normalizeStereoChannelsIndependently;
    });
    emit normalizeStereoChannelsIndependentlyChanged();
}

bool NormalizeLoudnessViewModel::dualMono() const
{
    const NormalizeLoudnessEffect* e = effect();
    if (!e) {
        return false;
    }
    return e->mDualMono;
}

void NormalizeLoudnessViewModel::setDualMono(bool dualMono)
{
    NormalizeLoudnessEffect* const e = effect();
    if (!e) {
        return;
    }
    modifySettings([e, dualMono](EffectSettings&) {
        e->mDualMono = dualMono;
    });
    emit dualMonoChanged();
}
}
