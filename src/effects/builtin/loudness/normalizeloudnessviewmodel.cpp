/*
 * Audacity: A Digital Audio Editor
 */
#include "normalizeloudnessviewmodel.h"
#include "normalizeloudnesseffect.h"

#include "global/log.h"
#include "global/translation.h"

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

QString NormalizeLoudnessViewModel::title() const
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

QString NormalizeLoudnessViewModel::independentStereoLabel() const
{
    return muse::qtrc("effects/loudness", "Normalize stereo channels independently");
}

QString NormalizeLoudnessViewModel::dualMonoLabel() const
{
    return muse::qtrc("effects/loudness", "Treat mono as dual mono (recommended)");
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
