/*
 * Audacity: A Digital Audio Editor
 */
#include "noisereductionviewmodel.h"

#include "libraries/lib-builtin-effects/NoiseReductionBase.h"

#include "log.h"

using namespace au::effects;

NoiseReductionBase* NoiseReductionViewModel::effect() const
{
    EffectId effectId = this->effectId();
    if (effectId.isEmpty()) {
        return nullptr;
    }
    Effect* e = effectsProvider()->effect(effectId);
    return dynamic_cast<NoiseReductionBase*>(e);
}

void NoiseReductionViewModel::doReload()
{
    emit isApplyAllowedChanged();
    emit reductionChanged();
    emit sensitivityChanged();
    emit frequencySmoothingBandsChanged();
    emit reductionModeChanged();
}

int NoiseReductionViewModel::reduction() const
{
    const NoiseReductionBase* const fx = effect();
    if (!fx) {
        return 0;
    }
    return static_cast<int>(fx->mSettings->mNoiseGain);
}

void NoiseReductionViewModel::setReduction(int newReduction)
{
    if (reduction() == newReduction) {
        return;
    }
    NoiseReductionBase* const fx = effect();
    IF_ASSERT_FAILED(fx) {
        return;
    }
    fx->mSettings->mNoiseGain = newReduction;
    emit reductionChanged();
}

float NoiseReductionViewModel::sensitivity() const
{
    const NoiseReductionBase* const fx = effect();
    if (!fx) {
        return 0.0f;
    }
    return fx->mSettings->mNewSensitivity;
}

void NoiseReductionViewModel::setSensitivity(float newSensitivity)
{
    if (muse::is_equal(sensitivity(), newSensitivity)) {
        return;
    }
    NoiseReductionBase* const fx = effect();
    IF_ASSERT_FAILED(fx) {
        return;
    }
    fx->mSettings->mNewSensitivity = newSensitivity;
    emit sensitivityChanged();
}

int NoiseReductionViewModel::frequencySmoothingBands() const
{
    const NoiseReductionBase* const fx = effect();
    if (!fx) {
        return 0;
    }
    return static_cast<int>(fx->mSettings->mFreqSmoothingBands);
}

void NoiseReductionViewModel::setFrequencySmoothingBands(int newFrequencySmoothingBands)
{
    if (frequencySmoothingBands() == newFrequencySmoothingBands) {
        return;
    }
    NoiseReductionBase* const fx = effect();
    IF_ASSERT_FAILED(fx) {
        return;
    }
    fx->mSettings->mFreqSmoothingBands = static_cast<double>(newFrequencySmoothingBands);
    emit frequencySmoothingBandsChanged();
}

NoiseReductionMode NoiseReductionViewModel::reductionMode() const
{
    const NoiseReductionBase* const fx = effect();
    if (!fx) {
        return NoiseReductionMode::ReduceNoise;
    }
    return fx->mSettings->mNoiseReductionChoice == ::NoiseReductionChoice::NRC_REDUCE_NOISE
           ? NoiseReductionMode::ReduceNoise : NoiseReductionMode::LeaveResidue;
}

void NoiseReductionViewModel::setReductionMode(NoiseReductionMode mode)
{
    if (reductionMode() == mode) {
        return;
    }

    const NoiseReductionBase* const fx = effect();
    IF_ASSERT_FAILED(fx) {
        return;
    }
    fx->mSettings->mNoiseReductionChoice = mode == NoiseReductionMode::ReduceNoise
                                           ? ::NoiseReductionChoice::NRC_REDUCE_NOISE
                                           : ::NoiseReductionChoice::NRC_LEAVE_RESIDUE;
    emit reductionModeChanged();
}

bool NoiseReductionViewModel::isApplyAllowed() const
{
    const NoiseReductionBase* const fx = effect();
    if (!fx) {
        return false;
    }
    return fx->mStatistics != nullptr;
}

bool NoiseReductionViewModel::getNoiseProfile()
{
    NoiseReductionBase* const fx = effect();
    IF_ASSERT_FAILED(fx) {
        return false;
    }

    const std::shared_ptr<EffectInstance> instance = instancesRegister()->instanceById(instanceId());
    IF_ASSERT_FAILED(instance) {
        return false;
    }

    const auto wasAllowed = isApplyAllowed();
    fx->mSettings->mDoProfile = true;
    auto success = false;
    modifySettings([&](EffectSettings& settings) {
        success = fx->Process(*instance, settings);
    });
    if (isApplyAllowed() && !wasAllowed) {
        emit isApplyAllowedChanged();
    }

    return success;
}
