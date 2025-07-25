/*
 * Audacity: A Digital Audio Editor
 */
#include "noisereductioneffect.h"
#include "noisereductionviewmodel.h"

#include "log.h"

using namespace au::effects;

NoiseReductionEffect* NoiseReductionViewModel::effect() const
{
    EffectId effectId = this->effectId();
    if (effectId.isEmpty()) {
        return nullptr;
    }
    Effect* e = effectsProvider()->effect(effectId);
    return dynamic_cast<NoiseReductionEffect*>(e);
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
    const NoiseReductionEffect* const fx = effect();
    if (!fx) {
        return 0;
    }
    return settings<NoiseReductionSettings>().mNoiseGain;
}

void NoiseReductionViewModel::setReduction(int newReduction)
{
    if (reduction() == newReduction) {
        return;
    }
    NoiseReductionEffect* const fx = effect();
    IF_ASSERT_FAILED(fx) {
        return;
    }
    modifySettings([&](EffectSettings& settings) {
        settings.cast<NoiseReductionSettings>()->mNoiseGain = newReduction;
    });
    emit reductionChanged();
}

int NoiseReductionViewModel::reductionMin() const
{
    return NoiseReductionEffect::noiseGain.min;
}

int NoiseReductionViewModel::reductionMax() const
{
    return NoiseReductionEffect::noiseGain.max;
}

double NoiseReductionViewModel::sensitivity() const
{
    const NoiseReductionEffect* const fx = effect();
    if (!fx) {
        return 0.0f;
    }
    return settings<NoiseReductionSettings>().mNewSensitivity;
}

void NoiseReductionViewModel::setSensitivity(double newSensitivity)
{
    if (!m_inited) {
        return;
    }
    if (muse::is_equal(sensitivity(), newSensitivity)) {
        return;
    }
    NoiseReductionEffect* const fx = effect();
    IF_ASSERT_FAILED(fx) {
        return;
    }
    modifySettings([&](EffectSettings& settings) {
        settings.cast<NoiseReductionSettings>()->mNewSensitivity = newSensitivity;
    });
    emit sensitivityChanged();
}

double NoiseReductionViewModel::sensitivityMin() const
{
    return NoiseReductionEffect::sensitivity.min;
}

double NoiseReductionViewModel::sensitivityMax() const
{
    return NoiseReductionEffect::sensitivity.max;
}

int NoiseReductionViewModel::frequencySmoothingBands() const
{
    const NoiseReductionEffect* const fx = effect();
    if (!fx) {
        return 0;
    }
    return static_cast<int>(settings<NoiseReductionSettings>().mFreqSmoothingBands);
}

void NoiseReductionViewModel::setFrequencySmoothingBands(int newFrequencySmoothingBands)
{
    if (!m_inited) {
        return;
    }
    if (frequencySmoothingBands() == newFrequencySmoothingBands) {
        return;
    }
    NoiseReductionEffect* const fx = effect();
    IF_ASSERT_FAILED(fx) {
        return;
    }
    modifySettings([&](EffectSettings& settings) {
        settings.cast<NoiseReductionSettings>()->mFreqSmoothingBands = static_cast<double>(newFrequencySmoothingBands);
    });
    emit frequencySmoothingBandsChanged();
}

int NoiseReductionViewModel::frequencySmoothingBandsMin()
{
    return NoiseReductionEffect::frequencySmoothingBands.min;
}

int NoiseReductionViewModel::frequencySmoothingBandsMax() const
{
    return NoiseReductionEffect::frequencySmoothingBands.max;
}

int NoiseReductionViewModel::reductionMode() const
{
    const NoiseReductionEffect* const fx = effect();
    if (!fx) {
        return 0;
    }
    return settings<NoiseReductionSettings>().mNoiseReductionChoice;
}

void NoiseReductionViewModel::setReductionMode(int mode)
{
    if (!m_inited) {
        return;
    }

    if (reductionMode() == mode) {
        return;
    }

    const NoiseReductionEffect* const fx = effect();
    IF_ASSERT_FAILED(fx) {
        return;
    }
    modifySettings([&](EffectSettings& settings) {
        settings.cast<NoiseReductionSettings>()->mNoiseReductionChoice = static_cast<NoiseReductionChoice>(mode);
    });
    emit reductionModeChanged();
}

bool NoiseReductionViewModel::isApplyAllowed() const
{
    const NoiseReductionEffect* const fx = effect();
    if (!fx) {
        return false;
    }
    return fx->mStatistics != nullptr;
}

void NoiseReductionViewModel::getNoiseProfile()
{
    NoiseReductionEffect* const fx = effect();
    IF_ASSERT_FAILED(fx) {
        return;
    }

    const std::shared_ptr<EffectInstance> instance = instancesRegister()->instanceById(instanceId());
    IF_ASSERT_FAILED(instance) {
        return;
    }

    const auto wasAllowed = isApplyAllowed();
    modifySettings([&](EffectSettings& settings) {
        settings.cast<NoiseReductionSettings>()->mDoProfile = true;
    });
    auto success = false;
    modifySettings([&](EffectSettings& settings) {
        fx->ResetLastError();
        success = fx->Process(*instance, settings);
    });

    if (isApplyAllowed() && !wasAllowed) {
        emit isApplyAllowedChanged();
    }

    if (!fx->GetLastError().empty()) {
        success ? interactive()->warningSync(muse::trc("effects/noisereduction", "Warning"), fx->GetLastError())
        : interactive()->errorSync(muse::trc("effects/noisereduction", "Error"), fx->GetLastError());
    }
}
