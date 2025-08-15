/*
 * Audacity: A Digital Audio Editor
 */
#include "noisereductioneffect.h"
#include "noisereductionviewmodel.h"

#include "log.h"

using namespace au::effects;

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
    return settings<NoiseReductionSettings>().mNoiseGain;
}

void NoiseReductionViewModel::setReduction(int newReduction)
{
    if (reduction() == newReduction) {
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
    return settings<NoiseReductionSettings>().mNewSensitivity;
}

void NoiseReductionViewModel::setSensitivity(double newSensitivity)
{
    if (muse::is_equal(sensitivity(), newSensitivity)) {
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
    return static_cast<int>(settings<NoiseReductionSettings>().mFreqSmoothingBands);
}

void NoiseReductionViewModel::setFrequencySmoothingBands(int newFrequencySmoothingBands)
{
    if (frequencySmoothingBands() == newFrequencySmoothingBands) {
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
    return settings<NoiseReductionSettings>().mNoiseReductionChoice;
}

void NoiseReductionViewModel::setReductionMode(int mode)
{
    if (reductionMode() == mode) {
        return;
    }
    modifySettings([&](EffectSettings& settings) {
        settings.cast<NoiseReductionSettings>()->mNoiseReductionChoice = static_cast<NoiseReductionChoice>(mode);
    });
    emit reductionModeChanged();
}

bool NoiseReductionViewModel::isApplyAllowed() const
{
    const auto& fx = effect<NoiseReductionEffect>();
    return fx.mStatistics != nullptr;
}

void NoiseReductionViewModel::getNoiseProfile()
{
    const std::shared_ptr<EffectInstance> instance = instancesRegister()->instanceById(instanceId());
    IF_ASSERT_FAILED(instance) {
        return;
    }

    const auto wasAllowed = isApplyAllowed();
    modifySettings([&](EffectSettings& settings) {
        settings.cast<NoiseReductionSettings>()->mDoProfile = true;
    });

    auto& fx = effect<NoiseReductionEffect>();
    auto success = false;

    modifySettings([&](EffectSettings& settings) {
        fx.ResetLastError();
        success = fx.Process(*instance, settings);
    });

    if (isApplyAllowed() && !wasAllowed) {
        emit isApplyAllowedChanged();
    }

    if (!fx.GetLastError().empty()) {
        success ? interactive()->warningSync(muse::trc("effects/noisereduction", "Warning"), fx.GetLastError())
        : interactive()->errorSync(muse::trc("effects/noisereduction", "Error"), fx.GetLastError());
    }
}
