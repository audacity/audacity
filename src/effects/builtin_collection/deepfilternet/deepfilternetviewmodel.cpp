/*
 * Audacity: A Digital Audio Editor
 */
#include "deepfilternetviewmodel.h"

#include "deepfilterneteffect.h"

#include "global/translation.h"

#include <algorithm>
#include <cmath>

namespace au::effects {
DeepFilterNetViewModel::DeepFilterNetViewModel(QObject* parent, int instanceId)
    : BuiltinEffectModel(parent, instanceId)
{
}

void DeepFilterNetViewModel::doReload()
{
    emit attenuationLimitChanged();
    emit mixChanged();
}

QString DeepFilterNetViewModel::effectTitle() const
{
    return muse::qtrc("effects/deepfilternet", "DeepFilterNet noise reduction");
}

double DeepFilterNetViewModel::attenuationLimit() const
{
    return settings<DeepFilterNetSettings>().mAttenuationLimitDb;
}

void DeepFilterNetViewModel::setAttenuationLimit(double value)
{
    value = std::clamp(value, attenuationLimitMin(), attenuationLimitMax());
    if (std::abs(attenuationLimit() - value) < 0.000001) {
        return;
    }

    modifySettings<DeepFilterNetSettings>([&](EffectSettings& settings) {
        settings.cast<DeepFilterNetSettings>()->mAttenuationLimitDb = value;
    });
    emit attenuationLimitChanged();
}

double DeepFilterNetViewModel::attenuationLimitMin() const
{
    return DeepFilterNetEffect::attenuationLimit.min;
}

double DeepFilterNetViewModel::attenuationLimitMax() const
{
    return DeepFilterNetEffect::attenuationLimit.max;
}

double DeepFilterNetViewModel::mix() const
{
    return settings<DeepFilterNetSettings>().mMix;
}

void DeepFilterNetViewModel::setMix(double value)
{
    value = std::clamp(value, mixMin(), mixMax());
    if (std::abs(mix() - value) < 0.000001) {
        return;
    }

    modifySettings<DeepFilterNetSettings>([&](EffectSettings& settings) {
        settings.cast<DeepFilterNetSettings>()->mMix = value;
    });
    emit mixChanged();
}

double DeepFilterNetViewModel::mixMin() const
{
    return DeepFilterNetEffect::mix.min;
}

double DeepFilterNetViewModel::mixMax() const
{
    return DeepFilterNetEffect::mix.max;
}
}
