/*
 * Audacity: A Digital Audio Editor
 */
#include "generatoreffectmodel.h"
#include "generatoreffect.h"
#include "log.h"

#include "libraries/lib-components/EffectInterface.h"
#include "libraries/lib-effects/Effect.h"

using namespace au::effects;

void GeneratorEffectModel::init()
{
    auto* const ge = dynamic_cast<GeneratorEffect*>(effect());
    IF_ASSERT_FAILED(ge) {
        return;
    }
    ge->init(settings());
    emit sampleRateChanged();
    emit durationChanged();
    emit durationFormatChanged();
}

double GeneratorEffectModel::sampleRate() const
{
    const auto ge = generatorEffect();
    IF_ASSERT_FAILED(ge) {
        return 1.0;
    }
    return ge->sampleRate();
}

double GeneratorEffectModel::duration() const
{
    const auto e = generatorEffect();
    IF_ASSERT_FAILED(e) {
        return 0.0;
    }
    return e->duration();
}

void GeneratorEffectModel::setDuration(double newDuration)
{
    auto e = generatorEffect();
    IF_ASSERT_FAILED(e) {
        return;
    }
    e->setDuration(newDuration);
}

QString GeneratorEffectModel::durationFormat() const
{
    const auto e = generatorEffect();
    IF_ASSERT_FAILED(e) {
        return "";
    }
    return e->durationFormat();
}

void GeneratorEffectModel::setDurationFormat(const QString& newDurationFormat)
{
    auto e = generatorEffect();
    IF_ASSERT_FAILED(e) {
        return;
    }
    e->setDurationFormat(newDurationFormat);
}

GeneratorEffect* GeneratorEffectModel::generatorEffect() const
{
    return dynamic_cast<GeneratorEffect*>(effect());
}
