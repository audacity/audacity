/*
 * Audacity: A Digital Audio Editor
 */
#include "generatoreffectmodel.h"
#include "generatoreffect.h"
#include "log.h"

#include "libraries/lib-components/EffectInterface.h"
#include "libraries/lib-effects/Effect.h"

using namespace au::effects;

void GeneratorEffectModel::doReload()
{
    auto* const ge = dynamic_cast<GeneratorEffect*>(effect());
    IF_ASSERT_FAILED(ge) {
        return;
    }
    ge->init(settings());
    emit sampleRateChanged();
    emit tempoChanged();
    emit upperTimeSignatureChanged();
    emit lowerTimeSignatureChanged();
    emit durationChanged();
    emit durationFormatChanged();
    doEmitSignals();
}

double GeneratorEffectModel::sampleRate() const
{
    const auto e = generatorEffect();
    if (!e) {
        return 1.0;
    }
    return e->sampleRate();
}

double GeneratorEffectModel::tempo() const
{
    const auto e = generatorEffect();
    if (!e) {
        return 120.0;
    }
    return e->tempo();
}

int GeneratorEffectModel::upperTimeSignature() const
{
    const auto e = generatorEffect();
    if (!e) {
        return 4;
    }
    return e->upperTimeSignature();
}

int GeneratorEffectModel::lowerTimeSignature() const
{
    const auto e = generatorEffect();
    if (!e) {
        return 4;
    }
    return e->lowerTimeSignature();
}

double GeneratorEffectModel::duration() const
{
    const auto e = generatorEffect();
    if (!e) {
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
    emit durationChanged();
}

QString GeneratorEffectModel::durationFormat() const
{
    const auto e = generatorEffect();
    if (!e) {
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
    emit durationFormatChanged();
}

GeneratorEffect* GeneratorEffectModel::generatorEffect() const
{
    return dynamic_cast<GeneratorEffect*>(effect());
}
