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
    GeneratorEffect* const ge = generatorEffect();
    IF_ASSERT_FAILED(ge) {
        return;
    }
    ge->init(&mutSettings());
    emit sampleRateChanged();
    emit tempoChanged();
    emit upperTimeSignatureChanged();
    emit lowerTimeSignatureChanged();
    emit durationChanged();
    emit durationFormatChanged();
    doEmitSignals();

    update();
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

void GeneratorEffectModel::prop_setDuration(double newDuration)
{
    if (!m_inited) {
        return;
    }

    auto e = generatorEffect();
    IF_ASSERT_FAILED(e) {
        return;
    }
    e->setDuration(newDuration);
    emit durationChanged();

    update();
}

QString GeneratorEffectModel::durationFormat() const
{
    const auto e = generatorEffect();
    if (!e) {
        return "";
    }
    return e->durationFormat();
}

void GeneratorEffectModel::prop_setDurationFormat(const QString& newDurationFormat)
{
    if (!m_inited) {
        return;
    }

    auto e = generatorEffect();
    IF_ASSERT_FAILED(e) {
        return;
    }
    e->setDurationFormat(newDurationFormat);
    emit durationFormatChanged();

    update();
}

bool GeneratorEffectModel::isApplyAllowed() const
{
    return generatorEffect()->isApplyAllowed();
}

void GeneratorEffectModel::update()
{
    const auto wasAllowed = m_isApplyAllowed;
    m_isApplyAllowed = isApplyAllowed();
    if (m_isApplyAllowed != wasAllowed) {
        emit isApplyAllowedChanged();
    }
}

GeneratorEffect* GeneratorEffectModel::generatorEffect() const
{
    EffectId effectId = this->effectId();
    Effect* e = effectsProvider()->effect(effectId);
    return dynamic_cast<GeneratorEffect*>(e);
}
