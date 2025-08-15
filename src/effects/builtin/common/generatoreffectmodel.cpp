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
    auto& ge = effect<GeneratorEffect>();
    ge.init(&mutSettings());
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
    const auto& e = effect<GeneratorEffect>();
    return e.sampleRate();
}

double GeneratorEffectModel::tempo() const
{
    const auto& e = effect<GeneratorEffect>();
    return e.tempo();
}

int GeneratorEffectModel::upperTimeSignature() const
{
    const auto& e = effect<GeneratorEffect>();
    return e.upperTimeSignature();
}

int GeneratorEffectModel::lowerTimeSignature() const
{
    const auto& e = effect<GeneratorEffect>();
    return e.lowerTimeSignature();
}

double GeneratorEffectModel::duration() const
{
    const auto& e = effect<GeneratorEffect>();
    return e.duration();
}

void GeneratorEffectModel::prop_setDuration(double newDuration)
{
    auto& e = effect<GeneratorEffect>();
    e.setDuration(newDuration);
    emit durationChanged();

    update();
}

QString GeneratorEffectModel::durationFormat() const
{
    const auto& e = effect<GeneratorEffect>();
    return e.durationFormat();
}

void GeneratorEffectModel::prop_setDurationFormat(const QString& newDurationFormat)
{
    auto& e = effect<GeneratorEffect>();
    e.setDurationFormat(newDurationFormat);
    emit durationFormatChanged();

    update();
}

bool GeneratorEffectModel::isApplyAllowed() const
{
    const auto& e = effect<GeneratorEffect>();
    return e.isApplyAllowed();
}

void GeneratorEffectModel::update()
{
    const auto wasAllowed = m_isApplyAllowed;
    m_isApplyAllowed = isApplyAllowed();
    if (m_isApplyAllowed != wasAllowed) {
        emit isApplyAllowedChanged();
    }
}
