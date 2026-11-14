/*
 * Audacity: A Digital Audio Editor
 */
#include "generatoreffectmodel.h"
#include "generatoreffect.h"
#include "log.h"

#include "au3-components/EffectInterface.h"
#include "au3-effects/Effect.h"

#include "playback/iaudiooutput.h"
#include "trackedit/itrackeditproject.h"

using namespace au::effects;

GeneratorEffectModel::GeneratorEffectModel(QObject* parent, int instanceId)
    : BuiltinEffectModel{parent, instanceId}
{
}

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
    return playback()->audioOutput()->sampleRate();
}

double GeneratorEffectModel::tempo() const
{
    auto project = globalContext()->currentTrackeditProject();
    if (!project) {
        return 0.0;
    }
    return project->timeSignature().tempo;
}

int GeneratorEffectModel::upperTimeSignature() const
{
    auto project = globalContext()->currentTrackeditProject();
    if (!project) {
        return 0;
    }
    return project->timeSignature().upper;
}

int GeneratorEffectModel::lowerTimeSignature() const
{
    auto project = globalContext()->currentTrackeditProject();
    if (!project) {
        return 0;
    }
    return project->timeSignature().lower;
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
