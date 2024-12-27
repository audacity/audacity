/*
 * Audacity: A Digital Audio Editor
 */
#include "toneviewmodel.h"
#include "toneeffect.h"
#include "log.h"

#include "libraries/lib-components/EffectInterface.h"

using namespace au::effects;

void ToneViewModel::doEmitSignals()
{
    emit amplitudeStartChanged();
    emit amplitudeEndChanged();
    emit frequencyStartChanged();
    emit frequencyEndChanged();
    emit waveformChanged();
    emit interpolationChanged();
    emit isApplyAllowedChanged();
}

ToneEffect* ToneViewModel::effect() const
{
    EffectId effectId = this->effectId();
    Effect* e = effectsProvider()->effect(effectId);
    ToneEffect* te = dynamic_cast<ToneEffect*>(e);
    return te;
}

bool ToneViewModel::isApplyAllowed() const
{
    const ToneEffect* const te = effect();
    if (!te) {
        return false;
    }
    return te->isApplyAllowed() && GeneratorEffectModel::isApplyAllowed();
}

QList<QString> ToneViewModel::waveforms() const
{
    return {
        tr("Sine"), tr("Square"), tr("Sawtooth"), tr("Square, not alias"), tr("Triangle")
    };
}

QList<QString> ToneViewModel::interpolationTypes() const
{
    return {
        tr("Linear"), tr("Logarithmic")
    };
}

double ToneViewModel::amplitudeStart() const
{
    const ToneEffect* const te = effect();
    if (!te) {
        return 0.0;
    }
    return te->amplitudeStart();
}

void ToneViewModel::prop_setAmplitudeStart(double newAmplitude)
{
    if (!m_inited) {
        return;
    }
    const auto wasAllowed = isApplyAllowed();
    ToneEffect* const te = effect();
    IF_ASSERT_FAILED(te) {
        return;
    }
    te->setAmplitudeStart(newAmplitude);
    if (wasAllowed != isApplyAllowed()) {
        emit isApplyAllowedChanged();
    }
}

double ToneViewModel::amplitudeEnd() const
{
    const ToneEffect* const te = effect();
    if (!te) {
        return 0.0;
    }
    return te->amplitudeEnd();
}

void ToneViewModel::prop_setAmplitudeEnd(double newAmplitude)
{
    if (!m_inited) {
        return;
    }
    const auto wasAllowed = isApplyAllowed();
    ToneEffect* const te = effect();
    IF_ASSERT_FAILED(te) {
        return;
    }
    te->setAmplitudeEnd(newAmplitude);
    if (wasAllowed != isApplyAllowed()) {
        emit isApplyAllowedChanged();
    }
}

double ToneViewModel::frequencyStart() const
{
    const ToneEffect* const te = effect();
    if (!te) {
        return 0.0;
    }
    return te->frequencyStart();
}

void ToneViewModel::prop_setFrequencyStart(double newFrequency)
{
    if (!m_inited) {
        return;
    }
    const auto wasAllowed = isApplyAllowed();
    ToneEffect* const te = effect();
    IF_ASSERT_FAILED(te) {
        return;
    }
    te->setFrequencyStart(newFrequency);
    if (wasAllowed != isApplyAllowed()) {
        emit isApplyAllowedChanged();
    }
}

double ToneViewModel::frequencyEnd() const
{
    const ToneEffect* const te = effect();
    if (!te) {
        return 0.0;
    }
    return te->frequencyEnd();
}

void ToneViewModel::prop_setFrequencyEnd(double newFrequency)
{
    if (!m_inited) {
        return;
    }
    const auto wasAllowed = isApplyAllowed();
    ToneEffect* const te = effect();
    IF_ASSERT_FAILED(te) {
        return;
    }
    te->setFrequencyEnd(newFrequency);
    if (wasAllowed != isApplyAllowed()) {
        emit isApplyAllowedChanged();
    }
}

int ToneViewModel::waveform() const
{
    const ToneEffect* const te = effect();
    if (!te) {
        return 0;
    }
    return static_cast<int>(te->waveform());
}

void ToneViewModel::prop_setWaveform(int newWaveform)
{
    if (!m_inited) {
        return;
    }
    ToneEffect* const te = effect();
    IF_ASSERT_FAILED(te) {
        return;
    }
    te->setWaveform(static_cast<ToneEffect::Waveform>(newWaveform));
}

int ToneViewModel::interpolation() const
{
    const ToneEffect* const te = effect();
    if (!te) {
        return 0;
    }
    return static_cast<int>(te->interpolation());
}

void ToneViewModel::prop_setInterpolation(int newInterpolation)
{
    if (!m_inited) {
        return;
    }
    ToneEffect* const te = effect();
    IF_ASSERT_FAILED(te) {
        return;
    }
    te->setInterpolation(static_cast<ToneEffect::Interpolation>(newInterpolation));
}
