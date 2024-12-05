/*
 * Audacity: A Digital Audio Editor
 */
#include "toneviewmodel.h"
#include "toneeffect.h"
#include "log.h"

#include "libraries/lib-components/EffectInterface.h"

using namespace au::effects;

void ToneViewModel::doReload()
{
    emit amplitudeChanged();
    emit frequencyChanged();
    emit waveformChanged();
    emit isApplyAllowedChanged();
}

ToneEffect* ToneViewModel::effect() const
{
    ToneEffect* const e = dynamic_cast<ToneEffect*>(AbstractEffectModel::effect());
    return e;
}

bool ToneViewModel::isApplyAllowed() const
{
    const ToneEffect* const te = effect();
    if (!te) {
        return false;
    }
    return te->isApplyAllowed();
}

QList<QString> ToneViewModel::waveforms() const
{
    return {
        tr("Sine"), tr("Square"), tr("Sawtooth"), tr("Square, not alias"), tr("Triangle")
    };
}

double ToneViewModel::amplitude() const
{
    const ToneEffect* const te = effect();
    if (!te) {
        return 0.0;
    }
    return te->amplitude();
}

void ToneViewModel::setAmplitude(double newAmplitude)
{
    const auto wasAllowed = isApplyAllowed();
    ToneEffect* const te = effect();
    IF_ASSERT_FAILED(te) {
        return;
    }
    te->setAmplitude(newAmplitude);
    if (wasAllowed != isApplyAllowed()) {
        emit isApplyAllowedChanged();
    }
}

double ToneViewModel::frequency() const
{
    const ToneEffect* const te = effect();
    if (!te) {
        return 0.0;
    }
    return te->frequency();
}

void ToneViewModel::setFrequency(double newFrequency)
{
    const auto wasAllowed = isApplyAllowed();
    ToneEffect* const te = effect();
    IF_ASSERT_FAILED(te) {
        return;
    }
    te->setFrequency(newFrequency);
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

void ToneViewModel::setWaveform(int newWaveform)
{
    ToneEffect* const te = effect();
    IF_ASSERT_FAILED(te) {
        return;
    }
    te->setWaveform(static_cast<ToneEffect::Waveform>(newWaveform));
}
