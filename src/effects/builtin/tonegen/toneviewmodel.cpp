/*
 * Audacity: A Digital Audio Editor
 */
#include "toneviewmodel.h"
#include "toneeffect.h"

#include "framework/global/translation.h"

using namespace au::effects;

ToneViewModel::ToneViewModel(QObject* parent, int instanceId)
    : GeneratorEffectModel(parent, instanceId)
{
}

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

bool ToneViewModel::isApplyAllowed() const
{
    return effect<ToneEffect>().isApplyAllowed() && GeneratorEffectModel::isApplyAllowed();
}

QList<QString> ToneViewModel::waveforms() const
{
    return {
        muse::qtrc("effects/tone", "Sine"), muse::qtrc("effects/tone", "Square"), muse::qtrc("effects/tone", "Sawtooth"), muse::qtrc(
            "effects/tone", "Square, no alias"), muse::qtrc("effects/tone", "Triangle")
    };
}

double ToneViewModel::amplitudeStart() const
{
    return effect<ToneEffect>().amplitudeStart();
}

void ToneViewModel::prop_setAmplitudeStart(double newAmplitude)
{
    const auto wasAllowed = isApplyAllowed();

    if (effect<ToneEffect>().amplitudeStart() == newAmplitude) {
        return;
    }

    effect<ToneEffect>().setAmplitudeStart(newAmplitude);
    emit amplitudeStartChanged();

    if (wasAllowed != isApplyAllowed()) {
        emit isApplyAllowedChanged();
    }
}

double ToneViewModel::amplitudeEnd() const
{
    return effect<ToneEffect>().amplitudeEnd();
}

void ToneViewModel::prop_setAmplitudeEnd(double newAmplitude)
{
    const auto wasAllowed = isApplyAllowed();

    if (effect<ToneEffect>().amplitudeEnd() == newAmplitude) {
        return;
    }

    effect<ToneEffect>().setAmplitudeEnd(newAmplitude);
    emit amplitudeEndChanged();

    if (wasAllowed != isApplyAllowed()) {
        emit isApplyAllowedChanged();
    }
}

double ToneViewModel::frequencyStart() const
{
    return effect<ToneEffect>().frequencyStart();
}

void ToneViewModel::prop_setFrequencyStart(double newFrequency)
{
    const auto wasAllowed = isApplyAllowed();

    if (effect<ToneEffect>().frequencyStart() == newFrequency) {
        return;
    }

    effect<ToneEffect>().setFrequencyStart(newFrequency);
    emit frequencyStartChanged();

    if (wasAllowed != isApplyAllowed()) {
        emit isApplyAllowedChanged();
    }
}

double ToneViewModel::frequencyEnd() const
{
    return effect<ToneEffect>().frequencyEnd();
}

void ToneViewModel::prop_setFrequencyEnd(double newFrequency)
{
    const auto wasAllowed = isApplyAllowed();

    if (effect<ToneEffect>().frequencyEnd() == newFrequency) {
        return;
    }

    effect<ToneEffect>().setFrequencyEnd(newFrequency);
    emit frequencyEndChanged();

    if (wasAllowed != isApplyAllowed()) {
        emit isApplyAllowedChanged();
    }
}

int ToneViewModel::waveform() const
{
    return static_cast<int>(effect<ToneEffect>().waveform());
}

void ToneViewModel::prop_setWaveform(int newWaveform)
{
    if (effect<ToneEffect>().waveform() == newWaveform) {
        return;
    }

    effect<ToneEffect>().setWaveform(static_cast<ToneEffect::Waveform>(newWaveform));
    emit waveformChanged();
}

int ToneViewModel::interpolation() const
{
    return static_cast<int>(effect<ToneEffect>().interpolation());
}

void ToneViewModel::prop_setInterpolation(int newInterpolation)
{
    if (effect<ToneEffect>().interpolation() == newInterpolation) {
        return;
    }

    effect<ToneEffect>().setInterpolation(static_cast<ToneEffect::Interpolation>(newInterpolation));
    emit interpolationChanged();
}
