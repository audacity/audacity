/*
 * Audacity: A Digital Audio Editor
 */
#include "toneviewmodel.h"
#include "toneeffect.h"
#include "log.h"

#include "global/translation.h"

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
        muse::qtrc("effects/tone", "Sine"), muse::qtrc("effects/tone", "Square"), muse::qtrc("effects/tone", "Sawtooth"), muse::qtrc(
            "effects/tone", "Square, no alias"), muse::qtrc("effects/tone", "Triangle")
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
    const auto wasAllowed = isApplyAllowed();

    ToneEffect* const te = effect();

    IF_ASSERT_FAILED(te) {
        return;
    }

    if (te->amplitudeStart() == newAmplitude) {
        return;
    }

    te->setAmplitudeStart(newAmplitude);
    emit amplitudeStartChanged();

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
    const auto wasAllowed = isApplyAllowed();

    ToneEffect* const te = effect();

    IF_ASSERT_FAILED(te) {
        return;
    }

    if (te->amplitudeEnd() == newAmplitude) {
        return;
    }

    te->setAmplitudeEnd(newAmplitude);
    emit amplitudeEndChanged();

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
    const auto wasAllowed = isApplyAllowed();

    ToneEffect* const te = effect();

    IF_ASSERT_FAILED(te) {
        return;
    }

    if (te->frequencyStart() == newFrequency) {
        return;
    }

    te->setFrequencyStart(newFrequency);
    emit frequencyStartChanged();

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
    const auto wasAllowed = isApplyAllowed();

    ToneEffect* const te = effect();

    IF_ASSERT_FAILED(te) {
        return;
    }

    if (te->frequencyEnd() == newFrequency) {
        return;
    }

    te->setFrequencyEnd(newFrequency);
    emit frequencyEndChanged();

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
    ToneEffect* const te = effect();

    IF_ASSERT_FAILED(te) {
        return;
    }

    if (te->waveform() == newWaveform) {
        return;
    }

    te->setWaveform(static_cast<ToneEffect::Waveform>(newWaveform));
    emit waveformChanged();
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
    ToneEffect* const te = effect();

    IF_ASSERT_FAILED(te) {
        return;
    }

    if (te->interpolation() == newInterpolation) {
        return;
    }

    te->setInterpolation(static_cast<ToneEffect::Interpolation>(newInterpolation));
    emit interpolationChanged();
}
