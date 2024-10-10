/*
* Audacity: A Digital Audio Editor
*/
#include "toneviewmodel.h"
#include "toneeffect.h"
#include "log.h"

#include "libraries/lib-components/EffectInterface.h"

using namespace au::effects;

ToneEffect* ToneViewModel::effect() const
{
    const auto instance = AbstractEffectModel::effect();
    IF_ASSERT_FAILED(instance) {
        return nullptr;
    }
    ToneEffect* const e = dynamic_cast<ToneEffect*>(instance);
    IF_ASSERT_FAILED(e) {
        return nullptr;
    }
    return e;
}

void ToneViewModel::init()
{
    ToneEffect* const te = effect();
    const auto s = settings();
    IF_ASSERT_FAILED(te && s) {
        return;
    }
    te->init(*s);
}

double ToneViewModel::sampleRate() const
{
    const auto te = effect();
    IF_ASSERT_FAILED(te) {
        return 1.0;
    }
    return te->sampleRate();
}

double ToneViewModel::amplitude() const
{
    const ToneEffect* const te = effect();
    IF_ASSERT_FAILED(te) {
        return 0.0;
    }
    return te->amplitude();
}

void ToneViewModel::setAmplitude(double newAmplitude)
{
    ToneEffect* const te = effect();
    IF_ASSERT_FAILED(te) {
        return;
    }
    te->setAmplitude(newAmplitude);
}

double ToneViewModel::frequency() const
{
    const ToneEffect* const te = effect();
    IF_ASSERT_FAILED(te) {
        return 0.0;
    }
    return te->frequency();
}

void ToneViewModel::setFrequency(double newFrequency)
{
    ToneEffect* const te = effect();
    IF_ASSERT_FAILED(te) {
        return;
    }
    te->setFrequency(newFrequency);
}

int ToneViewModel::waveform() const
{
    const ToneEffect* const te = effect();
    IF_ASSERT_FAILED(te) {
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

double ToneViewModel::duration() const
{
    if (const auto s = settings()) {
        return s->extra.GetDuration();
    }
    return 0.0;
}

void ToneViewModel::setDuration(double newDuration)
{
    auto e = effect();
    auto s = settings();
    IF_ASSERT_FAILED(e && s) {
        return;
    }
    s->extra.SetDuration(newDuration);
    e->setDuration(newDuration);
}
