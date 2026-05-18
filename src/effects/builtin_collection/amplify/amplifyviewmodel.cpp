/*
* Audacity: A Digital Audio Editor
*/
#include "amplifyviewmodel.h"

#include "amplifyeffect.h"

#include "global/log.h"
#include "global/translation.h"

using namespace au::effects;

AmplifyViewModel::AmplifyViewModel(QObject* parent, int instanceId)
    : BuiltinEffectModel(parent, instanceId)
{
}

void AmplifyViewModel::doReload()
{
    emit canClipChanged();
    update();
}

void AmplifyViewModel::update()
{
    emit ampValueChanged();
    emit newPeakValueChanged();
    emit isApplyAllowedChanged();
}

QString AmplifyViewModel::effectTitle() const
{
    return muse::qtrc("effects/amplify", "Amplify");
}

QString AmplifyViewModel::ampLabel() const
{
    return muse::qtrc("effects/amplify", "Amplification");
}

float AmplifyViewModel::ampValue() const
{
    return effect<AmplifyEffect>().amp().val.raw();
}

void AmplifyViewModel::setAmpValue(float newAmpValue)
{
    auto& ae = effect<AmplifyEffect>();
    const au::shared::Decibel newAmp { newAmpValue };
    if (ae.amp().val == newAmp) {
        return;
    }
    ae.setAmp(newAmp);
    update();
}

float AmplifyViewModel::ampMin() const
{
    return effect<AmplifyEffect>().amp().min.raw();
}

float AmplifyViewModel::ampMax() const
{
    return effect<AmplifyEffect>().amp().max.raw();
}

QString AmplifyViewModel::ampMeasureUnitsSymbol() const
{
    return muse::qtrc("global", "dB");
}

int AmplifyViewModel::ampDecimals() const
{
    return 4;
}

double AmplifyViewModel::ampStep() const
{
    return 0.02;
}

QString AmplifyViewModel::newPeakLabel() const
{
    return muse::qtrc("effects/amplify", "New peak amplitude");
}

float AmplifyViewModel::newPeakValue() const
{
    const auto& ae = effect<AmplifyEffect>();
    return au::shared::Decibel::fromLinear(ae.ratio() * ae.inputPeak()).raw();
}

void AmplifyViewModel::setNewPeakValue(float newNewPeakValue)
{
    auto& ae = effect<AmplifyEffect>();
    const au::shared::Decibel newNewPeak { newNewPeakValue };
    if (au::shared::Decibel::fromLinear(ae.ratio() * ae.inputPeak()) == newNewPeak) {
        return;
    }
    const ratio_t ratio = newNewPeak.toLinear() / ae.inputPeak();
    ae.setAmp(au::shared::Decibel::fromLinear(ratio));
    update();
}

float AmplifyViewModel::newPeakMin() const
{
    const auto& ae = effect<AmplifyEffect>();
    return (ae.amp().min + au::shared::Decibel::fromLinear(ae.inputPeak())).raw();
}

float AmplifyViewModel::newPeakMax() const
{
    const auto& ae = effect<AmplifyEffect>();
    return (ae.amp().max + au::shared::Decibel::fromLinear(ae.inputPeak())).raw();
}

QString AmplifyViewModel::newPeakMeasureUnitsSymbol() const
{
    return muse::qtrc("global", "dB");
}

int AmplifyViewModel::newPeakDecimals() const
{
    return 4;
}

double AmplifyViewModel::newPeakStep() const
{
    return 0.02;
}

QString AmplifyViewModel::canClipLabel() const
{
    return muse::qtrc("effects/amplify", "Allow clipping");
}

bool AmplifyViewModel::canClip() const
{
    return effect<AmplifyEffect>().canClip();
}

void AmplifyViewModel::setCanClip(bool newClipping)
{
    auto& ae = effect<AmplifyEffect>();
    if (ae.canClip() == newClipping) {
        return;
    }
    ae.setCanClip(newClipping);
    emit canClipChanged();
    update();
}

bool AmplifyViewModel::isApplyAllowed() const
{
    const auto& ae = effect<AmplifyEffect>();
    return ae.canClip() || ae.ratio() * ae.inputPeak() <= 1.0f;
}
