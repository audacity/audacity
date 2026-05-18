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
    return effect<AmplifyEffect>().amp().val;
}

void AmplifyViewModel::setAmpValue(float newAmpValue)
{
    auto& ae = effect<AmplifyEffect>();
    const db_t newAmp = newAmpValue;
    if (muse::is_equal<db_t>(ae.amp().val, newAmp)) {
        return;
    }
    ae.setAmp(newAmp);
    update();
}

float AmplifyViewModel::ampMin() const
{
    return effect<AmplifyEffect>().amp().min;
}

float AmplifyViewModel::ampMax() const
{
    return effect<AmplifyEffect>().amp().max;
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
    return muse::linear_to_db(ae.ratio() * ae.peak());
}

void AmplifyViewModel::setNewPeakValue(float newNewPeakValue)
{
    auto& ae = effect<AmplifyEffect>();
    const db_t newNewPeak = newNewPeakValue;
    if (muse::is_equal<db_t>(newPeakValue(), newNewPeak)) {
        return;
    }
    const ratio_t ratio = muse::db_to_linear(newNewPeak) / ae.peak();
    ae.setAmp(muse::linear_to_db(ratio));
    update();
}

float AmplifyViewModel::newPeakMin() const
{
    const auto& ae = effect<AmplifyEffect>();
    return ae.amp().min + muse::linear_to_db(ae.peak());
}

float AmplifyViewModel::newPeakMax() const
{
    const auto& ae = effect<AmplifyEffect>();
    return ae.amp().max + muse::linear_to_db(ae.peak());
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
    return ae.canClip() || ae.ratio() * ae.peak() <= 1.0f;
}
