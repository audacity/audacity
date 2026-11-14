/*
* Audacity: A Digital Audio Editor
*/
#include "paulstretchviewmodel.h"

#include "paulstretcheffect.h"

#include "global/log.h"
#include "global/translation.h"
#include "global/types/number.h"

using namespace au::effects;

PaulstretchViewModel::PaulstretchViewModel(QObject* parent, int instanceId)
    : BuiltinEffectModel(parent, instanceId)
{
}

void PaulstretchViewModel::doReload()
{
    emit amountChanged();
    emit timeResolutionChanged();
}

double PaulstretchViewModel::amount() const
{
    return effect<PaulstretchEffect>().mAmount;
}

void PaulstretchViewModel::setAmount(double newAmount)
{
    auto& pe = effect<PaulstretchEffect>();
    const auto clamped = std::clamp(newAmount, PaulstretchEffect::Amount.min, PaulstretchEffect::Amount.max);
    // Don't use `muse::is_equal(pe.mAmount, newAmount)`:
    // we want decimal increments to be possible even for values of 1,000,000 or more, which `is_equal` would reject.
    if (pe.mAmount == clamped) {
        return;
    }
    pe.mAmount = clamped;
    emit amountChanged();
}

double PaulstretchViewModel::timeResolution() const
{
    return effect<PaulstretchEffect>().mTime_resolution;
}

void PaulstretchViewModel::setTimeResolution(double newTimeResolution)
{
    auto& pe = effect<PaulstretchEffect>();
    const auto clamped = std::clamp(newTimeResolution, PaulstretchEffect::Time.min, PaulstretchEffect::Time.max);
    // Same as above: don't use `muse::is_equal
    if (pe.mTime_resolution == clamped) {
        return;
    }
    pe.mTime_resolution = clamped;
    emit timeResolutionChanged();
}

QString PaulstretchViewModel::stretchFactorLabel() const
{
    return muse::qtrc("effects/paulstretch", "Stretch factor");
}

double PaulstretchViewModel::stretchFactorMin() const
{
    return PaulstretchEffect::Amount.min;
}

double PaulstretchViewModel::stretchFactorMax() const
{
    return PaulstretchEffect::Amount.max;
}

double PaulstretchViewModel::stretchFactorStep() const
{
    return 0.1;
}

int PaulstretchViewModel::stretchFactorDecimals() const
{
    return 2;
}

QString PaulstretchViewModel::timeResolutionLabel() const
{
    return muse::qtrc("effects/paulstretch", "Time resolution");
}

double PaulstretchViewModel::timeResolutionMin() const
{
    return PaulstretchEffect::Time.min;
}

double PaulstretchViewModel::timeResolutionMax() const
{
    return PaulstretchEffect::Time.max;
}

double PaulstretchViewModel::timeResolutionStep() const
{
    return 0.01;
}

int PaulstretchViewModel::timeResolutionDecimals() const
{
    return 3;
}

QString PaulstretchViewModel::timeResolutionUnitSymbol() const
{
    return muse::qtrc("global", "s");
}
