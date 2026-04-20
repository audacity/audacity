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
    if (muse::is_equal<double>(pe.mAmount, newAmount)) {
        return;
    }
    pe.mAmount = static_cast<float>(newAmount);
    emit amountChanged();
}

double PaulstretchViewModel::timeResolution() const
{
    return effect<PaulstretchEffect>().mTime_resolution;
}

void PaulstretchViewModel::setTimeResolution(double newTimeResolution)
{
    auto& pe = effect<PaulstretchEffect>();
    if (muse::is_equal<double>(pe.mTime_resolution, newTimeResolution)) {
        return;
    }
    pe.mTime_resolution = static_cast<float>(newTimeResolution);
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
