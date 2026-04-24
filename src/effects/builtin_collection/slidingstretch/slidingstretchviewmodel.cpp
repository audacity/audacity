/*
* Audacity: A Digital Audio Editor
*/
#include "slidingstretchviewmodel.h"

#include "au3-builtin-effects/TimeScaleBase.h"

namespace au::effects {
SlidingStretchViewModel::SlidingStretchViewModel(QObject* parent, int instanceId)
    : BuiltinEffectModel(parent, instanceId)
{
}

void SlidingStretchViewModel::doReload()
{
}

double SlidingStretchViewModel::pctToSemitones(double pct) const
{
    return TimeScaleBase::PercentChangeToHalfSteps(pct);
}

double SlidingStretchViewModel::semitonesToPct(double semitones) const
{
    return TimeScaleBase::HalfStepsToPercentChange(semitones);
}
}
