/*
* Audacity: A Digital Audio Editor
*/
#include "slidingstretchviewmodel.h"

#include "slidingstretcheffect.h"

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
    return SlidingStretchEffect::PercentChangeToHalfSteps(pct);
}

double SlidingStretchViewModel::semitonesToPct(double semitones) const
{
    return SlidingStretchEffect::HalfStepsToPercentChange(semitones);
}
}
