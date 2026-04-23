/*
* Audacity: A Digital Audio Editor
*/
#include "slidingstretchviewmodel.h"

namespace au::effects {
SlidingStretchViewModel::SlidingStretchViewModel(QObject* parent, int instanceId)
    : BuiltinEffectModel(parent, instanceId)
{
}

void SlidingStretchViewModel::doReload()
{
}
}
