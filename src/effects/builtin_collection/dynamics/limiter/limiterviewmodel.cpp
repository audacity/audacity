/*
 * Audacity: A Digital Audio Editor
 */
#include "limiterviewmodel.h"

#include "log.h"

namespace au::effects {
LimiterViewModel::LimiterViewModel(QObject* parent, int instanceId)
    : BuiltinEffectModel{parent, instanceId}
{
}

void LimiterViewModel::doReload()
{
}
}
