/*
 * Audacity: A Digital Audio Editor
 */
#include "limiterviewmodel.h"

#include "log.h"

namespace au::effects {
LimiterViewModel::LimiterViewModel(QObject* parent)
    : BuiltinEffectModel{parent}
{
}

void LimiterViewModel::doReload()
{
}
}
