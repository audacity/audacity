/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effectstypes.h"
#include "au3wrap/au3types.h"

namespace au::effects {
class IRealtimeEffectService;
void setNotificationChannelForMasterEffectUndoRedo(au3::Au3Project&, IRealtimeEffectService* service);
}
