/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effectstypes.h"
#include "au3wrap/au3types.h"
#include "async/channel.h"

namespace au::effects {
class IRealtimeEffectStateRegister;

void setupMasterEffectUndoRedo(au3::Au3Project&, muse::async::Channel<TrackId> trackEffectsChanged,
                               IRealtimeEffectStateRegister* stateRegister);
}
