/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effectstypes.h"
#include "au3wrap/au3types.h"
#include "async/channel.h"

namespace au::effects {
void setNotificationChannelForMasterEffectUndoRedo(au3::Au3Project&, muse::async::Channel<TrackId> channel);
}
