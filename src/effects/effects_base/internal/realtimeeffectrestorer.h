/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effectstypes.h"
#include "au3wrap/au3types.h"
#include "async/channel.h"
#include "async/notification.h"

namespace au::effects {
class IRealtimeEffectService;

struct RealtimeEffectRestorerSignals {
    muse::async::Channel<TrackId> stackChanged;
    muse::async::Notification settingsChanged;
};

void setRealtimeEffectRestorerSignals(au3::Au3Project&, RealtimeEffectRestorerSignals);
}
