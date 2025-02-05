/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effectstypes.h"
#include "au3wrap/au3types.h"
#include "project/iaudacityproject.h"
#include "libraries/lib-realtime-effects/RealtimeEffectState.h"
#include "libraries/lib-realtime-effects/RealtimeEffectList.h"

#include <optional>

namespace au::effects::utils {
struct UtilData
{
    au3::Au3Project* const au3Project;
    au3::Au3Track* const au3Track;
    RealtimeEffectList* const effectList;
};

std::optional<TrackId> trackId(const project::IAudacityProjectPtr& project, const RealtimeEffectStatePtr& state);
std::optional<EffectChainLinkIndex> effectIndex(const project::IAudacityProjectPtr& project, const RealtimeEffectStatePtr& state);
std::optional<UtilData> utilData(const project::IAudacityProjectPtr& project, TrackId trackId);
}
