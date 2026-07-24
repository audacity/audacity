/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "abi.h"

namespace au::audacityplugin::internal {
Status invokeOfflineEffect(const aup_effect_offline_v0& offline, const OfflineArgs& args, IOfflineHost& host) noexcept;
} // namespace au::audacityplugin::internal
