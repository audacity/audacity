/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/global/types/uri.h"
#include "framework/rcommand/commandtypes.h"

namespace au::projectrecovery {
inline static const muse::rcommand::Command PROJECT_RECOVERY_COMMAND("command://project/recovery");

inline static const muse::Uri PROJECT_RECOVERY_URI("audacity://project/recovery");
}
