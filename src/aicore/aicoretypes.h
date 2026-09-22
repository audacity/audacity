/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <string>

namespace au::aicore {
// Every runtime request and result will carry this protocol version. Keeping it
// here prevents provider-specific schemas from leaking into editor modules.
inline constexpr int AI_RUNTIME_PROTOCOL_VERSION = 1;

struct JobId {
    std::string value;
};
}
