/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "aicore/aicoretypes.h"

namespace au::aijobs {
enum class RuntimeStatus {
    Unavailable,
    Healthy,
};

class IRuntimeClient
{
public:
    virtual ~IRuntimeClient() = default;
    virtual RuntimeStatus status() const = 0;
    virtual int protocolVersion() const = 0;
};
}
