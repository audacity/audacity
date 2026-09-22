/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "iruntimeclient.h"

namespace au::aijobs {
// Gate 1 placeholder. Gate 2 replaces this with an authenticated loopback
// client; keeping the same interface makes UI and controller tests deterministic.
class FakeRuntimeClient final : public IRuntimeClient
{
public:
    RuntimeStatus status() const override;
    int protocolVersion() const override;
};
}
