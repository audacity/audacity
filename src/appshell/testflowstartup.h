/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/global/modularity/ioc.h"

namespace au::appshell {
//! NOTE Swaps in a startup scenario that shows nothing waiting for the user
void installTestflowStartupScenario(const muse::modularity::ContextPtr& ctx);
}
