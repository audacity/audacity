/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/global/modularity/ioc.h"

namespace au::appshell {
//! NOTE Swaps in a startup scenario that shows nothing waiting for the user,
//! except, if `allowRecovery`, the recovery of projects left open by a previous run
void installTestflowStartupScenario(const muse::modularity::ContextPtr& ctx, bool allowRecovery);
}
