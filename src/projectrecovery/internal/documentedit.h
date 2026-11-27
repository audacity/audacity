/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <cstdint>
#include <set>

#include "framework/global/types/ret.h"

#include "binaryxml.h"

namespace au::projectrecovery {
//! Makes each block in ids a silent block in the given document
muse::Ret silenceBlocks(XmlDocument& document, const std::set<int64_t>& ids);
}
