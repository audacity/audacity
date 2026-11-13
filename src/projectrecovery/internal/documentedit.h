/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <cstdint>
#include <set>

#include "framework/global/types/bytearray.h"
#include "framework/global/types/retval.h"

#include "binaryxml.h"

namespace au::projectrecovery {
//! Gives the doc in which each waveblock with an id in `ids` is silence of its own length: Audacity reads a blockid of
//! 0 or less as that many silent samples. Every other byte stays the same, so the dict still fits the doc.
muse::RetVal<muse::ByteArray> silenceBlocks(const BinaryXml& binary, const std::set<int64_t>& ids);
}
