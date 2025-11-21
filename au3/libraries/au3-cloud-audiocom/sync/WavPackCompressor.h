/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WavPackCompressor.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstdint>
#include <limits>
#include <optional>
#include <vector>

#include "CloudSyncDTO.h"

class AudacityProject;

namespace audacity::cloud::audiocom::sync {
std::vector<uint8_t> CompressBlock(const LockedBlock& block);

struct MinMaxRMS final
{
    float Min { std::numeric_limits<float>::max() };
    float Max { -std::numeric_limits<float>::max() };
    float RMS { 0.0 };
};

struct DecompressedBlock final
{
    int64_t BlockId;
    sampleFormat Format;

    MinMaxRMS BlockMinMaxRMS;
    std::vector<MinMaxRMS> Summary256;
    std::vector<MinMaxRMS> Summary64k;
    std::vector<uint8_t> Data;
}; // struct DecompressedBlock

std::optional<DecompressedBlock> DecompressBlock(
    const void* data, const std::size_t size);
} // namespace audacity::cloud::audiocom::sync
