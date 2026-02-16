/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WavPackCompressor.h

  Dmitry Vedenko
  Generalized for project storage

**********************************************************************/
#pragma once

#include <cstdint>
#include <optional>
#include <vector>

#include "au3-math/SampleFormat.h"

namespace audacity::project {

struct DecompressedBlock final
{
    sampleFormat format;
    size_t sampleCount;
    std::vector<uint8_t> data;
};

std::vector<uint8_t> WavPackCompress(
    const void* src, size_t numSamples, sampleFormat format,
    long long blockId = 0);

std::optional<DecompressedBlock> WavPackDecompress(
    const void* src, size_t srcSize);

} // namespace audacity::project
