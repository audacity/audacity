/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WavPackCompressor.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstdint>
#include <vector>

#include "CloudSyncUtils.h"

class AudacityProject;

namespace cloud::audiocom::sync
{
std::vector<uint8_t> CompressBlock(AudacityProject& project, BlockID blockId);
} // namespace cloud::audiocom::sync
