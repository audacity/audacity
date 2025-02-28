/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudioFileIO.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "AudioFileInfo.h"

#include <chrono>
#include <optional>
#include <string>
#include <vector>

struct AudioFileIO
{
    static bool Read(
        const std::string& path, std::vector<std::vector<float> >& audio, AudioFileInfo& info,
        const std::optional<std::chrono::seconds>& upTo = std::nullopt);
};
