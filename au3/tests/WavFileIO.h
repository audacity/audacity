/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WavFileIO.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "AudioFileInfo.h"

#include <chrono>
#include <optional>
#include <string>
#include <vector>

struct WavFileIO
{
    static bool Read(
        const std::string& path, std::vector<std::vector<float> >&, AudioFileInfo&,
        const std::optional<std::chrono::seconds>& upTo = std::nullopt);

    static bool Write(
        const std::string& path, const std::vector<std::vector<float> >&, int sampleRate);
};
