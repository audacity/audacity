/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WavFileIO.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include <chrono>
#include <optional>
#include <string>
#include <vector>

struct WavFileIO
{
   struct Info
   {
      int sampleRate = 0;
      int numChannels = 0;
      int numFrames = 0;
   };

   static bool Read(
      const std::string& path, std::vector<std::vector<float>>&, Info&,
      const std::optional<std::chrono::seconds>& upTo = std::nullopt);

   static bool Write(
      const std::string& path, const std::vector<std::vector<float>>&,
      int sampleRate);
};
