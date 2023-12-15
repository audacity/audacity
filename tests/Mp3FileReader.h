/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WavFileIO.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include <string>
#include <vector>

struct Mp3FileReader
{
   struct Info
   {
      int sampleRate = 0;
      int numChannels = 0;
      int numFrames = 0;
   };

   static bool Read(
      const std::string& path, std::vector<std::vector<float>>& audio,
      Info& info);
};
