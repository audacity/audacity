/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  Mp3FileReader.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "AudioFileInfo.h"

#include <string>
#include <vector>

struct Mp3FileReader
{
    static bool Read(
        const std::string& path, std::vector<std::vector<float> >& audio, AudioFileInfo& info);
};
