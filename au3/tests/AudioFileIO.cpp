/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudioFileIO.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "AudioFileIO.h"
#include "Mp3FileReader.h"
#include "WavFileIO.h"

bool AudioFileIO::Read(
    const std::string& path, std::vector<std::vector<float> >& audio,
    AudioFileInfo& info, const std::optional<std::chrono::seconds>& upTo)
{
    return WavFileIO::Read(path, audio, info, upTo)
           || Mp3FileReader::Read(path, audio, info);
}
