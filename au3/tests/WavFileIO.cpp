/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WavFileIO.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "WavFileIO.h"
#include "AudioFileInfo.h"

#include <cassert>
#include <iostream>
#include <sndfile.h>

using namespace std::literals::string_literals;

bool WavFileIO::Read(
    const std::string& inputPath, std::vector<std::vector<float> >& audio,
    AudioFileInfo& info, const std::optional<std::chrono::seconds>& upTo)
{
    SF_INFO sfInfo;
    auto sndfile = sf_open(inputPath.c_str(), SFM_READ, &sfInfo);
    if (!sndfile) {
        return false;
    }
    const auto numFramesToRead
        =upTo.has_value()
          ? std::min<int>(
              sfInfo.frames,
              static_cast<int>(upTo->count() * sfInfo.samplerate))
          : sfInfo.frames;
    std::vector<float> tmp(numFramesToRead * sfInfo.channels);
    const auto numReadFrames
        =sf_readf_float(sndfile, tmp.data(), numFramesToRead);
    sf_close(sndfile);
    assert(numReadFrames == numFramesToRead);
    audio.resize(sfInfo.channels);
    for (auto i = 0; i < sfInfo.channels; ++i) {
        audio[i].resize(numFramesToRead);
        for (auto ii = 0; ii < numFramesToRead; ++ii) {
            audio[i][ii] = tmp[ii * sfInfo.channels + i];
        }
    }
    info.sampleRate = sfInfo.samplerate;
    info.numChannels = sfInfo.channels;
    info.numFrames = numFramesToRead;
    return true;
}

bool WavFileIO::Write(
    const std::string& outputPath, const std::vector<std::vector<float> >& audio,
    int sampleRate)
{
    const auto numChannels = audio.size();
    const auto numFrames = audio[0].size();
    SF_INFO sfInfo;

    sfInfo.channels = numChannels;
    sfInfo.frames = numFrames;
    sfInfo.samplerate = sampleRate;
    sfInfo.format = SF_FORMAT_WAV | SF_FORMAT_PCM_16;
    sfInfo.sections = 1;
    sfInfo.seekable = 1;
    auto sndfile = sf_open(outputPath.c_str(), SFM_WRITE, &sfInfo);
    if (!sndfile) {
        std::cout << "libsndfile could not open "s + outputPath + " for write."
                  << std::endl;
        return false;
    }
    std::vector<float> interleaved(numChannels * numFrames);
    for (auto i = 0u; i < numChannels; ++i) {
        for (auto ii = 0u; ii < numFrames; ++ii) {
            interleaved[ii * numChannels + i] = audio[i][ii];
        }
    }
    const auto numFramesWritten
        =sf_writef_float(sndfile, interleaved.data(), numFrames);
    assert(numFramesWritten == numFrames);
    sf_close(sndfile);
    return true;
}
