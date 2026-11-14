/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  Mp3FileReader.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "Mp3FileReader.h"
#include "AudioFileInfo.h"

#include <mpg123.h>

bool Mp3FileReader::Read(
    const std::string& path, std::vector<std::vector<float> >& floats,
    AudioFileInfo& info)
{
    int error = MPG123_OK;
    mpg123_handle* handle = mpg123_new(nullptr, &error);
    if (handle == nullptr) {
        return false;
    }

    if (mpg123_open(handle, path.c_str()) != MPG123_OK) {
        return false;
    }

    mpg123_param(handle, MPG123_FLAGS, MPG123_GAPLESS | MPG123_FORCE_FLOAT, 0.0);
    if (mpg123_scan(handle) != MPG123_OK) {
        return false;
    }

    if (
        mpg123_decode_frame(handle, nullptr, nullptr, nullptr)
        != MPG123_NEW_FORMAT) {
        return false;
    }

    long long framesCount = mpg123_framelength(handle);
    long rate;
    int channels;
    int encoding = MPG123_ENC_FLOAT_32;
    mpg123_getformat(handle, &rate, &channels, &encoding);
    const auto numChannels = channels == MPG123_MONO ? 1 : 2;
    if (encoding != MPG123_ENC_FLOAT_32) {
        return false;
    }

    floats.resize(numChannels);
    off_t frameIndex { 0 };
    unsigned char* data { nullptr };
    size_t dataSize { 0 };
    std::vector<float> conversionBuffer;
    int ret = MPG123_OK;
    while ((ret = mpg123_decode_frame(handle, &frameIndex, &data, &dataSize))
           == MPG123_OK) {
        for (auto channelIndex = 0; channelIndex < numChannels; ++channelIndex) {
            for (auto frameIndex = 0;
                 frameIndex < dataSize / numChannels / sizeof(float); ++frameIndex) {
                const auto floatIndex = channelIndex + frameIndex * numChannels;
                const auto value
                    =*reinterpret_cast<float*>(data + floatIndex * sizeof(float));
                floats[channelIndex].push_back(value);
            }
        }
    }

    mpg123_close(handle);
    mpg123_delete(handle);
    mpg123_exit();

    info.sampleRate = rate;
    info.numChannels = numChannels;
    info.numFrames = floats[0].size();

    return true;
}
