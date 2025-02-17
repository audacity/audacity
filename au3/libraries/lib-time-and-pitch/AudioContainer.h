/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudioContainer.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include <vector>

struct TIME_AND_PITCH_API AudioContainer final
{
    AudioContainer(int numSamplesPerChannel, int numChannels);
    float* const* Get() const;
    std::vector<std::vector<float> > channelVectors;
    std::vector<float*> channelPointers;
};
