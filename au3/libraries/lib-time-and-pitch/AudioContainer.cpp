/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudioContainer.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "AudioContainer.h"

AudioContainer::AudioContainer(int numSamplesPerChannel, int numChannels)
{
    for (auto i = 0; i < numChannels; ++i) {
        const auto channelData
            =channelVectors.emplace_back(numSamplesPerChannel).data();
        channelPointers.push_back(channelData);
    }
}

float* const* AudioContainer::Get() const
{
    return channelPointers.data();
}
