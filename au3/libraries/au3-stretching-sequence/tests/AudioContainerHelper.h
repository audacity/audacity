/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudioContainerHelper.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "AudioContainer.h"

#include <cstddef>

class AudioContainerHelper
{
public:
    template<typename T = float>
    static std::vector<T*>
    GetData(const AudioContainer& container, size_t offset = 0u)
    {
        std::vector<T*> chars(container.channelPointers.size());
        for (auto i = 0u; i < container.channelPointers.size(); ++i) {
            chars[i] = reinterpret_cast<T*>(container.channelPointers[i] + offset);
        }
        return chars;
    }
};
