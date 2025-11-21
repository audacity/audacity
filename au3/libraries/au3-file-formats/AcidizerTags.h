/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AcidizerTags.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include <cassert>
#include <optional>

namespace LibFileFormats {
/*!
 * @brief Models how SoundForge allows the editing of ACID metadata, as far as
 * our interest goes: setting the type to one-shot prevents the setting of BPM.
 * Conversely, loops must have their BPM set (normally by setting the number of
 * beats in the loop, but provided that the file duration is available, this is
 * equivalent).
 */
struct AcidizerTags
{
    struct OneShot
    {
    };

    struct Loop
    {
        explicit Loop(double bpm)
            : bpm{bpm}
        {
        }

        const double bpm;
    };

    AcidizerTags()
    {
    }

    AcidizerTags(OneShot)
        : isOneShot{true}
    {
    }

    AcidizerTags(Loop loop)
        : bpm{loop.bpm}
    {
    }

    const std::optional<double> bpm;
    const bool isOneShot = false;
};
} // namespace LibFileFormats
