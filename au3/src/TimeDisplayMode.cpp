/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TimeDisplayMode.cpp

  Dmitry Vedenko

**********************************************************************/
#include "TimeDisplayMode.h"

TimeDisplayModeSetting TimeDisplayModePreference {
    // Keep for the compatibility with old Audacity versions
    L"/GUI/RulerType",
    {
        { wxT("MinutesAndSeconds"), XO("Minutes and Seconds") },
        { wxT("BeatsAndMeasures"), XO("Beats and Measures") },
    },

    0, // minutes and seconds
    {
        TimeDisplayMode::MinutesAndSeconds,
        TimeDisplayMode::BeatsAndMeasures,
    }
};

bool TimeDisplayModeSetting::WriteEnum(TimeDisplayMode value)
{
    if (!EnumSetting::WriteEnum(value)) {
        return false;
    }

    Publish(value);

    return true;
}
