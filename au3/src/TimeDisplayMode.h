/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TimeDisplayMode.cpp

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "Prefs.h"
#include "Observer.h"

#include <functional>

enum class TimeDisplayMode
{
    MinutesAndSeconds,
    BeatsAndMeasures,
};

class TimeDisplayModeSetting : public EnumSetting<TimeDisplayMode>, private Observer::Publisher<TimeDisplayMode>
{
public:
    using EnumSetting::EnumSetting;

    bool WriteEnum(TimeDisplayMode value);

    template<typename Callback>
    Observer::Subscription Subscribe(Callback callback)
    {
        return Observer::Publisher<TimeDisplayMode>::Subscribe(
            std::move(callback));
    }
};

extern TimeDisplayModeSetting TimeDisplayModePreference;
