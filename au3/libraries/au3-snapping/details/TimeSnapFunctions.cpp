/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TimeSnapFunctions.cpp

  Dmitry Vedenko

**********************************************************************/

#include "SnapUtils.h"
#include "ProjectRate.h"

namespace {
double SnapToSamples(const AudacityProject& project)
{
    return ProjectRate::Get(project).GetRate();
}

SnapRegistryItemRegistrator secondsAndSamples {
    SnapFunctionItems("time",
                      SnapFunctionGroup(
                          "time", { XO("Seconds && samples"), false },
                          TimeInvariantSnapFunction("seconds", XO("Seconds"), 1.0),
                          TimeInvariantSnapFunction("deciseconds", XO("Deciseconds"), 10.0),
                          TimeInvariantSnapFunction("centiseconds", XO("Centiseconds"), 100.0),
                          TimeInvariantSnapFunction("milliseconds", XO("Milliseconds"), 1000.0),
                          TimeInvariantSnapFunction("samples", XO("Samples"), SnapToSamples))
                      ),
    Registry::Placement { {}, { Registry::OrderingHint::After, "beats" } }
};
}
