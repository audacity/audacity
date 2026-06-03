/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TimeSnapFunctions.cpp

  Dmitry Vedenko

**********************************************************************/

#include "SnapUtils.h"
#include "au3-project-rate/ProjectRate.h"

namespace {
double SnapToSamples(const AudacityProject& project)
{
    return ProjectRate::Get(project).GetRate();
}

SnapRegistryItemRegistrator secondsAndSamples {
    SnapFunctionItems("time",
                      SnapFunctionGroup(
                          "time", { TranslatableString("snapping", "Seconds && samples"), false },
                          TimeInvariantSnapFunction("seconds", TranslatableString("snapping", "Seconds"), 1.0),
                          TimeInvariantSnapFunction("deciseconds", TranslatableString("snapping", "Deciseconds"), 10.0),
                          TimeInvariantSnapFunction("centiseconds", TranslatableString("snapping", "Centiseconds"), 100.0),
                          TimeInvariantSnapFunction("milliseconds", TranslatableString("snapping", "Milliseconds"), 1000.0),
                          TimeInvariantSnapFunction("samples", TranslatableString("snapping", "Samples"), SnapToSamples))
                      ),
    Registry::Placement { {}, { Registry::OrderingHint::After, "beats" } }
};
}
