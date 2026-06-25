/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  BeatsSnapFunctions.cpp

  Dmitry Vedenko

**********************************************************************/

#include "au3-numeric-formats/ProjectTimeSignature.h"

#include "SnapUtils.h"

namespace {
/*
bps = tempo / 60
1/lower takes 1/bps
*/

double SnapToBar(const AudacityProject& project)
{
    auto& timeSignature = ProjectTimeSignature::Get(project);

    const auto barDuration = timeSignature.GetBarDuration();
    const auto multiplier = 1 / barDuration;

    return multiplier;
}

MultiplierFunctor SnapToBeat(int divisor)
{
    return [divisor](const AudacityProject& project)
    {
        auto& timeSignature = ProjectTimeSignature::Get(project);

        const auto quarterDuration = timeSignature.GetQuarterDuration();
        // DV: It was decided that for the time being,
        // BPM sets the duration for quarter notes.
        // For this reason, `cfg.timeSignature.second` is ignored
        const auto fracDuration = quarterDuration * 4.0 / divisor;
        const auto multiplier = 1.0 / fracDuration;

        return multiplier;
    };
}

MultiplierFunctor SnapToTriplets(int divisor)
{
    return [divisor](const AudacityProject& project)
    {
        auto& timeSignature = ProjectTimeSignature::Get(project);

        const auto quarterDuration = 60.0 / timeSignature.GetTempo();
        const auto tripletDivisor = 3 * (divisor / 2);
        const auto fracDuration = quarterDuration * 4.0 / tripletDivisor;
        const auto multiplier = 1.0 / fracDuration;

        return multiplier;
    };
}

SnapRegistryItemRegistrator beats {
    SnapFunctionItems("beats",
                      SnapFunctionGroup(
                          /*: The music theory "beat"*/
                          "beats", { TranslatableString("snapping", "Beats"), true },
                          /*: The music theory "bar"*/
                          TimeInvariantSnapFunction("bar", TranslatableString("snapping", "Bar"), SnapToBar),
                          TimeInvariantSnapFunction("bar_1_2", TranslatableString("snapping", "1/2"), SnapToBeat(2)),
                          TimeInvariantSnapFunction("bar_1_4", TranslatableString("snapping", "1/4"), SnapToBeat(4)),
                          TimeInvariantSnapFunction("bar_1_8", TranslatableString("snapping", "1/8"), SnapToBeat(8)),
                          TimeInvariantSnapFunction("bar_1_16", TranslatableString("snapping", "1/16"), SnapToBeat(16)),
                          TimeInvariantSnapFunction("bar_1_32", TranslatableString("snapping", "1/32"), SnapToBeat(32)),
                          TimeInvariantSnapFunction("bar_1_64", TranslatableString("snapping", "1/64"), SnapToBeat(64)),
                          TimeInvariantSnapFunction("bar_1_128", TranslatableString("snapping", "1/128"), SnapToBeat(128))),
                      SnapFunctionGroup(
                          /*: The music theory "triplet"*/
                          "triplets", { TranslatableString("snapping", "Triplets"), true },
                          TimeInvariantSnapFunction(
                              "triplet_1_2", TranslatableString("snapping", "1/2 (triplets)"), SnapToTriplets(2)),
                          TimeInvariantSnapFunction(
                              "triplet_1_4", TranslatableString("snapping", "1/4 (triplets)"), SnapToTriplets(4)),
                          TimeInvariantSnapFunction(
                              "triplet_1_8", TranslatableString("snapping", "1/8 (triplets)"), SnapToTriplets(8)),
                          TimeInvariantSnapFunction(
                              "triplet_1_16", TranslatableString("snapping", "1/16 (triplets)"), SnapToTriplets(16)),
                          TimeInvariantSnapFunction(
                              "triplet_1_32", TranslatableString("snapping", "1/32 (triplets)"), SnapToTriplets(32)),
                          TimeInvariantSnapFunction(
                              "triplet_1_64", TranslatableString("snapping", "1/64 (triplets)"), SnapToTriplets(64)),
                          TimeInvariantSnapFunction(
                              "triplet_1_128", TranslatableString("snapping", "1/128 (triplets)"), SnapToTriplets(128)))
                      ),
    Registry::Placement { {}, { Registry::OrderingHint::Begin } }
};
}
