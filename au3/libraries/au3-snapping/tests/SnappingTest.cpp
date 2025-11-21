/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  SnappingTest.cpp

  Dmitry Vedenko

**********************************************************************/

#include <catch2/catch.hpp>

#include "SnapUtils.h"

#include "Project.h"
#include "ProjectTimeSignature.h"

#include "MockedAudio.h"
#include "MockedPrefs.h"

void TimeCase(const AudacityProject& project, const char* format, double time, double expected, bool nearest)
{
    REQUIRE(SnapFunctionsRegistry::Snap(format, project, time, nearest).snapped);
    REQUIRE(
        SnapFunctionsRegistry::Snap(format, project, time, nearest).time
        == Approx(expected));
}

void BeatsCase(
    AudacityProject& project, const char* format, double time, double expected,
    bool nearest, int upper, int lower)
{
    auto& timeSignature = ProjectTimeSignature::Get(project);

    timeSignature.SetTempo(60.0);
    timeSignature.SetUpperTimeSignature(upper);
    timeSignature.SetLowerTimeSignature(lower);

    REQUIRE(
        SnapFunctionsRegistry::Snap(format, project, time, nearest).snapped);

    REQUIRE(
        SnapFunctionsRegistry::Snap(format, project, time, nearest).time
        == Approx(expected));
}

void TimeStepCase(
    const AudacityProject& project, const char* format, double time,
    double expected, bool upwards, bool successful)
{
    REQUIRE(
        SnapFunctionsRegistry::SingleStep(format, project, time, upwards)
        .snapped == successful);

    REQUIRE(
        SnapFunctionsRegistry::SingleStep(format, project, time, upwards).time
        == Approx(expected));
}

void BarStepCase(
    AudacityProject& project, const char* format, double time,
    double expected, bool upwards)
{
    auto& timeSignature = ProjectTimeSignature::Get(project);

    timeSignature.SetTempo(60.0);
    timeSignature.SetUpperTimeSignature(4);
    timeSignature.SetLowerTimeSignature(4);

    REQUIRE(SnapFunctionsRegistry::SingleStep(format, project, time, upwards)
            .snapped);

    REQUIRE(
        SnapFunctionsRegistry::SingleStep(format, project, time, upwards).time
        == Approx(expected));
}

TEST_CASE("Snapping", "")
{
    MockedPrefs mockedPrefs;
    MockedAudio mockedAudio;

    auto project = AudacityProject::Create();

    REQUIRE(!SnapFunctionsRegistry::Snap("foo", *project, 1.0, true).snapped);

    TimeCase(*project, "seconds", 1.0, 1.0, true);
    TimeCase(*project, "seconds", 1.0, 1.0, false);
    TimeCase(*project, "seconds", 1.1, 1.0, true);
    TimeCase(*project, "seconds", 1.1, 1.0, false);
    TimeCase(*project, "seconds", 1.9, 2.0, true);
    TimeCase(*project, "seconds", 1.9, 1.0, false);

    TimeCase(*project, "deciseconds", 1.0, 1.0, true);
    TimeCase(*project, "deciseconds", 1.0, 1.0, false);
    TimeCase(*project, "deciseconds", 1.1, 1.1, true);
    TimeCase(*project, "deciseconds", 1.1, 1.1, false);
    TimeCase(*project, "deciseconds", 1.91, 1.9, true);
    TimeCase(*project, "deciseconds", 1.91, 1.9, false);
    TimeCase(*project, "deciseconds", 1.99, 2.0, true);
    TimeCase(*project, "deciseconds", 1.99, 1.9, false);

    TimeCase(*project, "centiseconds", 1.0, 1.0, true);
    TimeCase(*project, "centiseconds", 1.0, 1.0, false);
    TimeCase(*project, "centiseconds", 1.1, 1.1, true);
    TimeCase(*project, "centiseconds", 1.1, 1.1, false);
    TimeCase(*project, "centiseconds", 1.91, 1.91, true);
    TimeCase(*project, "centiseconds", 1.91, 1.91, false);
    TimeCase(*project, "centiseconds", 1.911, 1.91, true);
    TimeCase(*project, "centiseconds", 1.911, 1.91, false);
    TimeCase(*project, "centiseconds", 1.999, 2.0, true);
    TimeCase(*project, "centiseconds", 1.999, 1.99, false);

    TimeCase(*project, "milliseconds", 1.0, 1.0, true);
    TimeCase(*project, "milliseconds", 1.0, 1.0, false);
    TimeCase(*project, "milliseconds", 1.1, 1.1, true);
    TimeCase(*project, "milliseconds", 1.1, 1.1, false);
    TimeCase(*project, "milliseconds", 1.91, 1.91, true);
    TimeCase(*project, "milliseconds", 1.91, 1.91, false);
    TimeCase(*project, "milliseconds", 1.911, 1.911, true);
    TimeCase(*project, "milliseconds", 1.911, 1.911, false);
    TimeCase(*project, "milliseconds", 1.999, 1.999, true);
    TimeCase(*project, "milliseconds", 1.999, 1.999, false);
    TimeCase(*project, "milliseconds", 1.9991, 1.999, true);
    TimeCase(*project, "milliseconds", 1.9991, 1.999, false);
    TimeCase(*project, "milliseconds", 1.9996, 2.0, true);
    TimeCase(*project, "milliseconds", 1.9996, 1.999, false);

    TimeCase(*project, "samples", 1.0, 1.0, true);
    TimeCase(*project, "samples", 1.0, 1.0, false);
    TimeCase(*project, "samples", 1.0 / 44100, 1.0 / 44100, true);
    TimeCase(*project, "samples", 1.0 / 44100, 1.0 / 44100, false);
    TimeCase(*project, "samples", 1.1 / 44100, 1.0 / 44100, true);
    TimeCase(*project, "samples", 1.1 / 44100, 1.0 / 44100, false);
    TimeCase(*project, "samples", 1.6 / 44100, 2.0 / 44100, true);
    TimeCase(*project, "samples", 1.6 / 44100, 1.0 / 44100, false);

    TimeStepCase(*project, "seconds", 1.0, 2.0, true, true);
    TimeStepCase(*project, "seconds", 1.0, 0.0, false, true);
    TimeStepCase(*project, "seconds", 0.5, 0.0, false, false);
    TimeStepCase(*project, "seconds", 0.4, 1.0, true, true);
    TimeStepCase(*project, "deciseconds", 1.0, 1.1, true, true);
    TimeStepCase(*project, "deciseconds", 1.0, 0.9, false, true);
    TimeStepCase(*project, "samples", 1.0, 1.0 + 1.0 / 44100, true, true);
    TimeStepCase(*project, "samples", 1.0, 1.0 - 1.0 / 44100, false, true);

    BeatsCase(*project, "bar", 1.0, 0.0, true, 3, 4);
    BeatsCase(*project, "bar", 1.0, 0.0, false, 3, 4);
    BeatsCase(*project, "bar", 2.0, 3.0, true, 3, 4);
    BeatsCase(*project, "bar", 2.0, 0.0, false, 3, 4);

    BeatsCase(*project, "bar", 2.0, 4.0, true, 4, 4);
    BeatsCase(*project, "bar", 2.0, 0.0, false, 4, 4);
    BeatsCase(*project, "bar", 3.0, 3.0, true, 3, 4);
    BeatsCase(*project, "bar", 3.0, 3.0, false, 3, 4);

    BeatsCase(*project, "bar_1_2", 0.0, 0.0, true, 3, 4);
    BeatsCase(*project, "bar_1_2", 0.5, 0.0, true, 3, 4);
    BeatsCase(*project, "bar_1_2", 1.0, 2.0, true, 3, 4);
    BeatsCase(*project, "bar_1_2", 2.0, 2.0, true, 3, 4);
    BeatsCase(*project, "bar_1_2", 3.0, 4.0, true, 3, 4);

    BeatsCase(*project, "bar_1_2", 0.0, 0.0, true, 4, 4);
    BeatsCase(*project, "bar_1_2", 0.5, 0.0, true, 4, 4);
    BeatsCase(*project, "bar_1_2", 1.0, 2.0, true, 4, 4);
    BeatsCase(*project, "bar_1_2", 2.0, 2.0, true, 4, 4);
    BeatsCase(*project, "bar_1_2", 3.0, 4.0, true, 4, 4);

    BeatsCase(*project, "bar_1_4", 0.0, 0.0, true, 3, 4);
    BeatsCase(*project, "bar_1_4", 0.5, 1.0, true, 3, 4);
    BeatsCase(*project, "bar_1_4", 1.0, 1.0, true, 3, 4);
    BeatsCase(*project, "bar_1_4", 2.0, 2.0, true, 3, 4);
    BeatsCase(*project, "bar_1_4", 3.0, 3.0, true, 3, 4);
    BeatsCase(*project, "bar_1_4", 4.0, 4.0, true, 3, 4);

    BeatsCase(*project, "bar_1_4", 0.0, 0.0, true, 4, 4);
    BeatsCase(*project, "bar_1_4", 0.5, 1.0, true, 4, 4);
    BeatsCase(*project, "bar_1_4", 1.0, 1.0, true, 4, 4);
    BeatsCase(*project, "bar_1_4", 2.0, 2.0, true, 4, 4);
    BeatsCase(*project, "bar_1_4", 3.0, 3.0, true, 4, 4);
    BeatsCase(*project, "bar_1_4", 4.0, 4.0, true, 4, 4);

    BeatsCase(*project, "bar_1_8", 0.0, 0.0, true, 3, 4);
    BeatsCase(*project, "bar_1_8", 0.1, 0.0, true, 3, 4);
    BeatsCase(*project, "bar_1_8", 0.5, 0.5, true, 3, 4);
    BeatsCase(*project, "bar_1_8", 0.6, 0.5, true, 3, 4);
    BeatsCase(*project, "bar_1_8", 0.8, 1.0, true, 3, 4);
    BeatsCase(*project, "bar_1_8", 1.0, 1.0, true, 3, 4);
    BeatsCase(*project, "bar_1_8", 1.6, 1.5, true, 3, 4);
    BeatsCase(*project, "bar_1_8", 1.8, 2.0, true, 3, 4);

    BeatsCase(*project, "bar_1_8", 0.0, 0.0, true, 4, 4);
    BeatsCase(*project, "bar_1_8", 0.1, 0.0, true, 4, 4);
    BeatsCase(*project, "bar_1_8", 0.5, 0.5, true, 4, 4);
    BeatsCase(*project, "bar_1_8", 0.6, 0.5, true, 4, 4);
    BeatsCase(*project, "bar_1_8", 0.8, 1.0, true, 4, 4);
    BeatsCase(*project, "bar_1_8", 1.0, 1.0, true, 4, 4);
    BeatsCase(*project, "bar_1_8", 1.6, 1.5, true, 4, 4);
    BeatsCase(*project, "bar_1_8", 1.8, 2.0, true, 4, 4);

    BeatsCase(*project, "triplet_1_2", 0.0, 0.0, true, 3, 4);
    BeatsCase(*project, "triplet_1_2", 0.5, 0.0, true, 3, 4);
    BeatsCase(*project, "triplet_1_2", 1.0, 1 + 1.0 / 3.0, true, 3, 4);
    BeatsCase(*project, "triplet_1_2", 2.0, 2.0 + 2.0 / 3.0, true, 3, 4);
    BeatsCase(*project, "triplet_1_2", 3.0, 2.0 + 2.0 / 3.0, true, 3, 4);

    BeatsCase(*project, "triplet_1_2", 0.0, 0.0, true, 4, 4);
    BeatsCase(*project, "triplet_1_2", 0.5, 0.0, true, 4, 4);
    BeatsCase(*project, "triplet_1_2", 1.0, 1 + 1.0 / 3.0, true, 4, 4);
    BeatsCase(*project, "triplet_1_2", 2.0, 2.0 + 2.0 / 3.0, true, 4, 4);
    BeatsCase(*project, "triplet_1_2", 3.0, 2.0 + 2.0 / 3.0, true, 4, 4);
    BeatsCase(*project, "triplet_1_2", 4.0, 4.0, true, 4, 4);

    BeatsCase(*project, "triplet_1_4", 0.0, 0.0, true, 4, 4);
    BeatsCase(*project, "triplet_1_4", 1.0, 1 + 1.0 / 3.0, true, 4, 4);
    BeatsCase(*project, "triplet_1_4", 2.0, 2.0, true, 4, 4);
    BeatsCase(*project, "triplet_1_4", 4.0, 4.0, true, 4, 4);

    BarStepCase(*project, "bar", 0.0, 4.0, true);
    BarStepCase(*project, "bar", 4.0, 0.0, false);
    BarStepCase(*project, "bar_1_4", 0.0, 1.0, true);
    BarStepCase(*project, "bar_1_4", 4.0, 3.0, false);
}
