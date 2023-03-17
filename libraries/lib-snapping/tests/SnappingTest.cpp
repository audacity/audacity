/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  SnappingTest.cpp

  Dmitry Vedenko

**********************************************************************/

#include <catch2/catch.hpp>

#include "SnapUtils.h"

void TimeCase(const char* format, double time, double expected, bool nearest)
{
   REQUIRE(
      SnapFunctionsRegistry::Snap(format, { 44100.0 }, time, nearest).snapped);
   REQUIRE(
      SnapFunctionsRegistry::Snap(format, { 44100.0 }, time, nearest).time ==
      Approx(expected));
}

void BeatsCase(const char* format, double time, double expected, bool nearest, int upper, int lower)
{
   REQUIRE(SnapFunctionsRegistry::Snap(
              format, { 44100.0, 60.0, { upper, lower } }, time, nearest)
              .snapped);
   
   REQUIRE(
      SnapFunctionsRegistry::Snap(
         format, { 44100.0, 60.0, { upper, lower } }, time, nearest)
         .time == Approx(expected));
}

TEST_CASE("Snapping", "")
{
   REQUIRE(
      !SnapFunctionsRegistry::Snap("foo", { 44100.0 }, 1.0, true).snapped);
   
   TimeCase("seconds", 1.0, 1.0, true);
   TimeCase("seconds", 1.0, 1.0, false);
   TimeCase("seconds", 1.1, 1.0, true);
   TimeCase("seconds", 1.1, 1.0, false);
   TimeCase("seconds", 1.9, 2.0, true);
   TimeCase("seconds", 1.9, 1.0, false);

   TimeCase("deciseconds", 1.0, 1.0, true);
   TimeCase("deciseconds", 1.0, 1.0, false);
   TimeCase("deciseconds", 1.1, 1.1, true);
   TimeCase("deciseconds", 1.1, 1.1, false);
   TimeCase("deciseconds", 1.91, 1.9, true);
   TimeCase("deciseconds", 1.91, 1.9, false);
   TimeCase("deciseconds", 1.99, 2.0, true);
   TimeCase("deciseconds", 1.99, 1.9, false);

   TimeCase("centiseconds", 1.0, 1.0, true);
   TimeCase("centiseconds", 1.0, 1.0, false);
   TimeCase("centiseconds", 1.1, 1.1, true);
   TimeCase("centiseconds", 1.1, 1.1, false);
   TimeCase("centiseconds", 1.91, 1.91, true);
   TimeCase("centiseconds", 1.91, 1.91, false);
   TimeCase("centiseconds", 1.911, 1.91, true);
   TimeCase("centiseconds", 1.911, 1.91, false);
   TimeCase("centiseconds", 1.999, 2.0, true);
   TimeCase("centiseconds", 1.999, 1.99, false);

   TimeCase("milliseconds", 1.0, 1.0, true);
   TimeCase("milliseconds", 1.0, 1.0, false);
   TimeCase("milliseconds", 1.1, 1.1, true);
   TimeCase("milliseconds", 1.1, 1.1, false);
   TimeCase("milliseconds", 1.91, 1.91, true);
   TimeCase("milliseconds", 1.91, 1.91, false);
   TimeCase("milliseconds", 1.911, 1.911, true);
   TimeCase("milliseconds", 1.911, 1.911, false);
   TimeCase("milliseconds", 1.999, 1.999, true);
   TimeCase("milliseconds", 1.999, 1.999, false);
   TimeCase("milliseconds", 1.9991, 1.999, true);
   TimeCase("milliseconds", 1.9991, 1.999, false);
   TimeCase("milliseconds", 1.9996, 2.0, true);
   TimeCase("milliseconds", 1.9996, 1.999, false);

   TimeCase("samples", 1.0, 1.0, true);
   TimeCase("samples", 1.0, 1.0, false);
   TimeCase("samples", 1.0 / 44100, 1.0 / 44100, true);
   TimeCase("samples", 1.0 / 44100, 1.0 / 44100, false);
   TimeCase("samples", 1.1 / 44100, 1.0 / 44100, true);
   TimeCase("samples", 1.1 / 44100, 1.0 / 44100, false);
   TimeCase("samples", 1.6 / 44100, 2.0 / 44100, true);
   TimeCase("samples", 1.6 / 44100, 1.0 / 44100, false);

   BeatsCase("bar", 1.0, 0.0, true, 3, 4);
   BeatsCase("bar", 1.0, 0.0, false, 3, 4);
   BeatsCase("bar", 2.0, 3.0, true, 3, 4);
   BeatsCase("bar", 2.0, 0.0, false, 3, 4);
   
   BeatsCase("bar", 2.0, 4.0, true, 4, 4);
   BeatsCase("bar", 2.0, 0.0, false, 4, 4);
   BeatsCase("bar", 3.0, 3.0, true, 3, 4);
   BeatsCase("bar", 3.0, 3.0, false, 3, 4);

   BeatsCase("bar_1_2", 0.0, 0.0, true, 3, 4);
   BeatsCase("bar_1_2", 0.5, 0.0, true, 3, 4);
   BeatsCase("bar_1_2", 1.0, 2.0, true, 3, 4);
   BeatsCase("bar_1_2", 2.0, 2.0, true, 3, 4);
   BeatsCase("bar_1_2", 3.0, 4.0, true, 3, 4);

   BeatsCase("bar_1_2", 0.0, 0.0, true, 4, 4);
   BeatsCase("bar_1_2", 0.5, 0.0, true, 4, 4);
   BeatsCase("bar_1_2", 1.0, 2.0, true, 4, 4);
   BeatsCase("bar_1_2", 2.0, 2.0, true, 4, 4);
   BeatsCase("bar_1_2", 3.0, 4.0, true, 4, 4);

   BeatsCase("bar_1_4", 0.0, 0.0, true, 3, 4);
   BeatsCase("bar_1_4", 0.5, 1.0, true, 3, 4);
   BeatsCase("bar_1_4", 1.0, 1.0, true, 3, 4);
   BeatsCase("bar_1_4", 2.0, 2.0, true, 3, 4);
   BeatsCase("bar_1_4", 3.0, 3.0, true, 3, 4);
   BeatsCase("bar_1_4", 4.0, 4.0, true, 3, 4);

   BeatsCase("bar_1_4", 0.0, 0.0, true, 4, 4);
   BeatsCase("bar_1_4", 0.5, 1.0, true, 4, 4);
   BeatsCase("bar_1_4", 1.0, 1.0, true, 4, 4);
   BeatsCase("bar_1_4", 2.0, 2.0, true, 4, 4);
   BeatsCase("bar_1_4", 3.0, 3.0, true, 4, 4);
   BeatsCase("bar_1_4", 4.0, 4.0, true, 4, 4);

   BeatsCase("bar_1_8", 0.0, 0.0, true, 3, 4);
   BeatsCase("bar_1_8", 0.1, 0.0, true, 3, 4);
   BeatsCase("bar_1_8", 0.5, 0.5, true, 3, 4);
   BeatsCase("bar_1_8", 0.6, 0.5, true, 3, 4);
   BeatsCase("bar_1_8", 0.8, 1.0, true, 3, 4);
   BeatsCase("bar_1_8", 1.0, 1.0, true, 3, 4);
   BeatsCase("bar_1_8", 1.6, 1.5, true, 3, 4);
   BeatsCase("bar_1_8", 1.8, 2.0, true, 3, 4);

   BeatsCase("bar_1_8", 0.0, 0.0, true, 4, 4);
   BeatsCase("bar_1_8", 0.1, 0.0, true, 4, 4);
   BeatsCase("bar_1_8", 0.5, 0.5, true, 4, 4);
   BeatsCase("bar_1_8", 0.6, 0.5, true, 4, 4);
   BeatsCase("bar_1_8", 0.8, 1.0, true, 4, 4);
   BeatsCase("bar_1_8", 1.0, 1.0, true, 4, 4);
   BeatsCase("bar_1_8", 1.6, 1.5, true, 4, 4);
   BeatsCase("bar_1_8", 1.8, 2.0, true, 4, 4);

   BeatsCase("triplet_1_2", 0.0, 0.0, true, 3, 4);
   BeatsCase("triplet_1_2", 0.5, 0.0, true, 3, 4);
   BeatsCase("triplet_1_2", 1.0, 1 + 1.0 / 3.0, true, 3, 4);
   BeatsCase("triplet_1_2", 2.0, 2.0 + 2.0 / 3.0, true, 3, 4);
   BeatsCase("triplet_1_2", 3.0, 2.0 + 2.0 / 3.0, true, 3, 4);
   
   BeatsCase("triplet_1_2", 0.0, 0.0, true, 4, 4);
   BeatsCase("triplet_1_2", 0.5, 0.0, true, 4, 4);
   BeatsCase("triplet_1_2", 1.0, 1 + 1.0 / 3.0, true, 4, 4);
   BeatsCase("triplet_1_2", 2.0, 2.0 + 2.0 / 3.0, true, 4, 4);
   BeatsCase("triplet_1_2", 3.0, 2.0 + 2.0 / 3.0, true, 4, 4);
   BeatsCase("triplet_1_2", 4.0, 4.0, true, 4, 4);

   BeatsCase("triplet_1_4", 0.0, 0.0, true, 4, 4);
   BeatsCase("triplet_1_4", 1.0, 1 + 1.0 / 3.0, true, 4, 4);
   BeatsCase("triplet_1_4", 2.0, 2.0, true, 4, 4);
   BeatsCase("triplet_1_4", 4.0, 4.0, true, 4, 4);
}
