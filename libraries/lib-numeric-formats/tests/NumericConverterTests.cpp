/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  NumericConverterTests.cpp

  Dmitry Vedenko

**********************************************************************/

#include <catch2/catch.hpp>

#include "formatters/ParsedNumericConverterFormatter.h"
#include "formatters/BeatsNumericConverterFormatter.h"

TEST_CASE("ParsedNumericConverterFormatter", "")
{
   auto hhmmssFormatter = CreateParsedNumericConverterFormatter(
      NumericConverterType_TIME, "0100 h 060 m 060 s", 44100.0);

   REQUIRE(
      hhmmssFormatter->ValueToString(0.0, false).valueString ==
      "00 h 00 m 00 s");

   REQUIRE(
      hhmmssFormatter->ValueToString(30.0, false).valueString ==
      "00 h 00 m 30 s");

   REQUIRE(
      hhmmssFormatter->ValueToString(60.0, false).valueString ==
      "00 h 01 m 00 s");

   REQUIRE(
      hhmmssFormatter->ValueToString(60.0, false)
         .fieldValueStrings.size() == 3);

   REQUIRE(
      hhmmssFormatter->ValueToString(60.0, false).fieldValueStrings[0] ==
      "00");

   REQUIRE(
      hhmmssFormatter->ValueToString(60.0, false).fieldValueStrings[1] ==
      "01");

   REQUIRE(
      hhmmssFormatter->ValueToString(60.0, false).fieldValueStrings[2] ==
      "00");

   REQUIRE(
      hhmmssFormatter->StringToValue("foobar").has_value() == false);
   REQUIRE(
      hhmmssFormatter->StringToValue("01 h 30 m 15 s").has_value() == true);
   REQUIRE(
      *hhmmssFormatter->StringToValue("01 h 30 m 15 s") ==
      Approx(60 * 60 + 30 * 60 + 15));
}

TEST_CASE("BeatsNumericConverterFormatter", "")
{
   auto basicFormatter = CreateBeatsNumericConverterFormatter(120, 3, 4);

   REQUIRE(
      basicFormatter->ValueToString(-1.0, false).valueString ==
      "--- bar -- beat");

   REQUIRE(
      basicFormatter->ValueToString(0.0, false).valueString ==
      "001 bar 01 beat");

   REQUIRE(
      basicFormatter->ValueToString(0.6, false).valueString ==
      "001 bar 02 beat");

   REQUIRE(
      basicFormatter->ValueToString(1.0, false).valueString ==
      "001 bar 03 beat");

   REQUIRE(
      basicFormatter->ValueToString(1.6, false).valueString ==
      "002 bar 01 beat");

   REQUIRE(basicFormatter->StringToValue("foobar").has_value() == false);

   REQUIRE(basicFormatter->StringToValue("001 bar 01 beat").has_value() == true);
   REQUIRE(
      *basicFormatter->StringToValue("001 bar 01 beat") == Approx(0));
   REQUIRE(
      *basicFormatter->StringToValue("001 bar 02 beat") == Approx(0.5));
   REQUIRE(
      *basicFormatter->StringToValue("002 bar 01 beat") == Approx(1.5));

   auto fracFormatter = CreateBeatsNumericConverterFormatter(120, 3, 4, 16);

   REQUIRE(
      fracFormatter->ValueToString(-1.0, false).valueString ==
      "--- bar -- beat --");

   REQUIRE(
      fracFormatter->ValueToString(0.0, false).valueString ==
      "001 bar 01 beat 01");

   REQUIRE(
      fracFormatter->ValueToString(0.6, false).valueString ==
      "001 bar 02 beat 01");

   REQUIRE(
      fracFormatter->ValueToString(1.0, false).valueString ==
      "001 bar 03 beat 01");

   REQUIRE(
      fracFormatter->ValueToString(1.9, false).valueString ==
      "002 bar 01 beat 04");
   REQUIRE(
      *fracFormatter->StringToValue("001 bar 01 beat 01") == Approx(0));
   REQUIRE(
      *fracFormatter->StringToValue("001 bar 02 beat 01") == Approx(0.5));
   REQUIRE(
      *fracFormatter->StringToValue("002 bar 01 beat 01") == Approx(1.5));
   REQUIRE(
      *fracFormatter->StringToValue("001 bar 01 beat 02") == Approx(0.125));
   REQUIRE(
      *fracFormatter->StringToValue("001 bar 02 beat 04") == Approx(0.875));
   REQUIRE(
      *fracFormatter->StringToValue("002 bar 01 beat 09") ==
      Approx(1.5 + 0.0 + 8 * (0.5 / 4)));

   REQUIRE(fracFormatter->SingleStep(0.0, 2, true) == Approx(1.5));
   REQUIRE(fracFormatter->SingleStep(0.0, 1, true) == Approx(15.0));
   REQUIRE(fracFormatter->SingleStep(0.0, 4, true) == Approx(0.5));
   REQUIRE(fracFormatter->SingleStep(0.0, 3, true) == Approx(5));
}
