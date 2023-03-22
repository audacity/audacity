/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  NumericConverterTests.cpp

  Dmitry Vedenko

**********************************************************************/

#include <catch2/catch.hpp>

#include "formatters/ParsedNumericConverterFormatter.h"

TEST_CASE("ParsedNumericConverterFormatter", "")
{
   auto hhmmssFormatter = CreateParsedNumericConverterFormatter(
      NumericConverterType::TIME, "0100 h 060 m 060 s", 44100.0);

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
