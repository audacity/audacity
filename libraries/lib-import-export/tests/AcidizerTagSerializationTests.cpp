/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AcidizerTagSerializationTests.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "AcidizerTagSerialization.h"
#include <catch2/catch.hpp>

namespace LibImportExport
{

using namespace LibFileFormats;

TEST_CASE("AcidizerTagSerialization")
{
   SECTION("is transparent")
   {
      const std::vector<AcidizerTags> testCases { AcidizerTags::Loop { 123. },
                                                  AcidizerTags::OneShot {} };
      for (const auto& testCase : testCases)
      {
         const auto str = AcidizerTagsToString(testCase);
         const auto tags = StringToAcidizerTags(str);
         REQUIRE(tags);
         REQUIRE(tags->bpm == testCase.bpm);
         REQUIRE(tags->isOneShot == testCase.isOneShot);
      }
   }

   SECTION("StringToAcidizerTags")
   {
      SECTION("returns null if")
      {
         SECTION("the string is invalid")
         REQUIRE(!StringToAcidizerTags("foo bar baz"));

         SECTION("some field is missing")
         REQUIRE(!StringToAcidizerTags("{\"bpm\":123.0}"));

         SECTION("some field has incorrect value type")
         REQUIRE(
            !StringToAcidizerTags("{\"bpm\":\"true\",\"isOneShot\":123.0}"));
      }

      SECTION("sets bpm to null if is one-shot")
      {
         const auto result =
            StringToAcidizerTags("{\"bpm\":123.0,\"isOneShot\":true}");
         REQUIRE(result);
         REQUIRE(!result->bpm);
      }

      SECTION("doesn't care if there are extra fields")
      {
         const auto resultWithUnexpectedField = StringToAcidizerTags(
            "{\"bpm\":123.0,\"isOneShot\":false,\"foo\":123}");
         REQUIRE(resultWithUnexpectedField);
         REQUIRE(resultWithUnexpectedField->bpm == Approx(123.));
         REQUIRE(resultWithUnexpectedField->isOneShot == false);
      }
   }
}
} // namespace LibImportExport
