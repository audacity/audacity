/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  GetAcidizerTagsTests.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "AcidizerTags.h"
#include "GetAcidizerTags.h"
#include "LibsndfileTagger.h"

#include "FileFormats.h"
#include "sndfile.h"
#include <array>
#include <catch2/catch.hpp>
#include <filesystem>
#include <fstream>
#include <iostream>

namespace LibImportExport
{
using namespace LibFileFormats;

TEST_CASE("GetAcidizerTags")
{
   SECTION("returns null if there is no loop info")
   {
      Test::LibsndfileTagger tagger;
      const auto actual = GetAcidizerTags(tagger.ReopenInReadMode(), {});
      REQUIRE(!actual.has_value());
   }

   SECTION("returns null if the distributor isn't whitelisted")
   {
      Test::LibsndfileTagger tagger;
      tagger.AddAcidizerTags(AcidizerTags::Loop { 120. });
      tagger.AddDistributorInfo("Distributor Zen");
      const auto actual = GetAcidizerTags(
         tagger.ReopenInReadMode(),
         { "foo", "Distributor Z", "Distributor Zen 2" });
      REQUIRE(!actual.has_value());
   }

   SECTION(
      "returns valid info if there is loop info and the distributor is whitelisted")
   {
      std::vector<AcidizerTags> expected {
         AcidizerTags::Loop { 120. },
         AcidizerTags::OneShot {},
      };
      for (const auto& tags : expected)
      {
         Test::LibsndfileTagger tagger;
         tagger.AddAcidizerTags(tags);
         tagger.AddDistributorInfo("Trusted Distributor");
         const auto actual = GetAcidizerTags(
            tagger.ReopenInReadMode(), { "Trusted Distributor" });
         REQUIRE(actual.has_value());
         REQUIRE(actual->bpm == tags.bpm);
         REQUIRE(actual->isOneShot == tags.isOneShot);
      }
   }
}
} // namespace LibImportExport
