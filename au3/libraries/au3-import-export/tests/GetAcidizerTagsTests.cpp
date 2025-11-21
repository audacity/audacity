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

namespace LibImportExport {
using namespace LibFileFormats;

TEST_CASE("GetAcidizerTags")
{
    SECTION("returns null if")
    {
        SECTION("there is no loop info")
        {
            Test::LibsndfileTagger tagger;
            const auto actual = GetAcidizerTags(tagger.ReopenInReadMode(), {});
            REQUIRE(!actual.has_value());
        }
        SECTION("tempo is set but the distributor isn't whitelisted")
        {
            Test::LibsndfileTagger tagger;
            tagger.AddAcidizerTags(AcidizerTags::Loop { 120. });
            tagger.AddDistributorInfo("Distributor Zen");
            const auto actual = GetAcidizerTags(
                tagger.ReopenInReadMode(),
                { "foo", "Distributor Z", "Distributor Zen 2" });
            REQUIRE(!actual.has_value());
        }
    }

    SECTION("returns valid info if")
    {
        SECTION("OneShot is set")
        {
            Test::LibsndfileTagger tagger;
            tagger.AddAcidizerTags(AcidizerTags::OneShot {});
            const auto actual = GetAcidizerTags(tagger.ReopenInReadMode(), {});
            REQUIRE(actual.has_value());
            REQUIRE(actual->isOneShot);
        }
        SECTION("Beats is set")
        {
            // 20 beats in 10 seconds -> 120 bpm.
            Test::LibsndfileTagger tagger { 10. };
            constexpr auto numBeats = 20;
            tagger.AddAcidizerTags(Test::AcidizerTags::Beats { numBeats });
            auto& file = tagger.ReopenInReadMode();
            const auto actual = GetAcidizerTags(file, {});
            REQUIRE(actual.has_value());
            SF_INFO info;
            sf_command(&file, SFC_GET_CURRENT_SF_INFO, &info, sizeof(SF_INFO));
            const auto durationAfterClose = 1. * info.frames / info.samplerate;
            const auto expectedBpm = numBeats * 60 / durationAfterClose;
            REQUIRE(actual->bpm == Approx(expectedBpm));
            REQUIRE(actual->isOneShot == false);
        };
        SECTION("Tempo is set and the distributor is whitelisted")
        {
            Test::LibsndfileTagger tagger;
            tagger.AddAcidizerTags(AcidizerTags::Loop { 120. });
            tagger.AddDistributorInfo("Trusted Distributor");
            const auto actual = GetAcidizerTags(
                tagger.ReopenInReadMode(), { "Trusted Distributor" });
            REQUIRE(actual.has_value());
            REQUIRE(actual->bpm == 120.);
            REQUIRE(actual->isOneShot == false);
        }
    }
}
} // namespace LibImportExport
