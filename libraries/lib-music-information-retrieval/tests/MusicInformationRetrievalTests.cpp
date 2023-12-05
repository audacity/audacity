#include <MusicInformationRetrieval.h>
#include <catch2/catch.hpp>

namespace MIR
{
TEST_CASE("GetBpmFromFilename")
{
   const std::vector<std::pair<std::string, std::optional<double>>> testCases {
      { "120 BPM", 120 },

      // there may be an extension
      { "120 BPM.opus", 120 },
      { "120 BPM", 120 },

      // it may be preceeded by a path
      { "C:/my\\path/to\\120 BPM", 120 },

      // value must be between 30 and 300 inclusive
      { "1 BPM", std::nullopt },
      { "29 BPM", std::nullopt },
      { "30 BPM", 30 },
      { "300 BPM", 300 },
      { "301 BPM", std::nullopt },
      { "1000 BPM", std::nullopt },

      // it may be preceeded by zeros
      { "000120 BPM", 120 },

      // there may be something before the value
      { "anything 120 BPM", 120 },
      // but then there must be a separator
      { "anything120 BPM", std::nullopt },
      // there may be something after the value
      { "120 BPM anything", 120 },
      // but then there must also be a separator
      { "120 BPManything", std::nullopt },

      // what separator is used doesn't matter
      { "anything-120-BPM", 120 },
      { "anything_120_BPM", 120 },
      { "anything.120.BPM", 120 },

      // but of course that can't be an illegal filename character
      { "120/BPM", std::nullopt },
      { "120\\BPM", std::nullopt },
      { "120:BPM", std::nullopt },
      { "120;BPM", std::nullopt },
      { "120'BPM", std::nullopt },
      // ... and so on.

      // separators before and after don't have to match
      { "anything_120-BPM", 120 },

      // no separator between value and "bpm" is ok
      { "anything.120BPM", 120 },

      // a few real file names found out there
      { "Cymatics - Cyclone Top Drum Loop 3 - 174 BPM", 174 },
      { "Fantasie Impromptu Op. 66.mp3", std::nullopt },
   };
   std::vector<bool> success(testCases.size());
   std::transform(
      testCases.begin(), testCases.end(), success.begin(),
      [](const auto& testCase) {
         return GetBpmFromFilename(testCase.first) == testCase.second;
      });
   REQUIRE(
      std::all_of(success.begin(), success.end(), [](bool b) { return b; }));
}

TEST_CASE("GetProjectSyncInfo")
{
   SECTION("stretchMinimizingPowOfTwo is as expected")
   {
      MusicInformation info { "my/path\\foo_-_100BPM_Sticks_-_foo.wav", 10. };
      REQUIRE(info);
      REQUIRE(info.GetProjectSyncInfo(100).stretchMinimizingPowOfTwo == 1.);

      // Project tempo twice as fast. Without compensation, the audio would be
      // stretched to 0.5 its length. Not stretching it at all may still yield
      // musically interesting results.
      REQUIRE(info.GetProjectSyncInfo(200).stretchMinimizingPowOfTwo == 2.);

      // Same principle applies in the following:
      REQUIRE(info.GetProjectSyncInfo(400).stretchMinimizingPowOfTwo == 4.);
      REQUIRE(info.GetProjectSyncInfo(50).stretchMinimizingPowOfTwo == .5);
      REQUIRE(info.GetProjectSyncInfo(25).stretchMinimizingPowOfTwo == .25);

      // Now testing edge cases:
      REQUIRE(
         info.GetProjectSyncInfo(100 * std::pow(2, .51))
            .stretchMinimizingPowOfTwo == 2.);
      REQUIRE(
         info.GetProjectSyncInfo(100 * std::pow(2, .49))
            .stretchMinimizingPowOfTwo == 1.);
      REQUIRE(
         info.GetProjectSyncInfo(100 * std::pow(2, -.49))
            .stretchMinimizingPowOfTwo == 1.);
      REQUIRE(
         info.GetProjectSyncInfo(100 * std::pow(2, -.51))
            .stretchMinimizingPowOfTwo == .5);
   }
}
} // namespace MIR
