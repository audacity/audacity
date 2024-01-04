/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  GetAcidizerTagsTests.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "AcidizerTags.h"
#include "EditRiffTags.h"
#include "GetAcidizerTags.h"
#include "MemoryX.h"

#include "FileFormats.h"
#include "sndfile.h"
#include <array>
#include <catch2/catch.hpp>
#include <filesystem>
#include <fstream>
#include <iostream>

namespace LibImportExport
{
namespace
{

void WriteAndGetTmpFile(
   SNDFILE*& file, const std::optional<LibFileFormats::AcidizerTags>& tags,
   const std::optional<std::string>& distributor)
{
   const std::string filename = tmpnam(nullptr);
   // Write an empty file using libsndfile:
   SF_INFO sfInfo;
   sfInfo.samplerate = 44100;
   sfInfo.channels = 1;
   sfInfo.format = SF_FORMAT_WAV | SF_FORMAT_PCM_16;
   file = sf_open(filename.c_str(), SFM_WRITE, &sfInfo);
   REQUIRE(file != nullptr);
   const std::vector<short> zero(sfInfo.frames, 0);
   REQUIRE(sf_write_short(file, zero.data(), sfInfo.frames) == sfInfo.frames);

   Finally Do = [&] {
      // Close the file and reopen it for reading, re-using the same variable
      // for further use:
      const auto result = sf_close(file);
      REQUIRE(result == SF_ERR_NO_ERROR);
      file = sf_open(filename.c_str(), SFM_READ, &sfInfo);
      if (file == nullptr)
         std::cout << "Error opening " << filename << ": " << sf_strerror(file)
                   << std::endl;
      REQUIRE(file != nullptr);
   };

   if (tags.has_value())
      AddAcidizerTags(*tags, file);

   if (distributor.has_value())
      AddDistributorInfo(*distributor, file);
}
} // namespace

TEST_CASE("GetAcidizerTags")
{
   SECTION("returns null if there is no loop info")
   {
      SNDFILE* file;
      WriteAndGetTmpFile(file, std::nullopt, std::nullopt);
      const auto actual = GetAcidizerTags(*file, {});
      REQUIRE(!actual.has_value());
      sf_close(file);
   }

   SECTION("returns null if there distributor isn't whitelisted")
   {
      SNDFILE* file;
      WriteAndGetTmpFile(
         file, LibFileFormats::AcidizerTags { 120, false },
         { "Distributor Zen" });
      const auto actual = GetAcidizerTags(
         *file, { "foo", "Distributor Z", "Distributor Zen 2" });
      REQUIRE(!actual.has_value());
      sf_close(file);
   }

   SECTION(
      "returns valid info if there is loop info and the distributor is whitelisted")
   {
      std::vector<LibFileFormats::AcidizerTags> expected {
         { 120, false },
         { 120, true },
      };
      for (const auto& info : expected)
      {
         SNDFILE* file;
         WriteAndGetTmpFile(file, info, { "Trusted Distributor" });
         const auto actual = GetAcidizerTags(*file, { "Trusted Distributor" });
         REQUIRE(actual.has_value());
         REQUIRE(actual->bpm == info.bpm);
         REQUIRE(actual->isOneShot == info.isOneShot);
         sf_close(file);
      }
   }
}
} // namespace LibImportExport
