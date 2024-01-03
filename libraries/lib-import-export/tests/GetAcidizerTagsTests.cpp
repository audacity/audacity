/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  GetAcidizerTagsTests.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "AcidizerTags.h"
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
// Unfortunately, libsndfile has support for neither writing loop info (although
// it does have support for reading it) nor writing LIST chunks.

void AddMusicInfo(const LibFileFormats::AcidizerTags& acidTags, SNDFILE* file)
{
   // Adapted from the ACID chunk readout code in libsndfile and its comment:
   // clang-format off
   /*
   ** The acid chunk goes a little something like this:
   **
   ** 4 bytes          'acid'
   ** 4 bytes (int)     length of chunk starting at next byte
   **
   ** 4 bytes (int)     type of file:
   **        this appears to be a bit mask,however some combinations
   **        are probably impossible and/or qualified as "errors"
   **
   **        0x01 On: One Shot         Off: Loop
   **        0x02 On: Root note is Set Off: No root
   **        0x04 On: Stretch is On,   Off: Strech is OFF
   **        0x08 On: Disk Based       Off: Ram based
   **        0x10 On: ??????????       Off: ????????? (Acidizer puts that ON)
   **
   ** 2 bytes (short)      root note
   **        if type 0x10 is OFF : [C,C#,(...),B] -> [0x30 to 0x3B]
   **        if type 0x10 is ON  : [C,C#,(...),B] -> [0x3C to 0x47]
   **         (both types fit on same MIDI pitch albeit different octaves, so who cares)
   **
   ** 2 bytes (short)      ??? always set to 0x8000
   ** 4 bytes (float)      ??? seems to be always 0
   ** 4 bytes (int)        number of beats
   ** 2 bytes (short)      meter denominator   //always 4 in SF/ACID
   ** 2 bytes (short)      meter numerator     //always 4 in SF/ACID
   **                      //are we sure about the order?? usually its num/denom
   ** 4 bytes (float)      tempo
   **
   */
   // clang-format on

   SF_LOOP_INFO loopInfo {};
   loopInfo.bpm = acidTags.bpm;
   loopInfo.loop_mode = acidTags.isOneShot ? SF_LOOP_NONE : SF_LOOP_FORWARD;

   SF_CHUNK_INFO chunk;
   memset(&chunk, 0, sizeof(chunk));
   snprintf(chunk.id, sizeof(chunk.id), "acid");
   chunk.id_size = 4;
   // All sizes listed above except the first two:
   chunk.datalen = 4 + 2 + 2 + 4 + 4 + 2 + 2 + 4;
   const auto dataUPtr = std::make_unique<char[]>(chunk.datalen);
   std::fill(dataUPtr.get(), dataUPtr.get() + chunk.datalen, 0);
   chunk.data = dataUPtr.get();

   // Ignore everything for now except loop mode and tempo:
   int32_t type = 0;
   if (acidTags.isOneShot)
      type |= 0x01;
   std::copy(&type, &type + sizeof(type), dataUPtr.get());
   const float tempo = acidTags.bpm;
   memcpy(((char*)chunk.data) + chunk.datalen - 4, &tempo, sizeof(tempo));

   const auto result = sf_set_chunk(file, &chunk);
   REQUIRE(result == SF_ERR_NO_ERROR);
}

void AddDistributorInfo(const std::string& distributor, SNDFILE* file)
{
   const int32_t distributorSize = distributor.size();
   SF_CHUNK_INFO chunk;
   snprintf(chunk.id, sizeof(chunk.id), "LIST");
   chunk.id_size = 4;
   constexpr std::array<char, 4> listTypeID = { 'I', 'N', 'F', 'O' };
   constexpr std::array<char, 4> distributorTypeID = { 'I', 'D', 'S', 'T' };
   chunk.datalen = sizeof(listTypeID) + sizeof(distributorTypeID) +
                   sizeof(distributorSize) + distributorSize;
   // A trick taken from libsndfile's source code, probably to ensure that
   // the rest of the data stays word-aligned:
   while (chunk.datalen & 3)
      ++chunk.datalen;
   const auto dataUPtr = std::make_unique<char[]>(chunk.datalen);
   chunk.data = dataUPtr.get();
   memset(chunk.data, 0, chunk.datalen);
   char* chars = reinterpret_cast<char*>(chunk.data);
   auto pos = 0;

   memcpy(chars, listTypeID.data(), sizeof(listTypeID));

   pos += sizeof(listTypeID);
   memcpy(chars + pos, distributorTypeID.data(), sizeof(distributorTypeID));

   pos += sizeof(distributorTypeID);
   memcpy(chars + pos, &distributorSize, sizeof(distributorSize));

   pos += sizeof(distributorSize);
   memcpy(chars + pos, distributor.c_str(), distributor.size());

   const auto result = sf_set_chunk(file, &chunk);
   REQUIRE(result == SF_ERR_NO_ERROR);
}

void WriteAndGetTmpFile(
   SNDFILE*& file, const std::optional<LibFileFormats::AcidizerTags>& info,
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

   if (info.has_value())
      AddMusicInfo(*info, file);

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
