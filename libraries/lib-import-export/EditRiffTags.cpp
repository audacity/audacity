#include "EditRiffTags.h"
#include "AcidizerTags.h"

#include "sndfile.h"
#include <array>
#include <cassert>
#include <cstdio>
#include <cstring>
#include <memory>

namespace LibImportExport
{
void AddAcidizerTags(
   const LibFileFormats::AcidizerTags& acidTags, SNDFILE* file)
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
   std::memset(&chunk, 0, sizeof(chunk));
   std::snprintf(chunk.id, sizeof(chunk.id), "acid");
   chunk.id_size = 4;
   // All sizes listed above except the first two:
   chunk.datalen = 4 + 2 + 2 + 4 + 4 + 2 + 2 + 4;
   const auto dataUPtr = std::make_unique<char[]>(chunk.datalen);
   chunk.data = dataUPtr.get();

   // The type has 4 bytes, of which we may only set the 1st bit to 1 if the
   // loop is one-shot:
   auto type = reinterpret_cast<uint32_t*>(dataUPtr.get());
   if (acidTags.isOneShot)
      *type |= 0x00000001;

   // Set the meter denominator 2 bytes to 4:
   auto numerator = reinterpret_cast<uint16_t*>(dataUPtr.get() + 16);
   *numerator |= 0x0004;
   auto denominator = reinterpret_cast<uint16_t*>(dataUPtr.get() + 18);
   *denominator |= 0x0004;

   auto tempo = reinterpret_cast<float*>(dataUPtr.get() + 20);
   *tempo = acidTags.bpm;

   const auto result = sf_set_chunk(file, &chunk);
   assert(result == SF_ERR_NO_ERROR);
}

void AddDistributorInfo(const std::string& distributor, SNDFILE* file)
{
   const int32_t distributorSize = distributor.size();
   SF_CHUNK_INFO chunk;
   std::snprintf(chunk.id, sizeof(chunk.id), "LIST");
   chunk.id_size = 4;
   constexpr std::array<char, 4> listTypeID = { 'I', 'N', 'F', 'O' };
   constexpr std::array<char, 4> distributorTypeID = { 'I', 'D', 'S', 'T' };
   chunk.datalen = sizeof(listTypeID) + sizeof(distributorTypeID) +
                   sizeof(distributorSize) + distributorSize;
   // A trick taken from libsndfile's source code, probably to ensure that
   // the rest of the data stays word-aligned:
   while (chunk.datalen & 3)
      ++chunk.datalen;
   const auto dataUPtr = std::make_unique<uint8_t[]>(chunk.datalen);
   chunk.data = dataUPtr.get();
   auto data = dataUPtr.get();
   std::memset(chunk.data, 0, chunk.datalen);
   auto pos = 0;

   std::memcpy(data + pos, listTypeID.data(), sizeof(listTypeID));

   pos += sizeof(listTypeID);
   std::memcpy(data + pos, distributorTypeID.data(), sizeof(listTypeID));

   pos += sizeof(distributorTypeID);
   std::memcpy(data + pos, &distributorSize, sizeof(distributorSize));

   pos += sizeof(distributorSize);
   std::memcpy(data + pos, distributor.data(), distributorSize);

   const auto result = sf_set_chunk(file, &chunk);
   assert(result == SF_ERR_NO_ERROR);
}
} // namespace LibImportExport
