#include "WavPackCompressor.h"
/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WavPackCompressor.cpp

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "WavPackCompressor.h"
#include "SampleFormat.h"

#include "sqlite3.h"

#include <wavpack/wavpack.h>

#include <wx/log.h>

namespace cloud::audiocom::sync
{
namespace
{
struct Exporter final
{
   WavpackContext* Context { nullptr };
   std::vector<uint8_t> CompressedData;
   sampleFormat Format;

   Exporter(sampleFormat format, int sampleCount)
       : Format(format)
   {
      WavpackConfig config = {};

      config.num_channels = 1;
      config.channel_mask = 0x4;
      // Sample rate is irrelevant, so just set it to something
      config.sample_rate = 48000;

      config.bytes_per_sample = SAMPLE_SIZE_DISK(format);
      config.bits_per_sample = config.bytes_per_sample * 8;
      config.float_norm_exp = format == floatSample ? 127 : 0;

      config.flags = CONFIG_FAST_FLAG;

      Context = WavpackOpenFileOutput(WriteBlock, this, nullptr);

      if (
         !WavpackSetConfiguration(Context, &config, sampleCount) ||
         !WavpackPackInit(Context))
      {
         wxLogDebug(WavpackGetErrorMessage(Context));
         WavpackCloseFile(Context);
         Context = nullptr;
      }
   }

   ~Exporter()
   {
      if (Context != nullptr)
         WavpackCloseFile(Context);
   }

   void Feed(int32_t* data, int32_t count)
   {
      if (Context == nullptr)
         return;

      WavpackPackSamples(Context, data, count);
   }

   void Flush()
   {
      if (Context == nullptr)
         return;

      WavpackFlushSamples(Context);

      std::string formatString = std::to_string(unsigned(Format));
      WavpackAppendTagItem(
         Context, "FORMAT", formatString.data(), formatString.size());

      WavpackWriteTag(Context);
      WavpackCloseFile(Context);
      Context = nullptr;
   }

   static int WriteBlock(void* id, void* data, int32_t length)
   {
      if (id == nullptr || data == nullptr || length == 0)
         return true; 

      Exporter* exporter = static_cast<Exporter*>(id);

      auto start = reinterpret_cast<uint8_t*>(data);
      auto end = start + length;

      exporter->CompressedData.insert(exporter->CompressedData.end(), start, end);

      return true;
   }
};
}

std::vector<uint8_t> CompressBlock(AudacityProject& project, BlockID blockId)
{
   // Read the block from the database
   auto db = GetProjectDatabaseHandle(project);

   sqlite3_stmt* stmt = nullptr;
   auto rc = sqlite3_prepare_v2(
      db, "SELECT sampleformat FROM sampleblocks WHERE blockid = ?;",
      -1, &stmt, nullptr);

   if (rc != SQLITE_OK)
      return {};

   sqlite3_bind_int64(stmt, 1, blockId);

   rc = sqlite3_step(stmt);

   if (rc != SQLITE_ROW)
      return {};

   const auto format = static_cast<sampleFormat>(sqlite3_column_int(stmt, 0));

   sqlite3_blob* blob = nullptr;
   rc = sqlite3_blob_open(db, "main", "sampleblocks", "samples", blockId, 0, &blob);

   if (rc != SQLITE_OK)
      return {};

   auto closeBlob = finally([&] { sqlite3_blob_close(blob); });

   const auto size = sqlite3_blob_bytes(blob);
   const auto samplesCount = size / SAMPLE_SIZE(format);

   Exporter exporter(format, samplesCount);

   if (exporter.Context == nullptr)
      return {};

   constexpr auto SAMPLES_PER_READ = 1024;

   std::vector<int32_t> buffer;
   buffer.resize(SAMPLES_PER_READ);

   std::vector<int16_t> buffer16;

   if (format == int16Sample)
      buffer16.resize(SAMPLES_PER_READ);

   auto offset = 0;

   if (format != int16Sample)
   {
      while (offset < size)
      {
         const auto bytesToRead =
            std::min<int>(SAMPLES_PER_READ * sizeof(int32_t), size - offset);
         sqlite3_blob_read(blob, buffer.data(), bytesToRead, offset);
         exporter.Feed(buffer.data(), bytesToRead / sizeof(int32_t));
         offset += bytesToRead;
      }
   }
   else
   {
      while (offset < size)
      {
         const auto bytesToRead =
            std::min<int>(SAMPLES_PER_READ * sizeof(int16_t), size - offset);
            sqlite3_blob_read(blob, buffer16.data(), bytesToRead, offset);

            for (size_t i = 0; i < bytesToRead / sizeof(int16_t); i++)
               buffer[i] = (static_cast<int32_t>(buffer16[i]) * 65536) >> 16;

            exporter.Feed(buffer.data(), bytesToRead / sizeof(int16_t));
            offset += bytesToRead;
         }
   }

   exporter.Flush();

   return std::move(exporter.CompressedData);
}
} // namespace cloud::audiocom::sync
