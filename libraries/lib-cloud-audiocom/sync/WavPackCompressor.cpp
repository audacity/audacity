#include "WavPackCompressor.h"
/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WavPackCompressor.cpp

  Dmitry Vedenko

**********************************************************************/

#include "WavPackCompressor.h"
#include "SampleFormat.h"

#include "SampleBlock.h"

#include <wavpack/wavpack.h>

#include <wx/log.h>

namespace cloud::audiocom::sync
{
namespace
{
struct Exporter final
{
   WavpackContext* Context { nullptr };
   LockedBlock Block;
   std::vector<uint8_t> CompressedData;

   Exporter(LockedBlock block)
       : Block { std::move(block) }
   {
      WavpackConfig config = {};

      config.num_channels = 1;
      config.channel_mask = 0x4;
      // Sample rate is irrelevant, so just set it to something
      config.sample_rate = 48000;

      config.bytes_per_sample = SAMPLE_SIZE_DISK(Block.Format);
      config.bits_per_sample = config.bytes_per_sample * 8;
      config.float_norm_exp = Block.Format == floatSample ? 127 : 0;

      config.flags = CONFIG_FAST_FLAG;

      Context = WavpackOpenFileOutput(WriteBlock, this, nullptr);

      if (
         !WavpackSetConfiguration(Context, &config, Block.Block->GetSampleCount()) ||
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

   std::vector<uint8_t> Compress()
   {
      const auto sampleFormat = Block.Format;
      const auto sampleCount = Block.Block->GetSampleCount();
      const auto dataSize = sampleCount * SAMPLE_SIZE(sampleFormat);

      std::vector<std::remove_pointer_t<samplePtr>> sampleData;
      sampleData.resize(dataSize);

      const size_t samplesRead = Block.Block->GetSamples(
         sampleData.data(), Block.Format, 0, sampleCount, false);

      if (sampleFormat == int16Sample)
      {
         constexpr size_t conversionSamplesCount = 1024;

         std::vector<int32_t> buffer;
         buffer.resize(conversionSamplesCount);

         for (size_t firstSample = 0; firstSample < samplesRead;
            ++firstSample)
         {
            const auto samplesThisRun = std::min(
               conversionSamplesCount, samplesRead - firstSample);

            for (size_t i = 0; i < samplesThisRun; ++i)
               buffer[i] = (static_cast<int32_t>(sampleData[firstSample + i]) * 65536) >> 16;

            WavpackPackSamples(Context, buffer.data(), samplesThisRun);
         }
      }
      else
      {
         const void* data = sampleData.data();
         WavpackPackSamples(
            Context, static_cast<int32_t*>(const_cast<void*>(data)),
            samplesRead);
      }

      Flush();

      return std::move(CompressedData);
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

      const std::string formatString = std::to_string(unsigned(Block.Format));
      WavpackAppendTagItem(
         Context, "FORMAT", formatString.data(), formatString.size());

      const std::string blockIdString = std::to_string(Block.Id);
      WavpackAppendTagItem(
         Context, "BLOCK_ID", blockIdString.data(), blockIdString.size());

      WavpackAppendTagItem(
         Context, "HASH", Block.Hash.data(), Block.Hash.size());

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

std::vector<uint8_t> CompressBlock(const LockedBlock& block)
{
   Exporter exporter { block };
   return exporter.Compress();
}

} // namespace cloud::audiocom::sync
