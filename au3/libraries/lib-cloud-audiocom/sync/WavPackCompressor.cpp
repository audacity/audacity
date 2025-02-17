/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WavPackCompressor.cpp

  Dmitry Vedenko

**********************************************************************/

#include "WavPackCompressor.h"

#include <cmath>
#include <cstdint>
#include <cstring>

#include <wavpack/wavpack.h>

#include "SampleFormat.h"
#include "SampleBlock.h"

#include "MemoryX.h"

#include "FromChars.h"

namespace audacity::cloud::audiocom::sync {
namespace {
struct Exporter final
{
    WavpackContext* Context { nullptr };
    LockedBlock Block;
    std::vector<uint8_t> CompressedData;

    Exporter(LockedBlock block)
        : Block{std::move(block)}
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
            !WavpackSetConfiguration(
                Context, &config, Block.Block->GetSampleCount())
            || !WavpackPackInit(Context)) {
            WavpackCloseFile(Context);
            Context = nullptr;
        }
    }

    ~Exporter()
    {
        if (Context != nullptr) {
            WavpackCloseFile(Context);
        }
    }

    std::vector<uint8_t> Compress()
    {
        const auto sampleFormat = Block.Format;
        const auto sampleCount = Block.Block->GetSampleCount();
        const auto dataSize = sampleCount * SAMPLE_SIZE(sampleFormat);

        std::vector<std::remove_pointer_t<samplePtr> > sampleData;
        sampleData.resize(dataSize);

        const size_t samplesRead = Block.Block->GetSamples(
            sampleData.data(), Block.Format, 0, sampleCount, false);

        // Reserve 1.5 times the size of the original data
        // The compressed data will be smaller than the original data,
        // but we overallocate just in case
        CompressedData.reserve(sampleData.size() * 3 / 2);

        if (sampleFormat == int16Sample) {
            constexpr size_t conversionSamplesCount = 4096;

            const int16_t* int16Data = reinterpret_cast<const int16_t*>(sampleData.data());
            std::vector<int32_t> buffer;
            buffer.resize(conversionSamplesCount);

            for (size_t firstSample = 0; firstSample < samplesRead;
                 firstSample += conversionSamplesCount) {
                const auto samplesThisRun
                    =std::min(conversionSamplesCount, samplesRead - firstSample);

                for (size_t i = 0; i < samplesThisRun; ++i) {
                    buffer[i]
                        =(static_cast<int32_t>(int16Data[firstSample + i]) * 65536) >>
                          16;
                }

                WavpackPackSamples(Context, buffer.data(), samplesThisRun);
            }
        } else {
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
        if (Context == nullptr) {
            return;
        }

        WavpackPackSamples(Context, data, count);
    }

    void Flush()
    {
        if (Context == nullptr) {
            return;
        }

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
        if (id == nullptr || data == nullptr || length == 0) {
            return true;
        }

        Exporter* exporter = static_cast<Exporter*>(id);

        auto start = reinterpret_cast<uint8_t*>(data);
        auto end = start + length;

        exporter->CompressedData.insert(
            exporter->CompressedData.end(), start, end);

        return true;
    }
}; // struct Exporter

struct Importer final
{
    WavpackContext* Context { nullptr };

    // Stream data
    const void* Data { nullptr };
    const int64_t Size { Offset };

    int64_t Offset { 0 };

    int Mode {};
    uint32_t SamplesCount {};

    uint8_t UngetcChar { 0 };
    bool UngetcFlag { false };

    // Header data

    sampleFormat Format { undefinedSample };
    int64_t BlockId { -1 };

    // Out buffers
    std::vector<int32_t> Int32Buffer;
    std::vector<int16_t> Int16Buffer;
    std::vector<float> FloatBuffer;

    bool IsValid() const
    {
        return Context != nullptr && SamplesCount > 0
               && Format != undefinedSample
               &&//The negative BlockId means silenced blocks and it's absolute value encodes it's length
                 //Older projects used to store them on the cloud as regular blocks.
               ((BlockId >= 0) || (BlockId < 0 && -BlockId == SamplesCount));
    }

    Importer(const void* data, const int64_t size)
        : Data{data}
        , Size{size}
    {
        if (Data == nullptr || Size == 0) {
            return;
        }

        char error[81];

        Context = WavpackOpenFileInputEx64(
            &raw_reader, this, nullptr, error, OPEN_DSD_AS_PCM | OPEN_TAGS, 0);

        if (Context == nullptr) {
            return;
        }

        Mode = WavpackGetMode(Context);
        SamplesCount = WavpackGetNumSamples(Context);

        const auto formatString = ReadTag("FORMAT");
        unsigned format {};
        auto result = FromChars(
            formatString.c_str(), formatString.c_str() + formatString.size(),
            format);
        if (result.ec == std::errc {}) {
            Format = static_cast<sampleFormat>(format);
        }

        auto blockIdString = ReadTag("BLOCK_ID");
        FromChars(
            blockIdString.c_str(), blockIdString.c_str() + blockIdString.size(),
            BlockId);
    }

    std::string ReadTag(const char* tagName)
    {
        if (Context == nullptr) {
            return {}
        }

        const auto tagLength = WavpackGetTagItem(Context, tagName, nullptr, 0);

        std::string tag;

        if (tagLength > 0) {
            tag.resize(tagLength + 1);
            WavpackGetTagItem(Context, tagName, tag.data(), tagLength + 1);
            tag.resize(tagLength);
        }

        return tag;
    }

    ~Importer()
    {
        if (Context != nullptr) {
            WavpackCloseFile(Context);
        }
    }

    bool Unpack()
    {
        if (!IsValid()) {
            return false;
        }

        Int32Buffer.resize(SamplesCount);

        if (Format == int16Sample) {
            Int16Buffer.resize(SamplesCount);
        } else if (Format == floatSample) {
            FloatBuffer.resize(SamplesCount);
        }

        const auto samplesRead
            =WavpackUnpackSamples(Context, Int32Buffer.data(), SamplesCount);

        if (samplesRead != SamplesCount) {
            return false;
        }

        const bool floatSamples = (Mode & MODE_FLOAT) == MODE_FLOAT;

        if (floatSamples) {
            FloatBuffer.resize(SamplesCount);
            std::memcpy(
                FloatBuffer.data(), Int32Buffer.data(),
                SamplesCount * sizeof(float));
        } else if (Format == int16Sample) {
            const auto bytesPerSample = WavpackGetBytesPerSample(Context);

            if (bytesPerSample == 1) {
                for (size_t i = 0; i < SamplesCount; ++i) {
                    Int16Buffer[i] = static_cast<int16_t>(Int32Buffer[i]) << 8;
                }
            } else if (bytesPerSample == 2) {
                for (size_t i = 0; i < SamplesCount; ++i) {
                    Int16Buffer[i] = static_cast<int16_t>(Int32Buffer[i]);
                }
            } else {
                assert(false);
                return false;
            }
        }

        return true;
    }

private:
    static int32_t raw_read_bytes(void* id, void* data, int32_t bcount)
    {
        Importer* importer = static_cast<Importer*>(id);
        uint8_t* outptr = static_cast<uint8_t*>(data);

        if (importer->UngetcFlag) {
            *outptr++ = importer->UngetcChar;
            importer->UngetcFlag = false;
            bcount--;
        }

        const auto bytesToCopy
            =std::min<int32_t>(bcount, importer->Size - importer->Offset);

        std::memcpy(
            outptr, static_cast<const uint8_t*>(importer->Data) + importer->Offset,
            bytesToCopy);

        outptr += bytesToCopy;
        bcount -= bytesToCopy;

        importer->Offset += bytesToCopy;

        return static_cast<int32_t>(outptr - static_cast<uint8_t*>(data));
    }

    static int32_t raw_write_bytes(void* id, void* data, int32_t bcount)
    {
        return 0;
    }

    static int64_t raw_get_pos(void* id)
    {
        return static_cast<Importer*>(id)->Offset;
    }

    static int raw_set_pos_abs(void* id, int64_t pos)
    {
        return raw_set_pos_rel(id, pos, SEEK_SET);
    }

    static int raw_set_pos_rel(void* id, int64_t delta, int mode)
    {
        Importer* importer = static_cast<Importer*>(id);

        switch (mode) {
        case SEEK_SET:
        {
            if (delta < 0 || delta > importer->Size) {
                return 1;
            }
            importer->Offset = delta;
            break;
        }
        case SEEK_CUR:
        {
            const auto newOffset = delta + importer->Offset;
            if (newOffset < 0 || newOffset > importer->Size) {
                return 1;
            }
            importer->Offset += delta;
            break;
        }
        case SEEK_END:
        {
            if (delta > 0 || -delta > importer->Size) {
                return 1;
            }
            importer->Offset = importer->Size + delta;
            break;
        }
        }

        return 0;
    }

    static int raw_push_back_byte(void* id, int c)
    {
        Importer* importer = static_cast<Importer*>(id);

        importer->UngetcChar = c;
        importer->UngetcFlag = true;

        return c;
    }

    static int64_t raw_get_length(void* id)
    {
        return static_cast<Importer*>(id)->Size;
    }

    static int raw_can_seek(void*)
    {
        return 1;
    }

    static int raw_close_stream(void*)
    {
        return 0;
    }

    WavpackStreamReader64 raw_reader { raw_read_bytes,  raw_write_bytes,
                                       raw_get_pos,     raw_set_pos_abs,
                                       raw_set_pos_rel, raw_push_back_byte,
                                       raw_get_length,  raw_can_seek,
                                       nullptr,         raw_close_stream };
}; // struct Importer

float GetFloatValue(int16_t value) noexcept
{
    return static_cast<float>(value) / std::numeric_limits<int16_t>::max();
}

float GetFloatValue(int32_t value) noexcept
{
    return static_cast<float>(value) / ((1 << 23) - 1);
}

float GetFloatValue(float value) noexcept
{
    return value;
}

template<typename T>
void UpdateRMS(DecompressedBlock& block, const std::vector<T>& data)
{
    const auto samplesCount = data.size();
    const auto sum256Count = (samplesCount + 255) / 256;
    const auto sum64kCount = (samplesCount + 65535) / 65536;

    block.Summary256.resize(sum256Count);
    block.Summary64k.resize(sum64kCount);

    auto& blockStats = block.BlockMinMaxRMS;

    for (size_t i = 0; i < samplesCount; ++i) {
        const auto value = GetFloatValue(data[i]);

        blockStats.Min = std::min(blockStats.Min, value);
        blockStats.Max = std::max(blockStats.Max, value);
        blockStats.RMS += value * value;

        auto& summary256 = block.Summary256[i / 256];

        summary256.Min = std::min(summary256.Min, value);
        summary256.Max = std::max(summary256.Max, value);
        summary256.RMS += value * value;

        auto& summary64k = block.Summary64k[i / 65536];
        summary64k.Min = std::min(summary64k.Min, value);
        summary64k.Max = std::max(summary64k.Max, value);
        summary64k.RMS += value * value;
    }

    block.BlockMinMaxRMS.RMS = std::sqrt(block.BlockMinMaxRMS.RMS / samplesCount);

    auto samplesProcessed = 0;
    for (auto& summary : block.Summary256) {
        const auto samplesToProcess = std::min<int>(256, samplesCount - samplesProcessed);
        summary.RMS = std::sqrt(summary.RMS / samplesToProcess);
        samplesProcessed += samplesToProcess;
    }

    samplesProcessed = 0;
    for (auto& summary : block.Summary64k) {
        const auto samplesToProcess = std::min<int>(65536, samplesCount - samplesProcessed);
        summary.RMS = std::sqrt(summary.RMS / samplesToProcess);
        samplesProcessed += samplesToProcess;
    }
}
} // namespace

std::vector<uint8_t> CompressBlock(const LockedBlock& block)
{
    Exporter exporter { block };
    return exporter.Compress();
}

std::optional<DecompressedBlock>
DecompressBlock(const void* data, const std::size_t size)
{
    if (data == nullptr || size == 0) {
        return {}
    }

    Importer importer { data, static_cast<int64_t>(size) };

    if (!importer.Unpack()) {
        return {}
    }

    DecompressedBlock result {};

    result.BlockId = importer.BlockId;
    result.Format = importer.Format;

    const auto sampleSize = SAMPLE_SIZE(importer.Format);

    result.Data.resize(importer.SamplesCount * sampleSize);

    if (importer.Format == int16Sample) {
        std::memcpy(
            result.Data.data(), importer.Int16Buffer.data(), result.Data.size());
        UpdateRMS(result, importer.Int16Buffer);
    } else if (importer.Format == sampleFormat::int24Sample) {
        std::memcpy(
            result.Data.data(), importer.Int32Buffer.data(), result.Data.size());
        UpdateRMS(result, importer.Int32Buffer);
    } else if (importer.Format == floatSample) {
        std::memcpy(
            result.Data.data(), importer.FloatBuffer.data(), result.Data.size());
        UpdateRMS(result, importer.FloatBuffer);
    } else {
        assert(false);
        return {};
    }

    return result;
}
} // namespace audacity::cloud::audiocom::sync
