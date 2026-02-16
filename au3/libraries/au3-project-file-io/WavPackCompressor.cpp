/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WavPackCompressor.cpp

  Dmitry Vedenko
  Generalized for project storage

**********************************************************************/

#include "WavPackCompressor.h"

#include <cmath>
#include <cstdint>
#include <cstring>
#include <algorithm>

#include <wavpack/wavpack.h>

#include "au3-math/SampleFormat.h"

namespace audacity::project {

namespace {
struct Exporter final
{
    WavpackContext* Context { nullptr };
    std::vector<uint8_t> CompressedData;
    sampleFormat Format;
    long long BlockId;

    Exporter(size_t numSamples, sampleFormat format, long long blockId)
        : Format{format}
        , BlockId{blockId}
    {
        WavpackConfig config = {};

        config.num_channels = 1;
        config.channel_mask = 0x4;
        config.sample_rate = 44100; // Irrelevant for raw block storage, but WavPack needs a value

        config.bytes_per_sample = SAMPLE_SIZE(Format);
        config.bits_per_sample = config.bytes_per_sample * 8;
        config.float_norm_exp = Format == floatSample ? 127 : 0;

        config.flags = CONFIG_FAST_FLAG;

        Context = WavpackOpenFileOutput(WriteBlock, this, nullptr);

        if (Context) {
            if (!WavpackSetConfiguration(Context, &config, numSamples) || !WavpackPackInit(Context)) {
                WavpackCloseFile(Context);
                Context = nullptr;
            }
        }
    }

    ~Exporter()
    {
        if (Context != nullptr) {
            WavpackCloseFile(Context);
        }
    }

    std::vector<uint8_t> Compress(const void* src, size_t numSamples)
    {
        if (Context == nullptr) {
            return {};
        }

        // Reserve 1.2 times the size of the original data as a safe bet
        CompressedData.reserve(numSamples * SAMPLE_SIZE(Format) * 12 / 10);

        if (Format == int16Sample) {
            constexpr size_t conversionSamplesCount = 4096;
            const int16_t* int16Data = static_cast<const int16_t*>(src);
            std::vector<int32_t> buffer(conversionSamplesCount);

            for (size_t firstSample = 0; firstSample < numSamples; firstSample += conversionSamplesCount) {
                const auto samplesThisRun = std::min(conversionSamplesCount, numSamples - firstSample);
                for (size_t i = 0; i < samplesThisRun; ++i) {
                    buffer[i] = (static_cast<int32_t>(int16Data[firstSample + i]) * 65536) >> 16;
                }
                WavpackPackSamples(Context, buffer.data(), samplesThisRun);
            }
        } else {
            WavpackPackSamples(Context, static_cast<int32_t*>(const_cast<void*>(src)), numSamples);
        }

        Flush();

        return std::move(CompressedData);
    }

    void Flush()
    {
        if (Context == nullptr) {
            return;
        }

        WavpackFlushSamples(Context);

        const std::string formatString = std::to_string(unsigned(Format));
        WavpackAppendTagItem(Context, "FORMAT", formatString.data(), (int)formatString.size());

        if (BlockId != 0) {
            const std::string blockIdString = std::to_string(BlockId);
            WavpackAppendTagItem(Context, "BLOCK_ID", blockIdString.data(), (int)blockIdString.size());
        }

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
        exporter->CompressedData.insert(exporter->CompressedData.end(), start, start + length);

        return true;
    }
};

struct Importer final
{
    WavpackContext* Context { nullptr };
    const void* Data { nullptr };
    const int64_t Size { 0 };
    int64_t Offset { 0 };
    uint32_t SamplesCount { 0 };
    uint8_t UngetcChar { 0 };
    bool UngetcFlag { false };

    sampleFormat Format { (sampleFormat)0 }; // undefinedSample
    long long BlockId { 0 };

    Importer(const void* data, const int64_t size)
        : Data{data}
        , Size{size}
    {
        if (Data == nullptr || Size == 0) {
            return;
        }

        char error[81];
        Context = WavpackOpenFileInputEx64(&raw_reader, this, nullptr, error, OPEN_TAGS, 0);

        if (Context == nullptr) {
            return;
        }

        SamplesCount = (uint32_t)WavpackGetNumSamples(Context);

        char tagBuffer[32];
        if (WavpackGetTagItem(Context, "FORMAT", tagBuffer, sizeof(tagBuffer)) > 0) {
            Format = static_cast<sampleFormat>(std::stoul(tagBuffer));
        }

        if (WavpackGetTagItem(Context, "BLOCK_ID", tagBuffer, sizeof(tagBuffer)) > 0) {
            BlockId = std::stoll(tagBuffer);
        }
    }

    ~Importer()
    {
        if (Context != nullptr) {
            WavpackCloseFile(Context);
        }
    }

    bool Unpack(void* dest)
    {
        if (Context == nullptr || SamplesCount == 0 || Format == (sampleFormat)0) {
            return false;
        }

        if (Format == int16Sample) {
            std::vector<int32_t> buffer(SamplesCount);
            if (WavpackUnpackSamples(Context, buffer.data(), SamplesCount) != SamplesCount) {
                return false;
            }
            int16_t* out = static_cast<int16_t*>(dest);
            for (size_t i = 0; i < SamplesCount; ++i) {
                out[i] = static_cast<int16_t>(buffer[i]);
            }
        } else {
            if (WavpackUnpackSamples(Context, static_cast<int32_t*>(dest), SamplesCount) != SamplesCount) {
                return false;
            }
        }

        return true;
    }

    static int32_t raw_read_bytes(void* id, void* data, int32_t bcount)
    {
        Importer* importer = static_cast<Importer*>(id);
        uint8_t* outptr = static_cast<uint8_t*>(data);

        if (importer->UngetcFlag) {
            *outptr++ = importer->UngetcChar;
            importer->UngetcFlag = false;
            bcount--;
        }

        const auto bytesToCopy = std::min<int32_t>(bcount, (int32_t)(importer->Size - importer->Offset));
        if (bytesToCopy > 0) {
            std::memcpy(outptr, static_cast<const uint8_t*>(importer->Data) + importer->Offset, bytesToCopy);
            importer->Offset += bytesToCopy;
            outptr += bytesToCopy;
        }

        return static_cast<int32_t>(outptr - static_cast<uint8_t*>(data));
    }

    static int32_t raw_write_bytes(void*, void*, int32_t) { return 0; }
    static int64_t raw_get_pos(void* id) { return static_cast<Importer*>(id)->Offset; }
    static int raw_set_pos_abs(void* id, int64_t pos)
    {
        Importer* importer = static_cast<Importer*>(id);
        if (pos < 0 || pos > importer->Size) return 1;
        importer->Offset = pos;
        return 0;
    }
    static int raw_set_pos_rel(void* id, int64_t delta, int mode)
    {
        Importer* importer = static_cast<Importer*>(id);
        int64_t newPos = 0;
        if (mode == SEEK_SET) newPos = delta;
        else if (mode == SEEK_CUR) newPos = importer->Offset + delta;
        else if (mode == SEEK_END) newPos = importer->Size + delta;
        return raw_set_pos_abs(id, newPos);
    }
    static int raw_push_back_byte(void* id, int c) { Importer* imp = static_cast<Importer*>(id); imp->UngetcChar = (uint8_t)c; imp->UngetcFlag = true; return c; }
    static int64_t raw_get_length(void* id) { return static_cast<Importer*>(id)->Size; }
    static int raw_can_seek(void*) { return 1; }
    static int raw_close_stream(void*) { return 0; }

    WavpackStreamReader64 raw_reader { raw_read_bytes,  raw_write_bytes,
                                       raw_get_pos,     raw_set_pos_abs,
                                       raw_set_pos_rel, raw_push_back_byte,
                                       raw_get_length,  raw_can_seek,
                                       nullptr,         raw_close_stream };
};
} // namespace

std::vector<uint8_t> WavPackCompress(const void* src, size_t numSamples, sampleFormat format, long long blockId)
{
    Exporter exporter(numSamples, format, blockId);
    return exporter.Compress(src, numSamples);
}

std::optional<DecompressedBlock> WavPackDecompress(const void* src, size_t srcSize)
{
    Importer importer(src, srcSize);
    if (importer.Context == nullptr || importer.SamplesCount == 0) {
        return std::nullopt;
    }

    DecompressedBlock result;
    result.format = importer.Format;
    result.sampleCount = importer.SamplesCount;
    result.data.resize(result.sampleCount * SAMPLE_SIZE(result.format));

    if (!importer.Unpack(result.data.data())) {
        return std::nullopt;
    }

    return result;
}

} // namespace audacity::project
