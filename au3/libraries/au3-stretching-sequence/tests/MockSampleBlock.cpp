/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MockSampleBlock.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "MockSampleBlock.h"

namespace {
std::vector<char>
copyToVector(constSamplePtr src, size_t numsamples, sampleFormat srcformat)
{
    const auto numChars = numsamples * SAMPLE_SIZE(srcformat);
    std::vector<char> data(numChars);
    std::copy(src, src + numChars, data.begin());
    return data;
}
} // namespace

MockSampleBlock::MockSampleBlock(
    long long id, constSamplePtr src, size_t numsamples, sampleFormat srcformat)
    : id{id}
    , srcFormat{srcformat}
    , data{copyToVector(src, numsamples, srcformat)}
{
}

void MockSampleBlock::CloseLock() noexcept
{
}

SampleBlockID MockSampleBlock::GetBlockID() const
{
    return id;
}

sampleFormat MockSampleBlock::GetSampleFormat() const
{
    return srcFormat;
}

size_t MockSampleBlock::GetSampleCount() const
{
    return data.size() / SAMPLE_SIZE(srcFormat);
}

bool MockSampleBlock::GetSummary256(
    float* dest, size_t frameoffset, size_t numframes)
{
    return true;
}

bool MockSampleBlock::GetSummary64k(
    float* dest, size_t frameoffset, size_t numframes)
{
    return true;
}

size_t MockSampleBlock::GetSpaceUsage() const
{
    return data.size();
}

void MockSampleBlock::SaveXML(XMLWriter&)
{
}

size_t MockSampleBlock::DoGetSamples(
    samplePtr dest, sampleFormat destformat, size_t sampleoffset,
    size_t numsamples)
{
    const auto charOffset = sampleoffset * SAMPLE_SIZE(srcFormat);
    const auto numChars = numsamples * SAMPLE_SIZE(destformat);
    std::copy(
        data.data() + charOffset, data.data() + charOffset + numChars, dest);
    return numsamples;
}

MinMaxRMS MockSampleBlock::DoGetMinMaxRMS(size_t start, size_t len)
{
    return { 0, 0, 0 };
}

MinMaxRMS MockSampleBlock::DoGetMinMaxRMS() const
{
    return { 0, 0, 0 };
}

BlockSampleView MockSampleBlock::GetFloatSampleView(bool mayThrow)
{
    std::vector<float> floatData { reinterpret_cast<const float*>(data.data()),
                                   reinterpret_cast<const float*>(
                                       data.data() + data.size()) };
    return std::make_shared<std::vector<float> >(floatData);
}
