/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MockSampleBlock.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "SampleBlock.h"

class MockSampleBlock final : public SampleBlock
{
public:
    MockSampleBlock(
        long long id, constSamplePtr src, size_t numsamples, sampleFormat srcformat);

    void CloseLock() noexcept override;

    SampleBlockID GetBlockID() const override;

    sampleFormat GetSampleFormat() const override;

    size_t GetSampleCount() const override;

    bool
    GetSummary256(float* dest, size_t frameoffset, size_t numframes) override;

    bool
    GetSummary64k(float* dest, size_t frameoffset, size_t numframes) override;

    size_t GetSpaceUsage() const override;

    void SaveXML(XMLWriter&) override;

    size_t DoGetSamples(
        samplePtr dest, sampleFormat destformat, size_t sampleoffset, size_t numsamples) override;

    MinMaxRMS DoGetMinMaxRMS(size_t start, size_t len) override;

    MinMaxRMS DoGetMinMaxRMS() const override;

    BlockSampleView GetFloatSampleView(bool mayThrow) override;

    const long long id;
    const sampleFormat srcFormat;
    const std::vector<char> data;
};
