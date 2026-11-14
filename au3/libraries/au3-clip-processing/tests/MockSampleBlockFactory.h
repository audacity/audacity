/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MockSampleBlockFactory.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "MockSampleBlock.h"
#include <numeric> // std::iota

class MockSampleBlockFactory final : public SampleBlockFactory
{
    SampleBlockIDs GetActiveBlockIDs() override
    {
        std::vector<long long> ids(blockIdCount);
        std::iota(ids.begin(), ids.end(), 1LL);
        return { ids.begin(), ids.end() };
    }

    SampleBlockPtr DoCreate(
        constSamplePtr src, size_t numsamples, sampleFormat srcformat) override
    {
        return std::make_shared<MockSampleBlock>(
            blockIdCount++, src, numsamples, srcformat);
    }

    SampleBlockPtr
    DoCreateSilent(size_t numsamples, sampleFormat srcformat) override
    {
        std::vector<char> silence(numsamples * SAMPLE_SIZE(srcformat));
        return std::make_shared<MockSampleBlock>(
            blockIdCount++, silence.data(), numsamples, srcformat);
    }

    SampleBlockPtr
    DoCreateFromXML(sampleFormat srcformat, const AttributesList& attrs) override
    {
        return nullptr;
    }

    SampleBlockPtr
    DoCreateFromId(sampleFormat srcformat, SampleBlockID id) override
    {
        return nullptr;
    }

    long long blockIdCount = 0;
};
