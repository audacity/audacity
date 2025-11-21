/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DownwardMeterValueProvider.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "DynamicRangeProcessorTypes.h"
#include "MeterValueProvider.h"
#include <memory>
#include <utility>
#include <vector>

class DYNAMIC_RANGE_PROCESSOR_API DownwardMeterValueProvider : public MeterValueProvider
{
public:
    DownwardMeterValueProvider(float upperValue = 0.f);
    ~DownwardMeterValueProvider() = default;
    void Update(float value, bool alsoFiveSecondMax) override;
    float GetGlobalMax() const override;
    float GetFiveSecMax() const override;
    float GetCurrentMax() const override;
    Direction GetDirection() const override;
    bool IsInvisible() const override;

private:
    //! The display tends to be earlier than the audio playback. We delay the
    //! former by (approximately) this amount, for a tighter audiovisual
    //! experience.
    static constexpr auto displayDelayMs = 100;
    static constexpr auto ringBufferLength
        =displayDelayMs / compressorMeterUpdatePeriodMs;

    const float mUpperValue;
    float mGlobalMin;
    float mCurrentMin;
    float mFiveSecMinState;
    std::vector<std::pair<int /*tick count*/, float> > mLastFiveSeconds;
    std::array<float, ringBufferLength> mRingBuffer;
    size_t mRingBufferIndex = 0;
    int mTimerCount = 0;
};
