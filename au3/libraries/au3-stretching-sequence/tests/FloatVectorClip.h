/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  FloatVectorClip.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "ClipInterface.h"

class FloatVectorClip : public ClipInterface
{
public:
    FloatVectorClip(
        int sampleRate, const std::vector<std::vector<float> >& audio);

    // Duplicates audio to numChannels
    FloatVectorClip(
        int sampleRate, const std::vector<float>& audio, size_t numChannels);

    AudioSegmentSampleView GetSampleView(
        size_t iChannel, sampleCount start, size_t len, bool mayThrow) const override;

    sampleCount GetVisibleSampleCount() const override;

    size_t NChannels() const override;

    int GetRate() const override;

    double GetPlayStartTime() const override
    {
        return playStartTime;
    }

    double GetPlayEndTime() const override
    {
        return playStartTime + GetPlayDuration();
    }

    sampleCount TimeToSamples(double time) const override;

    double GetStretchRatio() const override
    {
        return stretchRatio;
    }

    int GetCentShift() const override
    {
        return 0;
    }

    Observer::Subscription
    SubscribeToCentShiftChange(std::function<void(int)> cb) const override
    {
        return {};
    }

    PitchAndSpeedPreset GetPitchAndSpeedPreset() const override
    {
        return PitchAndSpeedPreset::Default;
    }

    Observer::Subscription SubscribeToPitchAndSpeedPresetChange(
        std::function<void(PitchAndSpeedPreset)> cb) const override
    {
        return {};
    }

public:
    double stretchRatio = 1.;
    double playStartTime = 0.;

private:
    double GetPlayDuration() const;

    const int mSampleRate;
    const std::vector<std::vector<float> > mAudio;
};
