/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudioSegmentFactoryInterface.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "SampleCount.h"

#include <memory>
#include <vector>

class AudioSegment;
class WaveClip;
enum class PlaybackDirection;

class STRETCHING_SEQUENCE_API AudioSegmentFactoryInterface
{
public:
    virtual ~AudioSegmentFactoryInterface();

    virtual std::vector<std::shared_ptr<AudioSegment> >
    CreateAudioSegmentSequence(double playbackStartTime, PlaybackDirection) = 0;
};
