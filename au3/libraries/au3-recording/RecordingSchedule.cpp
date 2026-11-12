/*
 * Audacity: A Digital Audio Editor
 */
#include "RecordingSchedule.h"

#include <algorithm>

double RecordingSchedule::ToConsume() const
{
    return mDuration - Consumed();
}

double RecordingSchedule::Consumed() const
{
    return std::max(0.0, mPosition + TotalCorrection());
}

double RecordingSchedule::ToDiscard() const
{
    return std::max(0.0, -(mPosition + TotalCorrection()));
}
