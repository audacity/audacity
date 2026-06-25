/*
 * Audacity: A Digital Audio Editor
 */
#include "spectrogramtypes.h"
#include "shared/axis/numberscale.h"

#include "framework/global/realfn.h"

namespace au::spectrogram {
FrequencySelection::FrequencySelection(int trackId)
    : trackId(trackId)
{
}

void FrequencySelection::setFrequencyRange(double f1, double f2, SpectrogramScale scale)
{
    m_startFrequency = std::min(f1, f2);
    m_endFrequency = std::max(f1, f2);
    NumberScale numberScale{ scale, static_cast<float>(m_startFrequency), static_cast<float>(m_endFrequency) };
    m_centerFrequency = numberScale.positionToValue(0.5f);
}

bool FrequencySelection::isValid() const
{
    return trackId != -1
           && m_startFrequency != SelectionInfo::UndefinedFrequency
           && m_endFrequency != SelectionInfo::UndefinedFrequency
           && m_centerFrequency != SelectionInfo::UndefinedFrequency
           && muse::RealIsEqualOrLess(m_centerFrequency, m_endFrequency)
           && muse::RealIsEqualOrLess(m_startFrequency, m_centerFrequency);
}

bool FrequencySelection::operator==(const FrequencySelection& other) const
{
    return trackId == other.trackId
           && m_startFrequency == other.m_startFrequency
           && m_endFrequency == other.m_endFrequency
           && m_centerFrequency == other.m_centerFrequency;
}

bool FrequencySelection::operator!=(const FrequencySelection& other) const
{
    return !(*this == other);
}
}
