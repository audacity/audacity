/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3wrap/au3types.h"

#include "au3-track/Track.h"

namespace au::au3 {
class TrackHalfWaveAttachment : public TrackAttachment
{
public:
    static TrackHalfWaveAttachment& Get(const Au3Track* track);
    static TrackHalfWaveAttachment& Get(Au3Track* track);

    TrackHalfWaveAttachment(Au3Track& track);
    void Reparent(const std::shared_ptr<Au3Track>& parent) override;
    void CopyTo(Au3Track& track) const override;
    void WriteXMLAttributes(XMLWriter& writer) const override;
    bool HandleXMLAttribute(const std::string_view& attr, const XMLAttributeValueView& valueView) override;

    bool IsHalfWave() const;
    void SetHalfWave(bool halfWave);

private:
    void applyToWaveformScale();

    std::weak_ptr<Au3Track> mTrack;
    bool mIsHalfWave = false;
};
}
