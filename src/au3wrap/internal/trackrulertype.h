/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "libraries/lib-track/Track.h"

namespace au::au3 {
enum class RulerType : uint8_t {
    Linear,
    DbLinear,
    DbLog
};

class TrackRulerType : public TrackAttachment
{
public:
    static TrackRulerType& Get(const Track* track);
    static TrackRulerType& Get(Track* track);

    TrackRulerType(Track& track);
    void Reparent(const std::shared_ptr<Track>& parent) override;
    void CopyTo(Track& track) const override;
    void WriteXMLAttributes(XMLWriter& writer) const override;
    bool HandleXMLAttribute(const std::string_view& attr, const XMLAttributeValueView& valueView) override;

    RulerType GetRulerType() const;
    void SetRulerType(const RulerType& rulerType);

private:
    std::weak_ptr<Track> mTrack;
    RulerType mRulerType;
};
}
