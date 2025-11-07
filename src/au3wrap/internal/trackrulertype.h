/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../au3types.h"
#include "trackedit/dom/track.h"
#include "libraries/lib-track/Track.h"

namespace au::au3 {
class TrackRulerTypeAttachment : public TrackAttachment
{
public:
    static TrackRulerTypeAttachment& Get(const Au3Track* track);
    static TrackRulerTypeAttachment& Get(Au3Track* track);

    TrackRulerTypeAttachment(Au3Track& track);
    void Reparent(const std::shared_ptr<Au3Track>& parent) override;
    void CopyTo(Au3Track& track) const override;
    void WriteXMLAttributes(XMLWriter& writer) const override;
    bool HandleXMLAttribute(const std::string_view& attr, const XMLAttributeValueView& valueView) override;

    trackedit::TrackRulerType GetRulerType() const;
    void SetRulerType(const trackedit::TrackRulerType& rulerType);

private:
    std::weak_ptr<Au3Track> mTrack;
    trackedit::TrackRulerType mRulerType;
};
}
