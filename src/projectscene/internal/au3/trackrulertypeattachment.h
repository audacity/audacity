/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "au3wrap/au3types.h"
#include "trackedit/dom/track.h"
#include "libraries/lib-track/Track.h"

class TrackRulerTypeAttachment : public TrackAttachment
{
public:
    static TrackRulerTypeAttachment& Get(const au::au3::Au3Track* track);
    static TrackRulerTypeAttachment& Get(au::au3::Au3Track* track);

    TrackRulerTypeAttachment(au::au3::Au3Track& track);
    void Reparent(const std::shared_ptr<au::au3::Au3Track>& parent) override;
    void CopyTo(au::au3::Au3Track& track) const override;
    void WriteXMLAttributes(XMLWriter& writer) const override;
    bool HandleXMLAttribute(const std::string_view& attr, const XMLAttributeValueView& valueView) override;

    au::trackedit::TrackRulerType GetRulerType() const;
    void SetRulerType(const au::trackedit::TrackRulerType& rulerType);

private:
    std::weak_ptr<au::au3::Au3Track> mTrack;
    au::trackedit::TrackRulerType mRulerType;
};
