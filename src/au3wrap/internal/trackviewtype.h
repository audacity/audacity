/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../au3types.h"
#include "trackedit/dom/track.h"
#include "libraries/lib-track/Track.h"

namespace au::au3 {
class TrackViewTypeAttachment : public TrackAttachment
{
public:
    static TrackViewTypeAttachment& Get(const Au3Track* track);
    static TrackViewTypeAttachment& Get(Au3Track* track);

    TrackViewTypeAttachment(Au3Track& track);
    void Reparent(const std::shared_ptr<Au3Track>& parent) override;
    void CopyTo(Au3Track& track) const override;
    void WriteXMLAttributes(XMLWriter& writer) const override;
    bool HandleXMLAttribute(const std::string_view& attr, const XMLAttributeValueView& valueView) override;

    trackedit::TrackViewType GetTrackViewType() const;
    void SetTrackViewType(const trackedit::TrackViewType&);

private:
    std::weak_ptr<Au3Track> mTrack;
    trackedit::TrackViewType mTrackViewType = trackedit::TrackViewType::Unspecified;
};
}
