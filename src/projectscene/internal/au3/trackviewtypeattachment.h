/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3wrap/au3types.h"

#include "au3-track/Track.h"
#include "../../types/trackviewtype.h"

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

    projectscene::TrackViewType GetTrackViewType() const;
    void SetTrackViewType(const projectscene::TrackViewType&);

private:
    std::weak_ptr<Au3Track> mTrack;
    projectscene::TrackViewType mTrackViewType = projectscene::TrackViewType::Undefined;
};
}
