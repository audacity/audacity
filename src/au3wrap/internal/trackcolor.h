#pragma once

#include "au3-track/Track.h"
#include "trackedit/trackedittypes.h"

namespace au::au3 {
class TrackColor : public TrackAttachment
{
public:
    static TrackColor& Get(const Track* track);
    static TrackColor& Get(Track* track);

    static void Init(size_t index);

    TrackColor(Track& track);
    void Reparent(const std::shared_ptr<Track>& parent) override;
    void CopyTo(Track& track) const override;
    void WriteXMLAttributes(XMLWriter& writer) const override;
    bool HandleXMLAttribute(const std::string_view& attr, const XMLAttributeValueView& valueView) override;

    trackedit::ClipColorIndex GetColorIndex() const;
    void SetColorIndex(trackedit::ClipColorIndex colorIndex);

    void assignColor();

private:
    std::weak_ptr<Track> mTrack;
    trackedit::ClipColorIndex mColorIndex = 0;
};
}
