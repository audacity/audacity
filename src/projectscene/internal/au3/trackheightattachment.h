/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3wrap/au3types.h"
#include "trackedit/dom/track.h"

#include "au3-track/Track.h"

namespace au::au3 {
class TrackHeightAttachment : public TrackAttachment
{
public:
    static TrackHeightAttachment& Get(const Au3Track* track);
    static TrackHeightAttachment& Get(Au3Track* track);

    TrackHeightAttachment(Au3Track& track);
    void Reparent(const std::shared_ptr<Au3Track>& parent) override;
    void CopyTo(Au3Track& track) const override;
    void WriteXMLAttributes(XMLWriter& writer) const override;
    bool HandleXMLAttribute(const std::string_view& attr, const XMLAttributeValueView& valueView) override;

    int GetHeight() const;
    void SetHeight(int height);

private:
    std::weak_ptr<Au3Track> mTrack;
    int mHeight{ 0 };  // 0 means use default height
};
}
