/*
 * Audacity: A Digital Audio Editor
 */
#include "trackheightattachment.h"

namespace au::au3 {
static const AttachedTrackObjects::RegisteredFactory keyTrackHeight{
    [](Au3Track& track) -> std::shared_ptr<TrackHeightAttachment> { return std::make_shared<TrackHeightAttachment>(track); }
};

static constexpr auto HeightAttr = "height";

TrackHeightAttachment& TrackHeightAttachment::Get(Au3Track* track)
{
    return track->AttachedTrackObjects::Get<TrackHeightAttachment>(keyTrackHeight);
}

TrackHeightAttachment& TrackHeightAttachment::Get(const Au3Track* track)
{
    return Get(const_cast<Au3Track*>(track));
}

TrackHeightAttachment::TrackHeightAttachment(Au3Track& track)
    : mTrack{track.shared_from_this()}
{
}

void TrackHeightAttachment::Reparent(const std::shared_ptr<Au3Track>& parent)
{
    mTrack = parent;
}

void TrackHeightAttachment::CopyTo(Au3Track& track) const
{
    auto& attachment = Get(&track);
    attachment.SetHeight(mHeight);
}

void TrackHeightAttachment::WriteXMLAttributes(XMLWriter& writer) const
{
    writer.WriteAttr(HeightAttr, mHeight);
}

bool TrackHeightAttachment::HandleXMLAttribute(const std::string_view& attr, const XMLAttributeValueView& valueView)
{
    long nValue;
    if (attr == HeightAttr && valueView.TryGet(nValue)) {
        mHeight = static_cast<int>(nValue);
        return true;
    }
    return false;
}

int TrackHeightAttachment::GetHeight() const
{
    return mHeight;
}

void TrackHeightAttachment::SetHeight(int height)
{
    mHeight = height;
}
}
