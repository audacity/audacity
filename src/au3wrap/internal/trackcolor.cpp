#include "trackcolor.h"

#include "trackedit/trackedittypes.h"

using namespace au::au3;
using namespace au::trackedit;

static const AttachedTrackObjects::RegisteredFactory keyTrackColor{
    [](Track& track) -> std::shared_ptr<TrackColor> { return std::make_shared<TrackColor>(track); }
};

static constexpr auto ColorIndexAttr = "colorindex";

static size_t lastColorIndex = 0;

TrackColor& TrackColor::Get(Track* track)
{
    return track->AttachedTrackObjects::Get<TrackColor>(keyTrackColor);
}

TrackColor& TrackColor::Get(const Track* track)
{
    return Get(const_cast<Track*>(track));
}

void TrackColor::Init(size_t index)
{
    lastColorIndex = index;
}

void TrackColor::CopyTo(Track& track) const
{
    auto& color = Get(&track);
    color.SetColorIndex(mColorIndex);
    if (mColorIndex > 0) {
        lastColorIndex = static_cast<size_t>(mColorIndex - 1);
    }
}

TrackColor::TrackColor(Track& track)
    : mTrack{track.shared_from_this()}
{
    auto ntrack = mTrack.lock();
    if (!ntrack) {
        return;
    }

    if (ntrack->GetId() == -1) {
        //Only assign color to newly created tracks
        assignColor();
    }
}

void TrackColor::Reparent(const std::shared_ptr<Track>& parent)
{
    mTrack = parent;
}

void TrackColor::WriteXMLAttributes(XMLWriter& writer) const
{
    writer.WriteAttr(ColorIndexAttr, mColorIndex);
}

bool TrackColor::HandleXMLAttribute(const std::string_view& attr, const XMLAttributeValueView& valueView)
{
    if (attr == ColorIndexAttr) {
        long nValue;
        if (valueView.TryGet(nValue)) {
            mColorIndex = static_cast<ClipColorIndex>(nValue);
        }
        return true;
    }

    return false;
}

ClipColorIndex TrackColor::GetColorIndex() const
{
    return mColorIndex;
}

void TrackColor::SetColorIndex(ClipColorIndex colorIndex)
{
    mColorIndex = colorIndex;
}

void TrackColor::assignColor()
{
    mColorIndex = static_cast<ClipColorIndex>((++lastColorIndex % CLIP_COLOR_COUNT) + 1);
}
