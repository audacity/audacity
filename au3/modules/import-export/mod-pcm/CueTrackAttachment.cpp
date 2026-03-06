/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file CueTrackAttachment.cpp

 **********************************************************************/

#include "CueTrackAttachment.h"
#include "au3-label-track/LabelTrack.h"
#include "au3-xml/XMLWriter.h"
#include <wx/log.h>

static const AttachedTrackObjects::RegisteredFactory keyCue{
    [](Track& track) -> std::shared_ptr<CueTrackAttachment> {
        if (const auto pTrack = dynamic_cast<LabelTrack*>(&track)) {
            return std::make_shared<CueTrackAttachment>(*pTrack);
        } else {
            return nullptr;
        }
    }
};

CueTrackAttachment& CueTrackAttachment::Get(LabelTrack& track)
{
    return track.AttachedObjects::Get<CueTrackAttachment>(keyCue);
}

const CueTrackAttachment& CueTrackAttachment::Get(const LabelTrack& track)
{
    return Get(const_cast<LabelTrack&>(track));
}

CueTrackAttachment::CueTrackAttachment(LabelTrack&)
{
}

CueTrackAttachment::~CueTrackAttachment() = default;

static constexpr auto CueSourceTrack_attr = "cueSourceTrack";

void CueTrackAttachment::WriteXMLAttributes(XMLWriter& writer) const
{
    if (!mSourceTrackName.empty()) {
        wxLogDebug(wxT("CueTrackAttachment: Writing cueSourceTrack='%s'"), mSourceTrackName);
        writer.WriteAttr(CueSourceTrack_attr, mSourceTrackName);
    }
}

bool CueTrackAttachment::HandleXMLAttribute(
    const std::string_view& attr, const XMLAttributeValueView& valueView)
{
    if (attr == CueSourceTrack_attr) {
        mSourceTrackName = valueView.ToWString();
        wxLogDebug(wxT("CueTrackAttachment: Read cueSourceTrack='%s'"), mSourceTrackName);
        return true;
    }
    return false;
}

void CueTrackAttachment::CopyTo(Track& track) const
{
    if (const auto pTrack = dynamic_cast<LabelTrack*>(&track)) {
        auto& other = Get(*pTrack);
        other.mSourceTrackName = mSourceTrackName;
        wxLogDebug(wxT("CueTrackAttachment: CopyTo cueSourceTrack='%s'"), mSourceTrackName);
    }
}
