/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file CueTrackAttachment.h

 **********************************************************************/
#pragma once

#include "au3-track/TrackAttachment.h"
#include <wx/string.h>

class LabelTrack;

class CueTrackAttachment final : public TrackAttachment {
public:
    static CueTrackAttachment& Get(LabelTrack& track);
    static const CueTrackAttachment& Get(const LabelTrack& track);

    explicit CueTrackAttachment(LabelTrack& track);
    ~CueTrackAttachment() override;

    void WriteXMLAttributes(XMLWriter& writer) const override;
    bool HandleXMLAttribute(
        const std::string_view& attr,
        const XMLAttributeValueView& valueView) override;
    void CopyTo(Track& track) const override;

    wxString GetSourceTrackName() const { return mSourceTrackName; }
    void SetSourceTrackName(const wxString& name) { mSourceTrackName = name; }
    bool IsCueTrack() const { return !mSourceTrackName.empty(); }

private:
    wxString mSourceTrackName;
};
