/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ClipPitchAndSpeedButtonHandle.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "HighlitClipButtonHandle.h"

class ClipPitchAndSpeedButtonHandle final : public HighlitClipButtonHandle
{
public:
    enum class Type
    {
        Speed,
        Pitch
    };

    ClipPitchAndSpeedButtonHandle(
        Type type, const std::shared_ptr<WaveTrack>& track, const std::shared_ptr<WaveTrack::Interval>& clip);

private:
    Result DoRelease(
        const TrackPanelMouseEvent& event, AudacityProject* pProject, wxWindow* pParent) override;

    HitTestPreview Preview(
        const TrackPanelMouseState& state, AudacityProject* pProject) override;

    void DoDraw(const wxRect& rect, wxDC& dc) override;

    Type mType;
};
