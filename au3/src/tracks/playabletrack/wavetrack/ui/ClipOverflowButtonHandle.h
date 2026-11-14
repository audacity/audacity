/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ClipOverflowButtonHandle.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "HighlitClipButtonHandle.h"
#include <optional>

class ClipOverflowButtonHandle final : public HighlitClipButtonHandle
{
public:
    ClipOverflowButtonHandle(
        const std::shared_ptr<WaveTrack>& track, const std::shared_ptr<WaveTrack::Interval>& clip, std::weak_ptr<TrackPanelCell> cell);

private:
    Result DoRelease(
        const TrackPanelMouseEvent& event, AudacityProject* pProject, wxWindow* pParent) override;

    HitTestPreview Preview(
        const TrackPanelMouseState& state, AudacityProject* pProject) override;

    void DoDraw(const wxRect& rect, wxDC& dc) override;

    std::weak_ptr<TrackPanelCell> mCell;
};
