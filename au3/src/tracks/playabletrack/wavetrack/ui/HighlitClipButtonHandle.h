/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  HighlitClipButtonHandle.h

  A `UIHandle` implementation to interact with the clip buttons when they are
  highlit, i.e., when the mouse hovers over them.

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "ClipButtonId.h"
#include "UIHandle.h"
#include "WaveTrack.h"
#include <functional>

class wxRect;
class wxPoint;
class Channel;
class TrackPanelMouseEvent;
class AudacityProject;
class wxWindow;
class AffordanceHandle;

class HighlitClipButtonHandle /* not final */ : public UIHandle
{
public:
    HighlitClipButtonHandle(
        ClipButtonId id, std::shared_ptr<WaveTrack> track, std::shared_ptr<WaveTrack::Interval> clip);

    void Enter(bool forward, AudacityProject* pProject) override;

    void Draw(
        TrackPanelDrawingContext& context, const wxRect& rect, unsigned iPass) override;

    Result
    Click(const TrackPanelMouseEvent& event, AudacityProject* pProject) override;

    Result
    Drag(const TrackPanelMouseEvent& event, AudacityProject* pProject) override;

    Result Cancel(AudacityProject* pProject) override;

    std::shared_ptr<const Track> FindTrack() const override;

    Result Release(
        const TrackPanelMouseEvent& event, AudacityProject* pProject, wxWindow* pParent) override;

    virtual void DoDraw(const wxRect& args, wxDC& dc) = 0;

protected:
    virtual Result DoRelease(
        const TrackPanelMouseEvent& event, AudacityProject* pProject, wxWindow* pParent) = 0;

    UIHandle::Result UpdateTrackSelection(
        const TrackPanelMouseEvent& event, AudacityProject* pProject);

    UIHandle::Result
    SelectAt(const TrackPanelMouseEvent& event, AudacityProject* project);

    ClipButtonId mButtonId;
    std::shared_ptr<WaveTrack> mTrack;
    std::shared_ptr<WaveTrack::Interval> mClip;

private:
    static void Highlight(const wxRect& rect, wxDC& dc);
    std::weak_ptr<TrackPanelCell> mwCell;
};
