/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 WaveClipAdjustBorderHandle.h

 Vitaly Sverchinsky

 **********************************************************************/

#pragma once 

#include "UIHandle.h"
#include "WaveClip.h"

class WaveChannelView;
class WaveTrack;

class WaveClipAdjustBorderHandle final : public UIHandle
{
    static constexpr int BoundaryThreshold = 5;

    static HitTestPreview HitPreview(const AudacityProject*, bool unsafe);

    //Different policies implement different trimming scenarios
    class ClipTrimPolicy
    {
    public:
       virtual ~ClipTrimPolicy();

       virtual bool Init(const TrackPanelMouseEvent& event) = 0;
       virtual UIHandle::Result Trim(const TrackPanelMouseEvent& event, AudacityProject& project) = 0;
       virtual void Finish(AudacityProject& project) = 0;
       virtual void Cancel() = 0;

       virtual void Draw(
           TrackPanelDrawingContext &context, 
           const wxRect &rect, 
           unsigned iPass);

       virtual wxRect DrawingArea(
           TrackPanelDrawingContext&, 
           const wxRect &rect, 
           const wxRect &panelRect, 
           unsigned iPass);
    };
    class AdjustBorder;
    class AdjustBetweenBorders;
    
    std::unique_ptr<ClipTrimPolicy> mClipTrimPolicy{};

public:
    WaveClipAdjustBorderHandle(std::unique_ptr<ClipTrimPolicy>& clipTrimPolicy);

    static UIHandlePtr HitAnywhere(std::weak_ptr<WaveClipAdjustBorderHandle>& holder,
        const std::shared_ptr<WaveTrack>& waveTrack,
        const AudacityProject* pProject,
        const TrackPanelMouseState& state);

    static UIHandlePtr HitTest(std::weak_ptr<WaveClipAdjustBorderHandle>& holder,
        WaveChannelView& view, const AudacityProject* pProject,
        const TrackPanelMouseState& state);

    HitTestPreview Preview(const TrackPanelMouseState& mouseState, AudacityProject* pProject) override;

    Result Click
    (const TrackPanelMouseEvent& event, AudacityProject* pProject) override;

    Result Drag
    (const TrackPanelMouseEvent& event, AudacityProject* pProject) override;

    Result Release
    (const TrackPanelMouseEvent& event, AudacityProject* pProject,
        wxWindow* pParent) override;

    Result Cancel(AudacityProject* pProject) override;

    // TrackPanelDrawable implementation

    void Draw(TrackPanelDrawingContext &context,
        const wxRect &rect,
        unsigned iPass ) override;

    wxRect DrawingArea(TrackPanelDrawingContext&,
        const wxRect &rect,
        const wxRect &panelRect,
        unsigned iPass) override;
};
