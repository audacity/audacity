/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 WaveClipTrimHandle.h

 Vitaly Sverchinsky

 **********************************************************************/

#pragma once 

#include "UIHandle.h"
#include "WaveClip.h"

class WaveTrackView;
class WaveTrack;

class WaveClipTrimHandle : public UIHandle
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
    WaveClipTrimHandle(std::unique_ptr<ClipTrimPolicy>& clipTrimPolicy);

    static UIHandlePtr HitAnywhere(std::weak_ptr<WaveClipTrimHandle>& holder,
        const std::shared_ptr<WaveTrack>& waveTrack,
        const AudacityProject* pProject,
        const TrackPanelMouseState& state);

    static UIHandlePtr HitTest(std::weak_ptr<WaveClipTrimHandle>& holder,
        WaveTrackView& view, const AudacityProject* pProject,
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
