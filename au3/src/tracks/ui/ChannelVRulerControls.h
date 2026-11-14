/**********************************************************************

Audacity: A Digital Audio Editor

ChannelVRulerControls.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_TRACK_VRULER_CONTROLS__
#define __AUDACITY_TRACK_VRULER_CONTROLS__

#include "CommonTrackPanelCell.h"

class Track;
class ChannelView;
class wxDC;

const int kGuard = 5; // 5 pixels to reduce risk of VZooming accidentally

class AUDACITY_DLL_API ChannelVRulerControls /* not final */ : public CommonChannelCell,
    public std::enable_shared_from_this<ChannelVRulerControls>
{
public:
    explicit
    ChannelVRulerControls(const std::shared_ptr<ChannelView>& pChannelView);

    virtual ~ChannelVRulerControls() = 0;

    static ChannelVRulerControls& Get(ChannelView&);
    static const ChannelVRulerControls& Get(const ChannelView&);

    // Define a default hit test method, just for message and cursor
    std::vector<UIHandlePtr> HitTest(const TrackPanelMouseState& state, const AudacityProject* pProject) override;

    // Helpers for handle classes' TrackPanelDrawable implementations
    static void DrawZooming(TrackPanelDrawingContext& context, const wxRect& rect, int zoomStart, int zoomEnd);
    static wxRect ZoomingArea(const wxRect& rect, const wxRect& panelRect);

    // Modify the ruler rectangle, and related display parameters,
    // cached in the associated track
    virtual void UpdateRuler(const wxRect& rect) = 0;

    std::shared_ptr<const ChannelView> GetChannelView() const
    {
        return mwChannelView.lock();
    }

protected:
    std::shared_ptr<Track> DoFindTrack() override;

    wxRect DrawingArea(
        TrackPanelDrawingContext&, const wxRect& rect, const wxRect& panelRect, unsigned iPass) override;

    Track* GetTrack() const;

    std::weak_ptr<ChannelView> mwChannelView;
};

#endif
