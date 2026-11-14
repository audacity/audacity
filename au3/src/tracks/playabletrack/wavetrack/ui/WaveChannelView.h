/**********************************************************************

Audacity: A Digital Audio Editor

WaveChannelView.h

Paul Licameli split from class WaveTrack

**********************************************************************/

#ifndef __AUDACITY_WAVE_TRACK_VIEW__
#define __AUDACITY_WAVE_TRACK_VIEW__

#include "../../../ui/CommonChannelView.h"
#include "ClientData.h"
#include "SampleCount.h"
#include "WaveTrack.h"
namespace WaveChannelViewConstants {
enum Display : int;
}
struct WaveChannelSubViewType;

class ClipTimes;
class CutlineHandle;
class TranslatableString;
class WaveChannel;
class WaveChannelView;
class WaveClipAdjustBorderHandle;
class ZoomInfo;

class TrackPanelResizeHandle;
class WaveTrackAffordanceHandle;

class SubViewCloseHandle;
class SubViewAdjustHandle;
class SubViewRearrangeHandle;

class wxDC;

class AUDACITY_DLL_API WaveChannelSubView : public CommonChannelView
{
public:

    using Display = WaveChannelViewConstants::Display;
    using Type = WaveChannelSubViewType;

    explicit
    WaveChannelSubView(WaveChannelView& waveChannelView);

    std::shared_ptr<WaveChannel> FindWaveChannel();

    virtual const Type& SubViewType() const = 0;

    // For undo and redo purpose
    // Empty abstract method to be inherited, for copying the spectral data in SpectrumSubView
    virtual void CopyToSubView(WaveChannelSubView* destSubView) const;

    std::pair<
        bool, // if true, hit-testing is finished
        std::vector<UIHandlePtr>
        > DoDetailedHitTest(
        const TrackPanelMouseState& state, const AudacityProject* pProject, int currentTool, bool bMultiTool,
        const std::shared_ptr<WaveChannel>& wt);

protected:
    static void DrawBoldBoundaries(
        TrackPanelDrawingContext& context, const WaveChannel& channel, const wxRect& rect);

    std::weak_ptr<WaveChannelView> GetWaveChannelView() const;

    std::vector<MenuItem> GetMenuItems(
        const wxRect& rect, const wxPoint* pPosition, AudacityProject* pProject)
    override;

private:
    std::weak_ptr<SubViewCloseHandle> mCloseHandle;
    std::weak_ptr<TrackPanelResizeHandle> mResizeHandle;
    std::weak_ptr<SubViewAdjustHandle> mAdjustHandle;
    std::weak_ptr<SubViewRearrangeHandle> mRearrangeHandle;
    std::weak_ptr<WaveClipAdjustBorderHandle> mClipBorderAdjustHandle;
    std::weak_ptr<CutlineHandle> mCutlineHandle;
    std::weak_ptr<WaveChannelView> mwWaveChannelView;
};

struct WaveChannelSubViewPlacement {
    int index;
    float fraction;
};
using WaveChannelSubViewPlacements = std::vector<WaveChannelSubViewPlacement>;

class WaveChannelView;
using WaveChannelSubViews = ClientData::Site<
    WaveChannelView, WaveChannelSubView, ClientData::SkipCopying, std::shared_ptr
    >;

class AUDACITY_DLL_API WaveChannelView final : public CommonChannelView, public WaveChannelSubViews
{
    WaveChannelView(const WaveChannelView&) = delete;
    WaveChannelView& operator=(const WaveChannelView&) = delete;

public:
    static constexpr int kChannelSeparatorThickness{ 8 };

    using Display = WaveChannelViewConstants::Display;

    static WaveChannelView& Get(WaveChannel& channel);
    static const WaveChannelView& Get(const WaveChannel& channel);
    static WaveChannelView* Find(WaveChannel* pChannel);
    static const WaveChannelView* Find(const WaveChannel* pChannel);

    //! Get the view of the first channel
    static WaveChannelView& GetFirst(WaveTrack& wt);

    //! Get the view of the first channel
    static const WaveChannelView& GetFirst(const WaveTrack& wt);

    //! If pWt is not null, return a pointer to the view of the first channel
    static WaveChannelView* FindFirst(WaveTrack* pWt);

    //! If pWt is not null, return a pointer to the view of the first channel
    static const WaveChannelView* FindFirst(const WaveTrack* pWt);

    using CommonChannelView::CommonChannelView;
    ~WaveChannelView() override;

    std::shared_ptr<WaveChannel> FindWaveChannel();

    // Preserve some view state too for undo/redo purposes
    void CopyTo(Track& track, size_t iChannel) const override;

    std::shared_ptr<ChannelVRulerControls> DoGetVRulerControls() override;

    // CommonChannelView implementation
    void Reparent(const std::shared_ptr<Track>& parent, size_t iChannel)
    override;

    static std::pair<
        bool, // if true, hit-testing is finished
        std::vector<UIHandlePtr>
        > DoDetailedHitTest(
        const TrackPanelMouseState& state, const AudacityProject* pProject, int currentTool, bool bMultiTool,
        const std::shared_ptr<WaveChannel>& wt, CommonChannelView& view);

    std::vector<WaveChannelSubView::Type> GetDisplays() const;
    void SetDisplay(Display display, bool exclusive = true);

    const WaveChannelSubViewPlacements& SavePlacements() const
    { return DoGetPlacements(); }
    void RestorePlacements(const WaveChannelSubViewPlacements& placements)
    { DoGetPlacements() = placements; }

    // Return true if successful.  Fails if you try to toggle off the only
    // sub-view.
    bool ToggleSubView(Display id);

    // Get all the sub-views, in a sequence that is unspecified but in
    // correspondence with the result of SavePlacements
    std::vector<std::shared_ptr<WaveChannelSubView> > GetAllSubViews();

    // Return cached height of rect in last call of GetSubViews
    wxCoord GetLastHeight() const { return mLastHeight; }

    bool GetMultiView() const { return DoGetMultiView(); }
    void SetMultiView(bool value) { DoGetMultiView() = value; }

    WaveTrack::IntervalHolder GetSelectedClip();

    // Returns a visible subset of subviews, sorted in the same
    // order as they are supposed to be displayed

    // Get the visible sub-views,
    // if rect is provided then result will contain
    // y coordinate for each subview within this rect
    Refinement GetSubViews(const wxRect* rect = nullptr);

    unsigned CaptureKey(wxKeyEvent& event, ViewInfo& viewInfo, wxWindow* pParent, AudacityProject* project) override;

    unsigned KeyDown(wxKeyEvent& event, ViewInfo& viewInfo, wxWindow* pParent, AudacityProject* project) override;

    unsigned Char(wxKeyEvent& event, ViewInfo& viewInfo, wxWindow* pParent, AudacityProject* project) override;

    unsigned LoseFocus(AudacityProject* project) override;

    static bool ClipDetailsVisible(
        const ClipTimes& clip, const ZoomInfo& zoomInfo, const wxRect& viewRect);
    static wxRect ClipHitTestArea(const ClipTimes& clip, const ZoomInfo& zoomInfo, const wxRect& viewRect);
    static bool HitTest(const ClipTimes& clip, const ZoomInfo& zoomInfo, const wxRect& rect, const wxPoint& pos);

    //FIXME: These functions do not push state to undo history
    //because attempt to do so leads to a focus lose which, in
    //turn finalizes text editing (state is saved after text
    //editing was intentionally finished instead)

    bool CutSelectedText(AudacityProject& project);
    bool CopySelectedText(AudacityProject& project);
    bool PasteText(AudacityProject& project);
    bool SelectAllText(AudacityProject& project);

    std::shared_ptr<CommonTrackCell> GetAffordanceControls() override;

private:
    void BuildSubViews() const;
    void DoSetDisplay(Display display, bool exclusive = true);
    bool SelectNextClip(ViewInfo& viewInfo, AudacityProject* project, bool forward);

    // TrackPanelDrawable implementation
    void Draw(
        TrackPanelDrawingContext& context, const wxRect& rect, unsigned iPass) override;

    std::vector<UIHandlePtr> DetailedHitTest(const TrackPanelMouseState& state, const AudacityProject* pProject, int currentTool,
                                             bool bMultiTool)
    override;

    // ChannelView implementation
    Refinement GetSubViews(const wxRect& rect) override;

private:

    void DoSetMinimized(bool minimized) override;

    // Placements are in correspondence with the array of sub-views
    // in the WaveChannelSubViews base class, though their sequence is
    // unspecified and maybe different in different platforms.
    WaveChannelSubViewPlacements& DoGetPlacements();
    const WaveChannelSubViewPlacements& DoGetPlacements() const;
    mutable wxCoord mLastHeight{};

    bool& DoGetMultiView();
    bool DoGetMultiView() const;

    std::shared_ptr<CommonTrackCell> DoGetAffordance(Track& track);

    std::shared_ptr<CommonTrackCell> mpAffordanceCellControl;

    std::weak_ptr<TrackPanelCell> mKeyEventDelegate;

    std::weak_ptr<WaveTrackAffordanceHandle> mAffordanceHandle;
};
#endif
