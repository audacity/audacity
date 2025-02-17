/**********************************************************************

Audacity: A Digital Audio Editor

LabelGlyphHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_LABEL_GLYPH_HANDLE__
#define __AUDACITY_LABEL_GLYPH_HANDLE__

#include "LabelDefaultClickHandle.h"
#include "Observer.h"

class wxMouseState;
class LabelTrack;
struct LabelTrackEvent;
class NotifyingSelectedRegion;
class ZoomInfo;

/// mEdge:
/// 0 if not over a glyph,
/// else a bitwise or of :
/// 1 if over the left-hand glyph,
/// 2 if over the right-hand glyph on a label,
/// 4 if over center.
///
///   mMouseLabelLeft - index of any left label hit
///   mMouseLabelRight - index of any right label hit
///
struct LabelTrackHit
{
    LabelTrackHit(const std::shared_ptr<LabelTrack>& pLT);
    ~LabelTrackHit();

    int mEdge{};
    //This one is to distinguish ranged label from point label
    int mMouseOverLabel{ -1 };       /// Keeps track of which (ranged) label the mouse is currently over.
    int mMouseOverLabelLeft{ -1 };   /// Keeps track of which left label the mouse is currently over.
    int mMouseOverLabelRight{ -1 };  /// Keeps track of which right label the mouse is currently over.
    bool mIsAdjustingLabel {};

    std::shared_ptr<LabelTrack> mpLT {};

    Observer::Subscription mSubscription;
    void OnLabelPermuted(const LabelTrackEvent& e);
};

class LabelGlyphHandle final : public LabelDefaultClickHandle
{
public:
    explicit LabelGlyphHandle(const std::shared_ptr<LabelTrack>& pLT, const wxRect& rect, const std::shared_ptr<LabelTrackHit>& pHit);

    LabelGlyphHandle& operator=(const LabelGlyphHandle&) = default;

    static UIHandlePtr HitTest(std::weak_ptr<LabelGlyphHandle>& holder, const wxMouseState& state, const std::shared_ptr<LabelTrack>& pLT,
                               const wxRect& rect);

    virtual ~LabelGlyphHandle();

    std::shared_ptr<const Track> FindTrack() const override;

    void Enter(bool forward, AudacityProject*) override;

    Result Click(const TrackPanelMouseEvent& event, AudacityProject* pProject) override;

    Result Drag(const TrackPanelMouseEvent& event, AudacityProject* pProject) override;

    HitTestPreview Preview(const TrackPanelMouseState& state, AudacityProject* pProject)
    override;

    Result Release(const TrackPanelMouseEvent& event, AudacityProject* pProject, wxWindow* pParent) override;

    Result Cancel(AudacityProject* pProject) override;

    bool StopsOnKeystroke() override { return true; }

    std::shared_ptr<LabelTrackHit> mpHit{};

    static UIHandle::Result NeedChangeHighlight(const LabelGlyphHandle& oldState, const LabelGlyphHandle& newState);

private:
    void HandleGlyphClick(LabelTrackHit& hit, const wxMouseEvent& evt, const wxRect& r, const ZoomInfo& zoomInfo,
                          NotifyingSelectedRegion& newSel);
    bool HandleGlyphDragRelease(AudacityProject& project, LabelTrackHit& hit, const wxMouseEvent& evt, wxRect& r, const ZoomInfo& zoomInfo,
                                NotifyingSelectedRegion& newSel);

    void MayAdjustLabel(LabelTrackHit& hit, int iLabel, int iEdge, bool bAllowSwapping, double fNewTime);
    void MayMoveLabel(int iLabel, int iEdge, double fNewTime);

    std::shared_ptr<LabelTrack> mpLT {};
    wxRect mRect {};

    /// Displacement of mouse cursor from the centre being dragged.
    int mxMouseDisplacement;
};

#endif
