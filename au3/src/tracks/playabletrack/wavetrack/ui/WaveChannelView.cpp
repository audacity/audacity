/**********************************************************************

Audacity: A Digital Audio Editor

WaveChannelView.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "WaveChannelView.h"

#include <unordered_set>

#include "CutlineHandle.h"

#include <numeric>
#include <wx/dc.h>
#include <wx/graphics.h>

#include "AColor.h"
#include "WaveTrack.h"

#include "../../../../../images/Cursors.h"
#include "AllThemeResources.h"

#include "../../../../HitTestResult.h"
#include "../../../../RefreshCode.h"
#include "../../../../TrackArtist.h"
#include "../../../../TrackPanel.h"
#include "../../../../TrackPanelDrawingContext.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../TrackPanelResizeHandle.h"
#include "../../../../prefs/TracksPrefs.h"
#include "CommandContext.h"
#include "PitchAndSpeedDialog.h"
#include "ProjectHistory.h"
#include "SyncLock.h"
#include "TrackFocus.h"
#include "ViewInfo.h"

#include "../../../ui/TimeShiftHandle.h"
#include "../../../ui/ButtonHandle.h"
#include "../../../ui/CommonTrackInfo.h"

#include "ClipParameters.h"
#include "WaveChannelUtilities.h"
#include "WaveTrackAffordanceControls.h"
#include "WaveTrackAffordanceHandle.h"
#include "WaveClipAdjustBorderHandle.h"
#include "WaveClipUIUtilities.h"

constexpr int kClipDetailedViewMinimumWidth{ 3 };

using WaveChannelSubViewPtrs = std::vector<std::shared_ptr<WaveChannelSubView> >;

namespace {
struct PlacementArray : ClientData::Cloneable<> {
    static PlacementArray& Get(Track& track);
    static const PlacementArray& Get(const Track& track);
    ~PlacementArray() = default;
    std::unique_ptr<Cloneable<> > Clone() const
    {
        return std::make_unique<PlacementArray>(*this);
    }

    WaveChannelSubViewPlacements mPlacements;
    bool mMultiView{ false };
};

static const ChannelGroup::Attachments::RegisteredFactory
    key { [](auto&) { return std::make_unique<PlacementArray>(); } };

// Access for per-track effect list
PlacementArray& PlacementArray::Get(Track& track)
{
    return track.Attachments::Get<PlacementArray>(key);
}

const PlacementArray& PlacementArray::Get(const Track& track)
{
    return Get(const_cast<Track&>(track));
}
}

WaveChannelSubViewPlacements& WaveChannelView::DoGetPlacements()
{
    auto& waveChannel = *FindWaveChannel();
    return PlacementArray::Get(waveChannel.GetTrack()).mPlacements;
}

const WaveChannelSubViewPlacements& WaveChannelView::DoGetPlacements() const
{
    return const_cast<WaveChannelView&>(*this).DoGetPlacements();
}

bool& WaveChannelView::DoGetMultiView()
{
    auto& waveTrack = FindWaveChannel()->GetTrack();
    return PlacementArray::Get(waveTrack).mMultiView;
}

bool WaveChannelView::DoGetMultiView() const
{
    return const_cast<WaveChannelView&>(*this).DoGetMultiView();
}

// Structure that collects and modifies information on sub-view positions
// Written with great generality, allowing any number of sub-views
struct SubViewAdjuster
{
    enum {
        HotZoneSize = 5
    };                       // so many pixels at top and bottom of each subview

    SubViewAdjuster(WaveChannelView& view)
        : mwView{
                 std::static_pointer_cast<WaveChannelView>(view.shared_from_this())}
    {
        mSubViews = view.GetAllSubViews();
        mOrigPlacements = mNewPlacements = view.SavePlacements();
        FindPermutation();
    }

    void FindPermutation()
    {
        // Find a certain sort of the sub-views
        auto size = mOrigPlacements.size();
        wxASSERT(mSubViews.size() == size);
        mPermutation.resize(size);
        const auto begin = mPermutation.begin(), end = mPermutation.end();
        std::iota(begin, end, 0);
        static auto invisible = [](const WaveChannelSubViewPlacement& placement) {
            return placement.index < 0 || placement.fraction <= 0;
        };
        const auto comp = [this]( size_t ii, size_t jj ){
            auto& pi = mOrigPlacements[ii];
            bool iInvisible = invisible(pi);

            auto& pj = mOrigPlacements[jj];
            bool jInvisible = invisible(pj);

            // Sort the invisibles to the front, rest by index
            if (iInvisible != jInvisible) {
                return iInvisible;
            } else if (!iInvisible) {
                return pi.index < pj.index;
            } else {
                // Minor sort among the invisible views by their type
                return mSubViews[ii]->SubViewType() < mSubViews[jj]->SubViewType();
            }
        };
        std::sort(begin, end, comp);
        // Find the start of visible sub-views
        auto first = std::find_if(begin, end, [this](size_t ii){
            return !invisible(mOrigPlacements[ii]);
        });
        mFirstSubView = first - begin;
    }

    size_t NVisible() const
    { return mPermutation.size() - mFirstSubView; }

    bool ModifyPermutation(bool top)
    {
        bool rotated = false;
        const auto pBegin = mPermutation.begin(), pEnd = mPermutation.end();
        auto pFirst = pBegin + mFirstSubView;
        if (mFirstSubView > 0) {
            // In case of dragging the top edge of the topmost view, or the
            // bottom edge of the bottommost, decide which of the invisible
            // views can become visible, and reassign the sequence.
            // For definiteness, that choice depends on the subview type numbers;
            // see the sorting criteria above.
            --mFirstSubView;
            --pFirst;
            if (top) {
                // If you drag down the top, the greatest-numbered invisible
                // subview type will appear there.
                mNewPlacements[ *pFirst ].fraction = 0;
            } else {
                // If you drag up the bottom, let the least-numbered invisible
                // subview type appear there.
                mNewPlacements[ *pBegin ].fraction = 0;
                std::rotate(pBegin, pBegin + 1, pEnd);
                rotated = true;
            }
        }
        // Reassign index numbers to all sub-views and 0 fraction to invisibles
        for ( auto pIter = pBegin; pIter != pFirst; ++pIter ) {
            auto& placement = mNewPlacements[ *pIter ];
            placement.index = -1;
            placement.fraction = 0;
        }
        size_t index = 0;
        for ( auto pIter = pFirst; pIter != pEnd; ++pIter ) {
            mNewPlacements[ *pIter ].index = index++;
        }
        return rotated;
    }

    size_t FindIndex(WaveChannelSubView& subView) const
    {
        const auto begin = mPermutation.begin(), end = mPermutation.end();
        auto iter = std::find_if(begin, end, [&](size_t ii){
            return mSubViews[ ii ].get() == &subView;
        });
        return iter - begin;
    }

    std::pair< size_t, bool >
    HitTest(WaveChannelSubView& subView,
            wxCoord yy, wxCoord top, wxCoord height)
    {
        const auto index = FindIndex(subView);
        auto size = mPermutation.size();
        if (index < (int)size) {
            yy -= top;
            if (yy >= 0 && yy < HotZoneSize && index > 0) {
                return { index, true }
            }                       // top hit
            if (yy < height && yy >= height - HotZoneSize
                &&// Have not yet called ModifyPermutation; dragging bottom of
                  // bottommost view allowed only if at least one view is invisible
                (index < (int)size - 1 || mFirstSubView > 0)) {
                return { index, false }
            }                        // bottom hit
        }
        return { size, false }; // not hit
    }

    std::vector<wxCoord> ComputeHeights(wxCoord totalHeight)
    {
        // Compute integer-valued heights
        float total = 0;
        for (const auto index : mPermutation ) {
            const auto& placement = mOrigPlacements[ index ];
            total += std::max(0.f, placement.fraction);
        }
        float partial = 0;
        wxCoord lastCoord = 0;
        std::vector<wxCoord> result;
        for (const auto index : mPermutation ) {
            const auto& placement = mOrigPlacements[ index ];
            auto fraction = std::max(0.f, placement.fraction);
            wxCoord coord = ((partial + fraction) / total) * totalHeight;
            auto height = coord - lastCoord;
            result.emplace_back(height);
            mNewPlacements[ index ].fraction = height;
            lastCoord = coord;
            partial += fraction;
        }
        return result;
    }

    void UpdateViews(bool rollback)
    {
        auto pView = mwView.lock();
        if (pView) {
            WaveChannelView::Get(*pView->FindWaveChannel()).RestorePlacements(
                rollback ? mOrigPlacements : mNewPlacements);
        }
    }

    std::weak_ptr<WaveChannelView> mwView;
    WaveChannelSubViewPtrs mSubViews;
    WaveChannelSubViewPlacements mOrigPlacements, mNewPlacements;
    // Array mapping ordinal into the placement and subview arrays
    std::vector< size_t > mPermutation;
    // index into mPermutation
    size_t mFirstSubView{};
};

class SubViewAdjustHandle : public UIHandle
{
public:
    enum {
        MinHeight = SubViewAdjuster::HotZoneSize
    };

    static UIHandlePtr HitTest(std::weak_ptr<SubViewAdjustHandle>& holder,
                               WaveChannelView& view,
                               WaveChannelSubView& subView,
                               const TrackPanelMouseState& state)
    {
        if (!view.GetMultiView()) {
            return {}
        }

        SubViewAdjuster adjuster{ view };
        auto hit = adjuster.HitTest(subView,
                                    state.state.GetY(), state.rect.GetTop(), state.rect.GetHeight());
        auto index = hit.first;

        if (index < adjuster.mPermutation.size()) {
            auto result = std::make_shared< SubViewAdjustHandle >(
                std::move(adjuster), index, view.GetLastHeight(), hit.second
                );
            result = AssignUIHandlePtr(holder, result);
            return result;
        } else {
            return {}
        }
    }

    SubViewAdjustHandle(
        SubViewAdjuster&& adjuster, size_t subViewIndex,
        wxCoord viewHeight, bool top)
        : mAdjuster{std::move(adjuster)}
        , mMySubView{subViewIndex}
        , mViewHeight{viewHeight}
        , mTop{top}
    {
        if (mAdjuster.ModifyPermutation(top)) {
            --mMySubView;
        }
    }

    std::shared_ptr<const Track> FindTrack() const override
    {
        auto pView = mAdjuster.mwView.lock();
        if (pView) {
            return TrackFromChannel(pView->FindChannel());
        }
        return nullptr;
    }

    Result Click(
        const TrackPanelMouseEvent& event, AudacityProject* pProject) override
    {
        using namespace RefreshCode;
        const auto& permutation = mAdjuster.mPermutation;
        const auto size = permutation.size();
        if (mMySubView >= size) {
            return Cancelled;
        }

        if (event.event.LeftDClick()) {
            for ( auto& placement : mAdjuster.mNewPlacements ) {
                if (placement.index >= 0) {
                    placement.fraction = 1.0f;
                } else {
                    placement.fraction = 0.0f;
                }
            }
            mAdjuster.UpdateViews(false);
            ProjectHistory::Get(*pProject).ModifyState(false);

            // Do not start a drag
            return Cancelled | RefreshAll;
        }

        const auto& rect = event.rect;
        const auto height = rect.GetHeight();
        mOrigHeight = height;

        mOrigHeights = mAdjuster.ComputeHeights(mViewHeight);

        // Find the total height of the sub-views that may resize
        mTotalHeight = 0;
        auto index = (mTop ? mAdjuster.mFirstSubView : mMySubView);
        const auto end = (mTop ? mMySubView + 1 : permutation.size());
        for (; index != end; ++index) {
            mTotalHeight += mOrigHeights[ index ];
        }

        wxASSERT(height == mOrigHeights[ mMySubView ]);

        // Compute the maximum and minimum Y coordinates for drag effect
        if (mTop) {
            mOrigY = rect.GetTop();
            mYMax = rect.GetBottom();
            mYMin = mYMax - mTotalHeight + 1;
        } else {
            mOrigY = rect.GetBottom();
            mYMin = rect.GetTop();
            mYMax = mYMin + mTotalHeight - 1;
        }

        return RefreshNone;
    }

    Result Drag(const TrackPanelMouseEvent& event, AudacityProject*) override
    {
        using namespace RefreshCode;
        auto pView = mAdjuster.mwView.lock();
        if (!pView) {
            return Cancelled;
        }

        // Find new height for the dragged sub-view
        auto newY = std::max(mYMin, std::min(mYMax, event.event.GetY()));
        const auto delta = newY - mOrigY;
        wxCoord newHeight = mTop
                            ? mOrigHeight - delta
                            : mOrigHeight + delta
        ;
        wxASSERT(newHeight >= 0 && newHeight <= mTotalHeight);
        if (newHeight < MinHeight) {
            // Snap the dragged sub-view to nothing
            newHeight = 0;
        }

        // Reassign height for the dragged sub-view
        auto& myPlacement
            =mAdjuster.mNewPlacements[ mAdjuster.mPermutation[ mMySubView ] ];
        myPlacement.fraction = newHeight;

        // Grow or shrink other sub-views
        auto excess = newHeight - mOrigHeight; // maybe negative
        const auto adjustHeight = [&](size_t ii) {
            if (excess == 0) {
                return true;
            }

            const auto oldFraction = mOrigHeights[ ii ];

            auto index = mAdjuster.mPermutation[ ii ];
            auto& placement = mAdjuster.mNewPlacements[ index ];
            auto& fraction = placement.fraction;

            if (excess > oldFraction) {
                excess -= oldFraction, fraction = 0;
                return false;
            } else {
                auto newFraction = oldFraction - excess;
                if (newFraction < MinHeight) {
                    // This snaps very short sub-views to nothing
                    myPlacement.fraction += newFraction;
                    fraction = 0;
                } else {
                    fraction = newFraction;
                }
                return true;
            }
        };
        if (mTop) {
            for ( size_t ii = mMySubView; ii > 0;) {
                --ii;
                if (adjustHeight(ii)) {
                    break;
                }
            }
        } else {
            for ( size_t ii = mMySubView + 1, size = mAdjuster.mPermutation.size();
                  ii < size; ++ii
                  ) {
                if (adjustHeight(ii)) {
                    break;
                }
            }
        }

        // Save adjustment to the track and request a redraw
        mAdjuster.UpdateViews(false);
        return RefreshAll;
    }

    HitTestPreview Preview(
        const TrackPanelMouseState& state, AudacityProject*) override
    {
        static auto resizeCursor
            =::MakeCursor(wxCURSOR_ARROW, SubViewsCursorXpm, 16, 16);
        return {
            XO(
                "Click and drag to adjust sizes of sub-views, double-click to split evenly"),
            &*resizeCursor
        };
    }

    Result Release(
        const TrackPanelMouseEvent& event, AudacityProject* pProject,
        wxWindow* pParent) override
    {
        ProjectHistory::Get(*pProject).ModifyState(false);
        return RefreshCode::RefreshNone;
    }

    Result Cancel(AudacityProject*) override
    {
        mAdjuster.UpdateViews(true);
        return RefreshCode::RefreshAll;
    }

private:

    SubViewAdjuster mAdjuster;
    std::vector<wxCoord> mOrigHeights;

    // An index into mAdjuster.mPermutation
    size_t mMySubView{};

    wxCoord mYMin{}, mYMax{};
    wxCoord mViewHeight{}; // Total height of all sub-views
    wxCoord mTotalHeight{}; // Total height of adjusting sub-views only
    wxCoord mOrigHeight{};
    wxCoord mOrigY{};

    // Whether we drag the top or the bottom of the sub-view
    bool mTop{};
};

class SubViewRearrangeHandle : public UIHandle
{
public:
    // Make it somewhat wider than the close button
    enum {
        HotZoneWidth = 3 * kTrackInfoBtnSize / 2
    };

    static UIHandlePtr HitTest(std::weak_ptr<SubViewRearrangeHandle>& holder,
                               WaveChannelView& view, WaveChannelSubView& subView,
                               const TrackPanelMouseState& state)
    {
        if (!view.GetMultiView()) {
            return {}
        }

        SubViewAdjuster adjuster{ view };
        if (adjuster.NVisible() < 2) {
            return {}
        }

        auto relX = state.state.GetX() - state.rect.GetLeft();
        if (relX >= HotZoneWidth) {
            return {}
        }

        auto index = adjuster.FindIndex(subView);

        // Hit on the rearrange cursor only in the top and bottom thirds of
        // sub-view height, leaving the rest free to hit the selection cursor
        // first.
        // And also exclude the top third of the topmost sub-view and bottom
        // third of bottommost.
        auto relY = state.state.GetY() - state.rect.GetTop();
        auto height = state.rect.GetHeight();
        bool hit
            =((3 * relY < height) && index > 0) // top hit
              ||
              ((3 * relY > 2 * height)
               && index < adjuster.mPermutation.size() - 1) // bottom
            ;
        if (!hit) {
            return {}
        }

        auto result = std::make_shared< SubViewRearrangeHandle >(
            std::move(adjuster),
            index, view.GetLastHeight()
            );
        result = AssignUIHandlePtr(holder, result);
        return result;
    }

    SubViewRearrangeHandle(
        SubViewAdjuster&& adjuster, size_t subViewIndex,
        wxCoord viewHeight)
        : mAdjuster{std::move(adjuster)}
        , mMySubView{subViewIndex}
        , mViewHeight{viewHeight}
    {
    }

    std::shared_ptr<const Track> FindTrack() const override
    {
        auto pView = mAdjuster.mwView.lock();
        if (pView) {
            return TrackFromChannel(pView->FindChannel());
        }
        return nullptr;
    }

    Result Click(
        const TrackPanelMouseEvent& event, AudacityProject* pProject) override
    {
        using namespace RefreshCode;
        const auto& permutation = mAdjuster.mPermutation;
        const auto size = permutation.size();
        if (mMySubView >= size) {
            return Cancelled;
        }

        mHeights = mAdjuster.ComputeHeights(mViewHeight);

        // Find y coordinate of first sub-view
        wxCoord heightAbove = 0;
        for (auto index = mAdjuster.mFirstSubView;
             index != mMySubView; ++index) {
            heightAbove += mHeights[ index ];
        }
        mTopY = event.rect.GetTop() - heightAbove;

        return RefreshNone;
    }

    bool Clicked() const { return !mHeights.empty(); }

    enum DragChoice_t {
        Upward, Downward, Neutral
    };

    DragChoice_t DragChoice(const TrackPanelMouseEvent& event) const
    {
        // Disregard x coordinate -- so the mouse need not be in any sub-view,
        // just in the correct range of y coordinates
        auto yy = event.event.GetY();
        auto coord = mTopY;
        size_t ii = mAdjuster.mFirstSubView;
        if (yy < mTopY) {
            return (mMySubView == ii) ? Neutral : Upward;
        }

        for ( auto nn = mHeights.size(); ii < nn; ++ii ) {
            const auto height = mHeights[ ii ];
            coord += height;
            if (yy < coord) {
                break;
            }
        }

        if (ii < mMySubView) {
            if (yy < coord - mHeights[ ii ] + mHeights[ mMySubView ]) {
                return Upward;
            }
        }

        if (ii > mMySubView) {
            if (mMySubView < mHeights.size() - 1
                && yy >= coord - mHeights[ mMySubView ]) {
                return Downward;
            }
        }

        return Neutral;
    }

    Result Drag(const TrackPanelMouseEvent& event, AudacityProject*) override
    {
        using namespace RefreshCode;
        auto pView = mAdjuster.mwView.lock();
        if (!pView) {
            return Cancelled;
        }

        switch (DragChoice(event)) {
        case Upward:
        {
            std::swap(mHeights[ mMySubView ], mHeights[ mMySubView - 1 ]);
            std::swap(
                mAdjuster.mNewPlacements[ mMySubView ].index,
                mAdjuster.mNewPlacements[ mMySubView - 1 ].index
                );
            --mMySubView;
            break;
        }
        case Downward:
        {
            std::swap(mHeights[ mMySubView ], mHeights[ mMySubView + 1 ]);
            std::swap(
                mAdjuster.mNewPlacements[ mMySubView ].index,
                mAdjuster.mNewPlacements[ mMySubView + 1 ].index
                );
            ++mMySubView;
            break;
        }
        default:
            return RefreshNone;
        }

        // Save adjustment to the track and request a redraw
        mAdjuster.UpdateViews(false);
        return RefreshAll;
    }

    HitTestPreview Preview(
        const TrackPanelMouseState& state, AudacityProject*) override
    {
        static auto hoverCursor
            =::MakeCursor(wxCURSOR_HAND, RearrangeCursorXpm, 16, 16);
        static auto clickedCursor
            =::MakeCursor(wxCURSOR_HAND, RearrangingCursorXpm, 16, 16);
        return {
            XO("Click and drag to rearrange sub-views"),
            Clicked() ? &*clickedCursor : &*hoverCursor,
            XO("Rearrange sub-views")
        };
    }

    Result Release(
        const TrackPanelMouseEvent& event, AudacityProject* pProject,
        wxWindow* pParent) override
    {
        ProjectHistory::Get(*pProject).ModifyState(false);
        return RefreshCode::RefreshNone;
    }

    Result Cancel(AudacityProject*) override
    {
        mAdjuster.UpdateViews(true);
        return RefreshCode::RefreshAll;
    }

private:

    SubViewAdjuster mAdjuster;
    std::vector<wxCoord> mHeights;
    wxCoord mTopY;

    // An index into mAdjuster.mPermutation
    size_t mMySubView{};

    wxCoord mViewHeight{}; // Total height of all sub-views
};

class SubViewCloseHandle : public ButtonHandle
{
    static wxRect GetButtonRect(const wxRect& rect)
    {
        return {
            rect.GetLeft(),
            rect.GetTop(),
            kTrackInfoBtnSize,
            kTrackInfoBtnSize
        };
    }

public:
    static UIHandlePtr HitTest(std::weak_ptr<SubViewCloseHandle>& holder,
                               WaveChannelView& view, WaveChannelSubView& subView,
                               const TrackPanelMouseState& state)
    {
        SubViewAdjuster adjuster{ view };
        if (adjuster.NVisible() < 2) {
            return {}
        }

        const auto rect = GetButtonRect(state.rect);
        if (!rect.Contains(state.state.GetPosition())) {
            return {}
        }
        auto index = adjuster.FindIndex(subView);
        auto result = std::make_shared<SubViewCloseHandle>(
            std::move(adjuster), index, view.FindChannel(), rect);
        result = AssignUIHandlePtr(holder, result);
        return result;
    }

    SubViewCloseHandle(
        SubViewAdjuster&& adjuster, size_t index,
        const std::shared_ptr<Channel>& pChannel, const wxRect& rect)
        : ButtonHandle{static_cast<Track&>(pChannel->GetChannelGroup())
                       .SharedPointer(),
                       rect}
        , mpChannel{pChannel}
        , mAdjuster{std::move(adjuster)}
        , mMySubView{index}
    {
    }

    Result CommitChanges(
        const wxMouseEvent& event, AudacityProject* pProject, wxWindow* pParent)
    override
    {
        ProjectHistory::Get(*pProject).ModifyState(false);
        auto& myPlacement
            =mAdjuster.mNewPlacements[ mAdjuster.mPermutation[ mMySubView ] ];
        myPlacement.fraction = 0;
        mAdjuster.UpdateViews(false);
        return RefreshCode::RefreshAll;
    }

    TranslatableString Tip(
        const wxMouseState& state, AudacityProject& project) const override
    {
        return XO("Close sub-view");
    }

    // TrackPanelDrawable implementation
    void Draw(
        TrackPanelDrawingContext& context, const wxRect& rect, unsigned iPass)
    override
    {
        if (iPass == TrackArtist::PassMargins) { // after PassTracks
            CommonTrackInfo::DrawCloseButton(
                context, GetButtonRect(rect), mpChannel.lock().get(), this);
        }
    }

private:
    std::weak_ptr<Channel> mpChannel;
    SubViewAdjuster mAdjuster;
    size_t mMySubView{};
};

std::pair<
    bool, // if true, hit-testing is finished
    std::vector<UIHandlePtr>
    > WaveChannelSubView::DoDetailedHitTest(
    const TrackPanelMouseState& state,
    const AudacityProject* pProject, int currentTool, bool bMultiTool,
    const std::shared_ptr<WaveChannel>& wc)
{
    const auto waveTrack = wc->GetTrack().SharedPointer<WaveTrack>();
    auto results = WaveChannelView::DoDetailedHitTest(
        state, pProject, currentTool, bMultiTool, wc, *this);
    if (results.first) {
        return results;
    }

    auto pWaveChannelView = mwWaveChannelView.lock();
    if (pWaveChannelView && !state.state.HasModifiers()) {
        if (auto pHandle = SubViewCloseHandle::HitTest(
                mCloseHandle,
                *pWaveChannelView, *this, state)) {
            results.second.push_back(pHandle);
        }

        auto&& channels = waveTrack->Channels();
        if (channels.size() > 1) {
            // Only one cell is tested and we need to know
            // which one and it's relative location to the border.
            auto subviews = pWaveChannelView->GetSubViews();
            auto currentSubview = std::find_if(subviews.begin(), subviews.end(),
                                               [self = shared_from_this()](const auto& p){
                return self == p.second;
            });
            if (currentSubview != subviews.end()) {
                auto currentSubviewIndex = std::distance(subviews.begin(), currentSubview);

                const auto py = state.state.GetY();
                const auto topBorderHit = std::abs(py - state.rect.GetTop())
                                          <= WaveChannelView::kChannelSeparatorThickness / 2;
                const auto bottomBorderHit = std::abs(py - state.rect.GetBottom())
                                             <= WaveChannelView::kChannelSeparatorThickness / 2;

                auto it = channels.find(wc);
                auto currentChannelIndex = std::distance(channels.begin(), it);

                if (//for not-last-view check the bottom border hit
                    ((currentChannelIndex != channels.size() - 1)
                     && (currentSubviewIndex == static_cast<int>(subviews.size()) - 1)
                     && bottomBorderHit)
                    ||
                    //or for not-first-view check the top border hit
                    ((currentChannelIndex != 0) && currentSubviewIndex == 0 && topBorderHit)) {
                    //depending on which border hit test succeeded on we
                    //need to choose a proper target for resizing
                    if (!bottomBorderHit) {
                        --it;
                    }
                    auto result = std::make_shared<TrackPanelResizeHandle>(*it, py);
                    result = AssignUIHandlePtr(mResizeHandle, result);
                    results.second.push_back(result);
                }
            }
        }

        if (auto pHandle = SubViewAdjustHandle::HitTest(
                mAdjustHandle,
                *pWaveChannelView, *this, state)) {
            results.second.push_back(pHandle);
        }
        if (auto pHandle = SubViewRearrangeHandle::HitTest(
                mRearrangeHandle,
                *pWaveChannelView, *this, state)) {
            results.second.push_back(pHandle);
        }
    }
    if (pWaveChannelView) {
        if (auto pHandle = WaveClipAdjustBorderHandle::HitTest(
                mClipBorderAdjustHandle,
                *pWaveChannelView, pProject, state)) {
            results.second.push_back(pHandle);
        }
    }
    if (auto result = CutlineHandle::HitTest(
            mCutlineHandle, state.state, state.rect, pProject, waveTrack)) {
        // This overriding test applies in all tools
        results.second.push_back(result);
    }

    return results;
}

void WaveChannelSubView::DrawBoldBoundaries(
    TrackPanelDrawingContext& context, const WaveChannel& channel,
    const wxRect& rect)
{
    auto& dc = context.dc;
    const auto artist = TrackArtist::Get(context);

    const auto& zoomInfo = *artist->pZoomInfo;

    // x coordinates for bold lines will be the same across channels
    for (const auto loc : FindWaveTrackLocations(channel.GetTrack())) {
        bool highlightLoc = false;
        const int xx = zoomInfo.TimeToPosition(loc.pos);

        if (xx >= 0 && xx < rect.width) {
            dc.SetPen(highlightLoc ? AColor::uglyPen : *wxGREY_PEN);
            AColor::Line(dc, (int)(rect.x + xx - 1), rect.y, (int)(rect.x + xx - 1), rect.y + rect.height);
            dc.SetPen(highlightLoc ? AColor::uglyPen : *wxRED_PEN);
            AColor::Line(dc, (int)(rect.x + xx), rect.y, (int)(rect.x + xx), rect.y + rect.height);
            dc.SetPen(highlightLoc ? AColor::uglyPen : *wxGREY_PEN);
            AColor::Line(dc, (int)(rect.x + xx + 1), rect.y, (int)(rect.x + xx + 1), rect.y + rect.height);
        }
    }
}

std::weak_ptr<WaveChannelView> WaveChannelSubView::GetWaveChannelView() const
{
    return mwWaveChannelView;
}

auto WaveChannelSubView::GetMenuItems(
    const wxRect& rect, const wxPoint* pPosition, AudacityProject* pProject)
-> std::vector<MenuItem>
{
    auto pChannel = FindWaveChannel();
    if (pChannel != nullptr && pPosition != nullptr) {
        auto& track = pChannel->GetTrack();
        const auto& viewInfo = ViewInfo::Get(*pProject);
        const auto t = viewInfo.PositionToTime(pPosition->x, rect.x);
        if (
            (track.IsSelected() && t > viewInfo.selectedRegion.t0()
             && t < viewInfo.selectedRegion.t1()
             && !track
             .GetSortedClipsIntersecting(
                 viewInfo.selectedRegion.t0(), viewInfo.selectedRegion.t1())
             .empty())
            || WaveChannelUtilities::GetClipAtTime(**track.Channels().begin(), t)) {
            return WaveClipUIUtilities::GetWaveClipMenuItems();
        }
    }
    return {
        { L"Paste", XO("Paste") },
        {},
        { L"TrackMute", XO("Mute/Unmute Track") },
    };
}

WaveChannelView& WaveChannelView::Get(WaveChannel& channel)
{
    return static_cast<WaveChannelView&>(ChannelView::Get(channel));
}

const WaveChannelView& WaveChannelView::Get(const WaveChannel& channel)
{
    return Get(const_cast<WaveChannel&>(channel));
}

WaveChannelView* WaveChannelView::Find(WaveChannel* pChannel)
{
    return static_cast<WaveChannelView*>(ChannelView::Find(pChannel));
}

const WaveChannelView* WaveChannelView::Find(const WaveChannel* pChannel)
{
    return Find(const_cast<WaveChannel*>(pChannel));
}

WaveChannelView& WaveChannelView::GetFirst(WaveTrack& wt)
{
    assert(wt.Channels().size() > 0);
    return Get(**wt.Channels().begin());
}

const WaveChannelView& WaveChannelView::GetFirst(const WaveTrack& wt)
{
    return GetFirst(const_cast<WaveTrack&>(wt));
}

WaveChannelView* WaveChannelView::FindFirst(WaveTrack* pWt)
{
    return pWt ? &GetFirst(*pWt) : nullptr;
}

const WaveChannelView* WaveChannelView::FindFirst(const WaveTrack* pWt)
{
    return pWt ? &GetFirst(*pWt) : nullptr;
}

WaveChannelSubView::WaveChannelSubView(WaveChannelView& waveChannelView)
    : CommonChannelView{waveChannelView.FindChannel()}
{
    mwWaveChannelView = std::static_pointer_cast<WaveChannelView>(
        waveChannelView.shared_from_this());
}

std::shared_ptr<WaveChannel> WaveChannelSubView::FindWaveChannel()
{
    return FindChannel<WaveChannel>();
}

void WaveChannelSubView::CopyToSubView(WaveChannelSubView* destSubView) const
{
}

WaveChannelView::~WaveChannelView()
{
}

std::shared_ptr<WaveChannel> WaveChannelView::FindWaveChannel()
{
    return FindChannel<WaveChannel>();
}

void WaveChannelView::CopyTo(Track& track, size_t iChannel) const
{
    ChannelView::CopyTo(track, iChannel);
    auto& other = ChannelView::Get(*track.GetChannel(0));
    if (const auto pOther = dynamic_cast<WaveChannelView*>(&other)) {
        // only these fields are important to preserve in undo/redo history
        pOther->RestorePlacements(SavePlacements());
        pOther->DoGetMultiView() = DoGetMultiView();

        auto srcSubViewsPtrs
            =const_cast<WaveChannelView*>(this)->GetAllSubViews();
        auto destSubViewsPtrs
            =const_cast<WaveChannelView*>(pOther)->GetAllSubViews();
        wxASSERT(srcSubViewsPtrs.size() == destSubViewsPtrs.size());

        for (auto i = 0; i != srcSubViewsPtrs.size(); i++) {
            srcSubViewsPtrs[i]->CopyToSubView(destSubViewsPtrs[i].get());
        }
    }
}

std::vector<UIHandlePtr> WaveChannelView::DetailedHitTest(
    const TrackPanelMouseState& st,
    const AudacityProject* pProject, int currentTool, bool bMultiTool)
{
    // should not come here any more, delegation to sub-view instead
    wxASSERT(false);
    return {};
}

std::pair< bool, std::vector<UIHandlePtr> >
WaveChannelView::DoDetailedHitTest(
    const TrackPanelMouseState& st,
    const AudacityProject* pProject, int currentTool, bool bMultiTool,
    const std::shared_ptr<WaveChannel>& pChannel,
    CommonChannelView& view)
{
    // common hit-testing for different sub-view types, to help implement their
    // DetailedHitTest()

    // This is the only override of Track::DetailedHitTest that still
    // depends on the state of the Tools toolbar.
    // If that toolbar were eliminated, this could simplify to a sequence of
    // hit test routines describable by a table.

    std::vector<UIHandlePtr> results;

    const auto& viewInfo = ViewInfo::Get(*pProject);
    const auto pTrack = pChannel->GetTrack().SharedPointer<WaveTrack>();

    for (const auto& clip : pChannel->Intervals()) {
        if (!WaveChannelView::ClipDetailsVisible(*clip, viewInfo, st.rect)
            && HitTest(*clip, viewInfo, st.rect, st.state.GetPosition())) {
            auto& waveChannelView = WaveChannelView::Get(*pChannel);
            results.push_back(
                AssignUIHandlePtr(
                    waveChannelView.mAffordanceHandle,
                    std::make_shared<WaveTrackAffordanceHandle>(pTrack, clip)
                    )
                );
        }
    }

    if (bMultiTool && st.state.CmdDown()) {
        // Ctrl modifier key in multi-tool overrides everything else
        // (But this does not do the time shift constrained to the vertical only,
        //  which is what happens when you hold Ctrl in the Time Shift tool mode)
        auto result = TimeShiftHandle::HitAnywhere(view.mTimeShiftHandle,
                                                   pTrack, false);
        if (result) {
            results.push_back(result);
        }
        return { true, results };
    }

    return { false, results };
}

auto WaveChannelView::GetDisplays() const
-> std::vector<WaveChannelSubView::Type>
{
    BuildSubViews();

    // Collect the display types of visible views and sort them by position
    using Pair = std::pair<int, WaveChannelSubView::Type>;
    std::vector< Pair > pairs;
    size_t ii = 0;
    const auto& placements = DoGetPlacements();
    WaveChannelSubViews::ForEach([&](const WaveChannelSubView& subView) {
        auto& placement = placements[ii];
        if (placement.fraction > 0) {
            pairs.emplace_back(placement.index, subView.SubViewType());
        }
        ++ii;
    });
    std::sort(pairs.begin(), pairs.end());
    std::vector<WaveChannelSubView::Type> results;
    for (const auto& pair : pairs) {
        results.push_back(pair.second);
    }
    return results;
}

void WaveChannelView::SetDisplay(Display display, bool exclusive)
{
    BuildSubViews();
    DoSetDisplay(display, exclusive);
}

bool WaveChannelView::ToggleSubView(Display display)
{
    size_t ii = 0;
    size_t found = 0;
    if (WaveChannelSubViews::FindIf([&](const WaveChannelSubView& subView) {
        if (subView.SubViewType().id == display) {
            found = ii;
            return true;
        }
        ++ii;
        return false;
    })) {
        auto& placements = DoGetPlacements();
        auto& foundPlacement = placements[found];
        if (foundPlacement.fraction > 0.0) {
            // Toggle off

            if (GetDisplays().size() < 2) {
                // refuse to do it
                return false;
            }

            auto index = foundPlacement.index;
            foundPlacement = { -1, 0.0 };
            if (index >= 0) {
                for ( auto& placement : placements ) {
                    if (placement.index > index) {
                        --placement.index;
                    }
                }
            }

            return true;
        } else {
            // Toggle on
            float total = 0;
            int greatest = -1;
            unsigned nn = 0;
            for ( const auto& placement : placements ) {
                if (placement.fraction > 0.0 && placement.index >= 0) {
                    total += placement.fraction;
                    greatest = std::max(greatest, placement.index);
                    ++nn;
                }
            }
            // Turn on the sub-view, putting it lowest, and with average of the
            // heights of the other sub-views
            foundPlacement = { greatest + 1, total / nn };

            return true;
        }
    } else {
        // unknown sub-view
        return false;
    }
}

// If exclusive, make the chosen view take up all the height.  Else,
// partition equally, putting the specified view on top.
// Be sure the sequence in which the other views appear is determinate.
void WaveChannelView::DoSetDisplay(Display display, bool exclusive)
{
    // Some generality here anticipating more than two views.
    // The order of sub-views in the array is not specified, so make it definite
    // by sorting by the view type constants.
    size_t ii = 0;
    std::vector<std::pair<WaveChannelViewConstants::Display, size_t> > pairs;
    WaveChannelSubViews::ForEach([&pairs, &ii](WaveChannelSubView& subView){
        pairs.push_back({ subView.SubViewType().id, ii++ });
    });
    std::sort(pairs.begin(), pairs.end());

    int jj = 1;
    auto& placements = DoGetPlacements();
    for ( const auto& pair : pairs ) {
        auto& placement = placements[ pair.second ];
        if (pair.first == display) {
            // 0 for first view
            placement = { 0, 1.0 };
        } else if (exclusive) {
            // -1 for not displayed
            placement = { -1, 0.0 }
        } else {
            // positions other than the first.
            // (Note that the fractions in the placement don't need to be
            // denominated to 1.  Just make them all equal to get an equal
            // partitioning of the sub-views.)
            placement = { jj++, 1.0 }
        }
    }
}

namespace {
template<typename Iter>
Iter SelectedClip(const ViewInfo& viewInfo, Iter begin, Iter end)
{
    //! Decide whether a clip is selected from its start and end times (only)
    const auto isClipSelected
        =[&viewInfo](const std::shared_ptr<WaveChannelInterval>& pClip) {
        return pClip->GetPlayStartTime() == viewInfo.selectedRegion.t0()
               && pClip->GetPlayEndTime() == viewInfo.selectedRegion.t1();
    };
    return std::find_if(begin, end, isClipSelected);
}

template<typename Iter, typename Comp>
std::shared_ptr<WaveChannelInterval>
NextClipLooped(ViewInfo& viewInfo, Iter begin, Iter end, Comp comp)
{
    auto it = SelectedClip(viewInfo, begin, end);
    if (it == end) {
        it = std::find_if(begin, end, comp);
    } else {
        it = std::next(it);
    }

    if (it == end) {
        return *begin;
    }
    return *it;
}
}

bool WaveChannelView::SelectNextClip(
    ViewInfo& viewInfo, AudacityProject* project, bool forward)
{
    //Iterates through clips in a looped manner
    const auto pChannel = FindWaveChannel();
    if (!pChannel) {
        return false;
    }
    auto clips = WaveChannelUtilities::SortedClipArray(*pChannel);
    if (clips.empty()) {
        return false;
    }

    std::shared_ptr<WaveChannelInterval> clip{};
    if (forward) {
        clip = NextClipLooped(viewInfo, clips.begin(), clips.end(),
                              [&](const auto& other) {
            return other->GetPlayStartTime() >= viewInfo.selectedRegion.t1();
        });
    } else {
        clip = NextClipLooped(viewInfo, clips.rbegin(), clips.rend(),
                              [&](const auto& other) {
            return other->GetPlayStartTime() <= viewInfo.selectedRegion.t0();
        });
    }

    viewInfo.selectedRegion.setTimes(clip->GetPlayStartTime(), clip->GetPlayEndTime());
    ProjectHistory::Get(*project).ModifyState(false);

    // create and send message to screen reader
    auto it = std::find(clips.begin(), clips.end(), clip);
    auto index = std::distance(clips.begin(), it);

    auto& waveTrack = pChannel->GetTrack();
    auto wideClipIt = waveTrack.Intervals().first;
    std::advance(wideClipIt, waveTrack.GetClipIndex(clip->GetClip()));
    PitchAndSpeedDialog::Get(*project)
    .Retarget(waveTrack.SharedPointer<WaveTrack>(), *wideClipIt);

    auto message = XP(
        /* i18n-hint:
            string is the name of a clip
            first number is the position of that clip in a sequence of clips,
            second number counts the clips */
        "%s, %d of %d clip",
        "%s, %d of %d clips",
        2
        )(
        clip->GetClip().GetName(),
        static_cast<int>(index + 1),
        static_cast<int>(clips.size())
        );

    TrackFocus::Get(*project).MessageForScreenReader(message);
    return true;
}

auto WaveChannelView::GetSubViews(const wxRect& rect) -> Refinement
{
    return GetSubViews(&rect);
}

auto WaveChannelView::GetSubViews(const wxRect* rect) -> Refinement
{
    BuildSubViews();

    // Collect the visible views in the right sequence
    struct Item {
        int index;
        float fraction;
        std::shared_ptr<ChannelView> pView;
    };
    std::vector< Item > items;
    size_t ii = 0;
    float total = 0;
    const auto& placements = DoGetPlacements();
    WaveChannelSubViews::ForEach([&](WaveChannelSubView& subView) {
        auto& placement = placements[ii];
        auto index = placement.index;
        auto fraction = placement.fraction;
        if (index >= 0 && fraction > 0.0) {
            total += fraction,
            items.push_back({ index, fraction, subView.shared_from_this() });
        }
        ++ii;
    });
    std::sort(items.begin(), items.end(), [](const Item& a, const Item& b) {
        return a.index < b.index;
    });

    // Remove views we don't need
    auto begin = items.begin(), end = items.end(),
         newEnd = std::remove_if(begin, end,
                                 [](const Item& item) { return !item.pView; });
    items.erase(newEnd, end);

    Refinement results;

    if (rect != nullptr) {
        // Assign coordinates, redenominating to the total height,
        // storing integer values
        results.reserve(items.size());
        const auto top = rect->GetTop();
        const auto height = rect->GetHeight();
        float partial = 0;
        wxCoord lastCoord = 0;
        for (const auto& item : items) {
            wxCoord newCoord = top + (partial / total) * height;
            results.emplace_back(newCoord, item.pView);
            partial += item.fraction;
        }

        // Cache for the use of sub-view dragging
        mLastHeight = height;
    } else {
        std::transform(items.begin(), items.end(), std::back_inserter(results), [](const auto& item) {
            return std::make_pair(0, item.pView);
        });
    }

    return results;
}

/*
 Note that the WaveChannelView isn't in the TrackPanel subdivision, but it is
 set sometimes as the focused cell, and therefore the following functions can
 be visited.  To visit their overrides in the sub-views and affordances,
 which are never focused, we must forward to them.  To do that properly, if
 any cell declines to handle the event by setting it as skipped, it must be
 set again to not-skipped before attempting the next call-through.
 */
unsigned WaveChannelView::CaptureKey(
    wxKeyEvent& event, ViewInfo& viewInfo, wxWindow* pParent,
    AudacityProject* project)
{
    unsigned result{ RefreshCode::RefreshNone };
    const auto pChannel = FindWaveChannel();
    if (!pChannel) {
        return result;
    }
    auto& track = pChannel->GetTrack();
    for (auto pChannel : track.Channels()) {
        event.Skip(false);
        auto& waveChannelView = WaveChannelView::Get(*pChannel);
        // Give sub-views first chance to handle the event
        for (auto& subView : waveChannelView.GetSubViews()) {
            // Event defaults in skipped state which must be turned off explicitly
            wxASSERT(!event.GetSkipped());
            result |= subView.second->CaptureKey(event, viewInfo, pParent, project);
            if (!event.GetSkipped()) {
                // sub view wants it
                mKeyEventDelegate = subView.second;
                return result;
            } else {
                event.Skip(false);
            }
        }

        if (auto affordance = waveChannelView.GetAffordanceControls()) {
            result |= affordance->CaptureKey(event, viewInfo, pParent, project);
            if (!event.GetSkipped()) {
                mKeyEventDelegate = affordance;
                return result;
            }
        }

        event.Skip(false);
    }
    switch (event.GetKeyCode()) {
    case WXK_TAB:
        break;
    default:
        result |= CommonChannelView::CaptureKey(
            event, viewInfo, pParent, project);
        break;
    }
    if (!event.GetSkipped()) {
        mKeyEventDelegate = shared_from_this();
    }

    return result;
}

unsigned WaveChannelView::KeyDown(
    wxKeyEvent& event, ViewInfo& viewInfo, wxWindow* pParent,
    AudacityProject* project)
{
    unsigned result{ RefreshCode::RefreshNone };
    if (auto delegate = mKeyEventDelegate.lock()) {
        if (auto pWaveChannelView
                =dynamic_cast<WaveChannelView*>(delegate.get())) {
            if (event.GetKeyCode() == WXK_TAB) {
                SelectNextClip(viewInfo, project, event.GetModifiers() != wxMOD_SHIFT);
                result |= RefreshCode::RefreshCell;
            } else {
                result |= pWaveChannelView->CommonChannelView::KeyDown(
                    event, viewInfo, pParent, project);
            }
        } else {
            result |= delegate->KeyDown(event, viewInfo, pParent, project);
        }
    } else {
        event.Skip();
    }

    return result;
}

unsigned WaveChannelView::Char(
    wxKeyEvent& event, ViewInfo& viewInfo, wxWindow* pParent,
    AudacityProject* project)
{
    unsigned result{ RefreshCode::RefreshNone };
    if (auto delegate = mKeyEventDelegate.lock()) {
        if (auto pWaveChannelView
                =dynamic_cast<WaveChannelView*>(delegate.get())) {
            result |= pWaveChannelView->CommonChannelView::Char(
                event, viewInfo, pParent, project);
        } else {
            result |= delegate->Char(event, viewInfo, pParent, project);
        }
    } else {
        event.Skip();
    }

    return result;
}

unsigned WaveChannelView::LoseFocus(AudacityProject* project)
{
    unsigned result = RefreshCode::RefreshNone;
    if (auto delegate = mKeyEventDelegate.lock()) {
        if (auto pWaveChannelView
                =dynamic_cast<WaveChannelView*>(delegate.get())) {
            result = pWaveChannelView->CommonChannelView::LoseFocus(project);
        } else {
            result = delegate->LoseFocus(project);
        }
        mKeyEventDelegate.reset();
    }
    return result;
}

namespace {
using PMF = bool (WaveTrackAffordanceControls::*)(AudacityProject&);
bool AnyAffordance(AudacityProject& project, WaveChannelView& view, PMF pmf)
{
    const auto pWaveChannel = view.FindWaveChannel();
    const auto pTrack = &pWaveChannel->GetTrack();
    auto& channelView = ChannelView::Get(**pTrack->Channels().begin());
    if (const auto affordance
            =std::dynamic_pointer_cast<WaveTrackAffordanceControls>(
                  channelView.GetAffordanceControls()).get()
            ; affordance && (affordance->*pmf)(project)
        ) {
        return true;
    }
    return false;
}
}

bool WaveChannelView::CutSelectedText(AudacityProject& project)
{
    return
        AnyAffordance(project, *this, &WaveTrackAffordanceControls::OnTextCut);
}

bool WaveChannelView::CopySelectedText(AudacityProject& project)
{
    return
        AnyAffordance(project, *this, &WaveTrackAffordanceControls::OnTextCopy);
}

bool WaveChannelView::ClipDetailsVisible(const ClipTimes& clip,
                                         const ZoomInfo& zoomInfo, const wxRect& viewRect)
{
    //Do not fold clips to line at sample zoom level, as
    //it may become impossible to 'unfold' it when clip is trimmed
    //to a single sample
    bool showSamples{ false };
    auto clipRect = ClipParameters::GetClipRect(clip, zoomInfo, viewRect, &showSamples);
    return showSamples || clipRect.width >= kClipDetailedViewMinimumWidth;
}

wxRect WaveChannelView::ClipHitTestArea(const ClipTimes& clip,
                                        const ZoomInfo& zoomInfo, const wxRect& viewRect)
{
    bool showSamples{ false };
    auto clipRect = ClipParameters::GetClipRect(clip, zoomInfo, viewRect, &showSamples);
    if (showSamples || clipRect.width >= kClipDetailedViewMinimumWidth) {
        return clipRect;
    }

    return clipRect.Inflate(2, 0);
}

bool WaveChannelView::HitTest(const ClipTimes& clip,
                              const ZoomInfo& viewInfo, const wxRect& viewRect, const wxPoint& pos)
{
    return ClipHitTestArea(clip, viewInfo, viewRect).Contains(pos);
}

bool WaveChannelView::PasteText(AudacityProject& project)
{
    return
        AnyAffordance(project, *this, &WaveTrackAffordanceControls::OnTextPaste);
}

bool WaveChannelView::SelectAllText(AudacityProject& project)
{
    return
        AnyAffordance(project, *this, &WaveTrackAffordanceControls::OnTextSelect);
}

std::vector<std::shared_ptr<WaveChannelSubView> >
WaveChannelView::GetAllSubViews()
{
    BuildSubViews();

    std::vector<std::shared_ptr<WaveChannelSubView> > results;
    WaveChannelSubViews::ForEach([&](WaveChannelSubView& subView) {
        results.push_back(std::static_pointer_cast<WaveChannelSubView>(
                              subView.shared_from_this()));
    });
    return results;
}

std::shared_ptr<CommonTrackCell> WaveChannelView::GetAffordanceControls()
{
    auto pChannel = FindWaveChannel();
    if (pChannel) {
        auto& track = pChannel->GetTrack();
        if (pChannel == *track.Channels().begin()) {
            return DoGetAffordance(track);
        }
    }
    return {};
}

void WaveChannelView::DoSetMinimized(bool minimized)
{
    BuildSubViews();

    // May come here.  Invoke also on sub-views.
    ChannelView::DoSetMinimized(minimized);
    WaveChannelSubViews::ForEach([minimized](WaveChannelSubView& subView) {
        subView.DoSetMinimized(minimized);
    });
}

std::shared_ptr<CommonTrackCell>
WaveChannelView::DoGetAffordance(Track& track)
{
    if (mpAffordanceCellControl == nullptr) {
        mpAffordanceCellControl
            =std::make_shared<WaveTrackAffordanceControls>(track.SharedPointer());
    }
    return mpAffordanceCellControl;
}

using DoGetWaveChannelView = DoGetView::Override<WaveTrack>;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(DoGetWaveChannelView) {
    return [](WaveTrack& track, size_t iChannel) {
        auto channels = track.Channels();
        assert(iChannel < channels.size());
        auto& iter = channels.first;
        std::advance(iter, iChannel);
        return std::make_shared<WaveChannelView>(*iter);
    };
}

std::shared_ptr<ChannelVRulerControls> WaveChannelView::DoGetVRulerControls()
{
    return {};
}

void WaveChannelView::Reparent(
    const std::shared_ptr<Track>& parent, size_t iChannel)
{
    // BuildSubViews(); // not really needed
    CommonChannelView::Reparent(parent, iChannel);
    WaveChannelSubViews::ForEach([&parent, iChannel](WaveChannelSubView& subView)
    {
        subView.Reparent(parent, iChannel);
    });
    if (mpAffordanceCellControl) {
        mpAffordanceCellControl->Reparent(parent);
    }
}

WaveTrack::IntervalHolder WaveChannelView::GetSelectedClip()
{
    const auto pChannel = FindWaveChannel();
    if (!pChannel) {
        return {}
    }
    auto& track = pChannel->GetTrack();
    auto& topmostView = Get(**track.Channels().begin());
    if (auto affordance = std::dynamic_pointer_cast<WaveTrackAffordanceControls>(
            topmostView.GetAffordanceControls())) {
        return *affordance->GetSelectedInterval();
    }
    return {};
}

void WaveChannelView::BuildSubViews() const
{
    if (WaveChannelSubViews::size() == 0) {
        // On-demand steps that can't happen in the constructor
        auto pThis = const_cast<WaveChannelView*>(this);
        pThis->BuildAll();
        bool minimized = GetMinimized();
        pThis->WaveChannelSubViews::ForEach([&](WaveChannelSubView& subView){
            subView.DoSetMinimized(minimized);
        });

        auto& placements = pThis->DoGetPlacements();
        if (placements.empty()) {
            placements.resize(WaveChannelSubViews::size());

            auto display = TracksPrefs::ViewModeChoice();
            bool multi = (display == WaveChannelViewConstants::MultiView);
            if (multi) {
                pThis->SetMultiView(true);
                display = WaveChannelSubViewType::Default();
            }

            pThis->DoSetDisplay(display, !multi);
        }
    }
}

void WaveChannelView::Draw(
    TrackPanelDrawingContext& context,
    const wxRect& rect, unsigned iPass)
{
    // Should not come here, drawing is now delegated to sub-views
    wxASSERT(false);

    CommonChannelView::Draw(context, rect, iPass);
}

using GetWaveTrackSyncLockPolicy
    =GetSyncLockPolicy::Override< const WaveTrack >;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(GetWaveTrackSyncLockPolicy) {
    return [](auto&) { return SyncLockPolicy::Grouped; };
}
