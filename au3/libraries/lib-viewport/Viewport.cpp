/**********************************************************************

Audacity: A Digital Audio Editor

Viewport.cpp

Paul Licameli split from ProjectWindow.cpp

**********************************************************************/
#include "Viewport.h"

#include "BasicUI.h"
#include "PendingTracks.h"
#include "PlayableTrack.h" // just for AudioTrack
#include "Project.h"
#include "ProjectSnap.h"
#include "TrackFocus.h"
#include "UndoManager.h"
#include "ViewInfo.h"

#include <numeric>

ViewportCallbacks::~ViewportCallbacks() = default;

static const AudacityProject::AttachedObjects::RegisteredFactory
    sKey{
    []( AudacityProject& project ){
        auto result = std::make_shared<Viewport>(project);
        return result;
    }
};

Viewport& Viewport::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get<Viewport>(sKey);
}

const Viewport& Viewport::Get(const AudacityProject& project)
{
    return Get(const_cast<AudacityProject&>(project));
}

Viewport::Viewport(AudacityProject& project)
    : mProject{project}
    , mSnappingChangedSubscription{
                                   ProjectSnap::Get(project).Subscribe([this](auto&){ Redraw(); })},
    mUndoSubscription{
    UndoManager::Get(project).Subscribe([this](UndoRedoMessage message){
        switch (message.type) {
            case UndoRedoMessage::Pushed:
            case UndoRedoMessage::Modified:
                return OnUndoPushedModified();
            case UndoRedoMessage::UndoOrRedo:
                return OnUndoRedo();
            case UndoRedoMessage::Reset:
                return OnUndoReset();
            default:
                return;
        }
    }) }
{
}

void Viewport::SetCallbacks(std::unique_ptr<ViewportCallbacks> pCallbacks)
{
    mpCallbacks = move(pCallbacks);
}

void Viewport::FinishAutoScroll()
{
    // Set a flag so we don't have to generate two update events
    mAutoScrolling = true;

    // Call our Scroll method which updates our ViewInfo variables
    // to reflect the positions of the scrollbars
    DoScroll();

    mAutoScrolling = false;
}

const int sbarHjump = 30;       //STM: This is how far the thumb jumps when the l/r buttons are pressed, or auto-scrolling occurs -- in pixels

// Make sure selection edge is in view
void Viewport::ScrollIntoView(double pos)
{
    auto& project = mProject;
    auto& viewInfo = ViewInfo::Get(project);
    auto w = viewInfo.GetTracksUsableWidth();

    int pixel = viewInfo.TimeToPosition(pos);
    if (pixel < 0 || pixel >= w) {
        SetHorizontalThumb(viewInfo.OffsetTimeByPixels(pos, -(w / 2)));
        Publish({ true, false, false });
    }
}

void Viewport::ScrollIntoView(int x)
{
    auto& project = mProject;
    auto& viewInfo = ViewInfo::Get(project);
    ScrollIntoView(viewInfo.PositionToTime(x, viewInfo.GetLeftOffset()));
}

void Viewport::OnScrollLeft()
{
    auto& project = mProject;
    wxInt64 pos = mpCallbacks ? mpCallbacks->GetHorizontalThumbPosition() : 0;
    const auto prevPos = pos;
    // move at least one scroll increment
    pos -= std::max<wxInt64>((sbarHjump * sbarScale), 1);
    pos = std::max<wxInt64>(pos, 0);
    sbarH -= sbarHjump;
    sbarH = std::max<wxInt64>(sbarH, -PixelWidthBeforeTime(0.0));

    if (mpCallbacks && pos != prevPos) {
        mpCallbacks->SetHorizontalThumbPosition(static_cast<int>(pos));
        FinishAutoScroll();
    }
}

void Viewport::OnScrollRight()
{
    auto& project = mProject;
    wxInt64 pos = mpCallbacks ? mpCallbacks->GetHorizontalThumbPosition() : 0;
    const auto prevPos = pos;
    // move at least one scroll increment
    // use wxInt64 for calculation to prevent temporary overflow
    pos += std::max<wxInt64>((sbarHjump * sbarScale), 1);
    wxInt64 max = mpCallbacks
                  ? mpCallbacks->GetHorizontalRange()
                  - mpCallbacks->GetHorizontalThumbSize()
                  : 0;
    pos = std::min(pos, max);
    sbarH += sbarHjump;
    sbarH = std::min<wxInt64>(sbarH,
                              sbarTotal - PixelWidthBeforeTime(0.0) - sbarScreen);

    if (mpCallbacks && pos != prevPos) {
        mpCallbacks->SetHorizontalThumbPosition(static_cast<int>(pos));
        FinishAutoScroll();
    }
}

void Viewport::OnScrollLeftButton()
{
    auto& project = mProject;
    wxInt64 pos = mpCallbacks ? mpCallbacks->GetHorizontalThumbPosition() : 0;
    const auto prevPos = pos;
    // move at least one scroll increment
    pos -= std::max<wxInt64>((sbarHjump * sbarScale), 1);
    pos = std::max<wxInt64>(pos, 0);
    sbarH -= sbarHjump;
    sbarH = std::max<wxInt64>(sbarH, -PixelWidthBeforeTime(0.0));

    if (mpCallbacks && pos != prevPos) {
        mpCallbacks->SetHorizontalThumbPosition(static_cast<int>(pos));
        DoScroll();
    }
}

void Viewport::OnScrollRightButton()
{
    auto& project = mProject;
    wxInt64 pos = mpCallbacks ? mpCallbacks->GetHorizontalThumbPosition() : 0;
    const auto prevPos = pos;
    // move at least one scroll increment
    // use wxInt64 for calculation to prevent temporary overflow
    pos += std::max<wxInt64>((sbarHjump * sbarScale), 1);
    wxInt64 max = mpCallbacks
                  ? mpCallbacks->GetHorizontalRange()
                  - mpCallbacks->GetHorizontalThumbSize()
                  : 0;
    pos = std::min(pos, max);
    sbarH += sbarHjump;
    sbarH = std::min<wxInt64>(sbarH,
                              sbarTotal - PixelWidthBeforeTime(0.0) - sbarScreen);

    if (mpCallbacks && pos != prevPos) {
        mpCallbacks->SetHorizontalThumbPosition(static_cast<int>(pos));
        DoScroll();
    }
}

double Viewport::ScrollingLowerBoundTime() const
{
    return 0;
}

// PRL: Bug1197: we seem to need to compute all in double, to avoid differing results on Mac
// That's why ViewInfo::TimeRangeToPixelWidth was defined, with some regret.
double Viewport::PixelWidthBeforeTime(double scrollto) const
{
    auto& project = mProject;
    auto& viewInfo = ViewInfo::Get(project);
    const double lowerBound = ScrollingLowerBoundTime();
    return
        // Ignoring fisheye is correct here
        viewInfo.TimeRangeToPixelWidth(scrollto - lowerBound);
}

void Viewport::SetHorizontalThumb(double scrollto, bool doScroll)
{
    if (!mpCallbacks) {
        return;
    }
    auto& project = mProject;
    const auto unscaled = PixelWidthBeforeTime(scrollto);
    const int max = std::max(
        0, mpCallbacks->GetHorizontalRange()
        - mpCallbacks->GetHorizontalThumbSize());
    const int pos = std::clamp<int>(floor(0.5 + unscaled * sbarScale), 0, max);
    mpCallbacks->SetHorizontalThumbPosition(pos);
    sbarH = floor(0.5 + unscaled - PixelWidthBeforeTime(0.0));
    sbarH = std::clamp<wxInt64>(
        sbarH, -PixelWidthBeforeTime(0.0),
        std::max(sbarTotal - PixelWidthBeforeTime(0.0) - sbarScreen, 0.));

    if (doScroll) {
        DoScroll();
    }
}

bool Viewport::ScrollUpDown(int delta)
{
    int oldPos = mpCallbacks ? mpCallbacks->GetVerticalThumbPosition() : 0;
    int pos = oldPos + delta;
    int max = mpCallbacks
              ? mpCallbacks->GetVerticalRange() - mpCallbacks->GetVerticalThumbSize()
              : 0;

    // Can be negative in case of only one track
    if (max < 0) {
        max = 0;
    }

    if (pos > max) {
        pos = max;
    } else if (pos < 0) {
        pos = 0;
    }

    if (pos != oldPos) {
        if (mpCallbacks) {
            mpCallbacks->SetVerticalThumbPosition(pos);
        }

        DoScroll();
        return true;
    } else {
        return false;
    }
}

void Viewport::UpdateScrollbarsForTracks()
{
    auto& project = mProject;
    auto& tracks = TrackList::Get(project);
    auto& viewInfo = ViewInfo::Get(project);

    // To decide whether to repaint the view
    bool refresh = false;
    bool rescroll = false;

    // Gather inputs
    const int totalHeight
        =(mpCallbacks ? mpCallbacks->GetTotalHeight(tracks) : 0) + 32;

    // (From Debian...at least I think this is the change corresponding
    // to this comment)
    //
    // (2.) GTK critical warning "IA__gtk_range_set_range: assertion
    // 'min < max' failed" because of negative numbers as result of window
    // size checking. Added a sanity check that straightens up the numbers
    // in edge cases.
    const auto panelWidth = std::max(0, viewInfo.GetTracksUsableWidth());
    const auto panelHeight = std::max(0, viewInfo.GetHeight());

    // Whether scrollbars are visible now
    const bool oldhstate = (viewInfo.GetScreenEndTime() - viewInfo.hpos) < total;
    const bool oldvstate = panelHeight < totalHeight;

    auto& pendingTracks = PendingTracks::Get(mProject);
    const auto LastTime = std::accumulate(tracks.begin(), tracks.end(),
                                          viewInfo.selectedRegion.t1(),
                                          [&pendingTracks](double acc, const Track* track){
        // Iterate over pending changed tracks if present.
        track = &pendingTracks.SubstitutePendingChangedTrack(*track);
        return std::max(acc, track->GetEndTime());
    });

    const double screen = viewInfo.GetScreenEndTime() - viewInfo.hpos;
    const double halfScreen = screen / 2.0;

    const double lowerBound = ScrollingLowerBoundTime();
    const double additional = screen / 4.0;

    total = LastTime + additional;

    // Don't remove time from total that's still on the screen
    total = std::max(total, viewInfo.hpos + screen);

    // Scroll the view later if needed to respect the lower bound
    if (viewInfo.hpos < lowerBound) {
        viewInfo.hpos = lowerBound;
        rescroll = true;
    }

    // To compute new horizontal scrollbar settings
    sbarTotal = static_cast<wxInt64>(total * viewInfo.GetZoom());
    sbarScreen = static_cast<wxInt64>(panelWidth);
    sbarH = static_cast<wxInt64>(viewInfo.GetBeforeScreenWidth());

    // PRL:  Can someone else find a more elegant solution to bug 812, than
    // introducing this boolean member variable?
    // Setting mVSbar earlier, in HandlXMLTag, didn't succeed in restoring
    // the vertical scrollbar to its saved position.  So defer that till now.
    // mbInitializingScrollbar should be true only at the start of the life
    // of an AudacityProject reopened from disk.
    if (!mbInitializingScrollbar) {
        viewInfo.vpos
            =(mpCallbacks ? mpCallbacks->GetVerticalThumbPosition() : 0)
              * scrollStep;
    }
    mbInitializingScrollbar = false;

    // Constrain new top of visible area
    viewInfo.vpos = std::clamp(viewInfo.vpos, 0, totalHeight - 1);

    // Decide whether the tracks are large enough to scroll for the zoom level
    // and heights
    bool newhstate
        =(viewInfo.GetScreenEndTime() - viewInfo.hpos) < total;
    bool newvstate = panelHeight < totalHeight;

    /* Leo: This has been broken forever (2.0.0/Win11)
       and causes the bars to not show on Lin since 3.2. #2937  */
    // Hide scrollbar thumbs and buttons if not scrollable
    // if (mpCallbacks) {
    //    mpCallbacks->ShowHorizontalScrollbar(newhstate);
    //    mpCallbacks->ShowVerticalScrollbar(newvstate);
    // }

    // When not scrollable in either axis, align viewport to top or left and
    // repaint it later
    if (!newvstate && viewInfo.vpos != 0) {
        viewInfo.vpos = 0;

        refresh = true;
        rescroll = false;
    }
    if (!newhstate && sbarH != 0) {
        sbarH = 0;

        refresh = true;
        rescroll = false;
    }

    // wxScrollbar only supports int values but we need a greater range, so
    // we scale the scrollbar coordinates on demand. We only do this if we
    // would exceed the int range, so we can always use the maximum resolution
    // available.

    // Don't use the full 2^31 max int range but a bit less, so rounding
    // errors in calculations do not overflow max int
    wxInt64 maxScrollbarRange = (wxInt64)(2147483647 * 0.999);
    if (sbarTotal > maxScrollbarRange) {
        sbarScale = (static_cast<double>(maxScrollbarRange)) / sbarTotal;
    } else {
        sbarScale = 1.0; // use maximum resolution
    }
    {
        auto scaledSbarH = static_cast<int>(sbarH * sbarScale);
        auto scaledSbarScreen = static_cast<int>(sbarScreen * sbarScale);
        auto scaledSbarTotal = static_cast<int>(sbarTotal * sbarScale);
        const auto offset
            =static_cast<int>(floor(0.5 + sbarScale * PixelWidthBeforeTime(0.0)));

        if (mpCallbacks) {
            mpCallbacks->SetHorizontalScrollbar(
                scaledSbarH + offset, scaledSbarScreen, scaledSbarTotal,
                scaledSbarScreen, true);
        }
    }

    if (mpCallbacks) {
        mpCallbacks->SetVerticalScrollbar(viewInfo.vpos / scrollStep,
                                          panelHeight / scrollStep,
                                          totalHeight / scrollStep,
                                          panelHeight / scrollStep, true);
    }

    //Leo: this needs to be rescroll = rescroll && (...
    //if scrollbar hiding is to be reimplemented.
    //Or maybe not. It's all broken anyway.  #2937
    rescroll = (viewInfo.GetScreenEndTime() - viewInfo.hpos) < total;
    Publish({ (refresh || rescroll),
              (oldhstate != newhstate || oldvstate != newvstate), false });
}

void Viewport::HandleResize()
{
    BasicUI::CallAfter([wthis = weak_from_this()]{
        if (auto This = wthis.lock()) {
            This->UpdateScrollbarsForTracks();
            This->Publish({ false, false, true });
        }
    });
}

void Viewport::OnScroll()
{
    auto& project = mProject;
    auto& viewInfo = ViewInfo::Get(project);
    const wxInt64 offset = PixelWidthBeforeTime(0.0);
    const auto pos = mpCallbacks ? mpCallbacks->GetHorizontalThumbPosition() : 0;
    sbarH = static_cast<wxInt64>(pos / sbarScale) - offset;
    DoScroll();

#ifndef __WXMAC__
    // Bug2179
    // This keeps the time ruler in sync with horizontal scrolling, without
    // making an undesirable compilation dependency of this source file on
    // the ruler
    BasicUI::Yield();
#endif
}

void Viewport::DoScroll()
{
    auto& project = mProject;
    auto& viewInfo = ViewInfo::Get(project);
    const double lowerBound = ScrollingLowerBoundTime();

    auto width = viewInfo.GetTracksUsableWidth();
    const auto zoom = viewInfo.GetZoom();
    viewInfo.hpos = std::clamp(sbarH / zoom, lowerBound, std::max(lowerBound, total - width / zoom));

    const auto pos = mpCallbacks ? mpCallbacks->GetVerticalThumbPosition() : 0;
    viewInfo.vpos = pos * scrollStep;

    //mchinen: do not always set this project to be the active one.
    //a project may autoscroll while playing in the background
    //I think this is okay since OnMouseEvent has one of these.
    //SetActiveProject(this);

    if (!mAutoScrolling) {
        Publish({ true, false, false });
    }
}

void Viewport::ZoomFitHorizontallyAndShowTrack(Track* pTrack)
{
    auto& project = mProject;
    auto& tracks = TrackList::Get(project);

    ZoomFitHorizontally();

    if (!pTrack) {
        pTrack = *tracks.Selected().begin();
    }
    if (!pTrack) {
        pTrack = *tracks.begin();
    }
    if (pTrack) {
        TrackFocus::Get(project).Set(pTrack, true);
        ShowTrack(*pTrack);
    }
}

void Viewport::ShowTrack(const Track& track)
{
    auto& viewInfo = ViewInfo::Get(mProject);

    int trackTop = 0;
    int trackHeight = 0;
    for (auto it : TrackList::Get(mProject)) {
        trackTop += trackHeight;
        trackHeight = mpCallbacks ? mpCallbacks->GetTrackHeight(*it) : 0;

        if (it == &track) {
            //We have found the track we want to ensure is visible.

            //Get the size of the trackpanel.
            const auto size
                =mpCallbacks ? mpCallbacks->ViewportSize() : std::pair{ 1, 1 };
            auto [width, height] = size;

            if (trackTop < viewInfo.vpos) {
                height = viewInfo.vpos - trackTop + scrollStep;
                height /= scrollStep;
                ScrollUpDown(-height);
            } else if (trackTop + trackHeight > viewInfo.vpos + height) {
                height = (trackTop + trackHeight) - (viewInfo.vpos + height);
                height = (height + scrollStep + 1) / scrollStep;
                ScrollUpDown(height);
            }

            break;
        }
    }

    Publish({ true, false, false });
}

// Utility function called by other zoom methods
void Viewport::Zoom(double pixelsPerSecond)
{
    auto& project = mProject;
    auto& viewInfo = ViewInfo::Get(project);
    viewInfo.SetZoom(pixelsPerSecond);
    UpdateScrollbarsForTracks();
    // See if we can center the selection on screen, and have it actually fit.
    // tOnLeft is the amount of time we would need before the selection left edge to center it.
    float t0 = viewInfo.selectedRegion.t0();
    float t1 = viewInfo.selectedRegion.t1();
    float tAvailable = viewInfo.GetScreenEndTime() - viewInfo.hpos;
    float tOnLeft = (tAvailable - t0 + t1) / 2.0;
    // Bug 1292 (Enh) is effectively a request to do this scrolling of  the selection into view.
    // If tOnLeft is positive, then we have room for the selection, so scroll to it.
    if (tOnLeft >= 0) {
        SetHorizontalThumb(t0 - tOnLeft);
    }
}

void Viewport::ZoomBy(double multiplier)
{
    auto& project = mProject;
    auto& viewInfo = ViewInfo::Get(project);
    viewInfo.ZoomBy(multiplier);
    UpdateScrollbarsForTracks();
}

void Viewport::ScrollToStart(bool extend)
{
    auto& project = mProject;
    auto& viewInfo = ViewInfo::Get(project);
    viewInfo.selectedRegion.setT0(0, false);
    if (!extend) {
        viewInfo.selectedRegion.setT1(0);
    }

    SetHorizontalThumb(0);
}

void Viewport::ScrollToTop()
{
    if (mpCallbacks) {
        mpCallbacks->SetVerticalThumbPosition(0);
    }
}

void Viewport::ScrollToEnd(bool extend)
{
    auto& project = mProject;
    auto& tracks = TrackList::Get(project);
    auto& viewInfo = ViewInfo::Get(project);
    double len = tracks.GetEndTime();

    viewInfo.selectedRegion.setT1(len, false);
    if (!extend) {
        viewInfo.selectedRegion.setT0(len);
    }

    // Make sure the end of the track is visible
    ScrollIntoView(len);
}

void Viewport::ScrollToBottom()
{
    auto& project = mProject;
    auto& tracks = TrackList::Get(project);
    auto& viewInfo = ViewInfo::Get(project);

    auto range = tracks.Any();
    int trackHeight = 0;
    const auto getHeight = [this](auto pTrack){
        return mpCallbacks ? mpCallbacks->GetTrackHeight(*pTrack) : 0;
    };
    if (!range.empty()) {
        trackHeight = getHeight(*range.rbegin());
        --range.second;
    }
    int trackTop
        =range.sum(getHeight);
    const auto size
        =mpCallbacks ? mpCallbacks->ViewportSize() : std::pair{ 1, 1 };
    const auto [width, height] = size;
    const auto step = scrollStep;
    const int delta = ((trackTop + trackHeight - height) - viewInfo.vpos
                       + step) / step;
    ScrollUpDown(delta);
    Publish({ true, false, false });
}

void Viewport::ZoomAboutSelection(double multiplier)
{
    auto& project = mProject;
    auto& viewInfo = ViewInfo::Get(project);

    // DMM: Here's my attempt to get logical zooming behavior
    // when there's a selection that's currently at least
    // partially on-screen

    const double endTime = viewInfo.GetScreenEndTime();
    const double duration = endTime - viewInfo.hpos;

    bool selectionIsOnscreen
        =(viewInfo.selectedRegion.t0() < endTime)
          && (viewInfo.selectedRegion.t1() >= viewInfo.hpos);

    bool selectionFillsScreen
        =(viewInfo.selectedRegion.t0() < viewInfo.hpos)
          && (viewInfo.selectedRegion.t1() > endTime);

    if (selectionIsOnscreen && !selectionFillsScreen) {
        // Start with the center of the selection
        double selCenter = (viewInfo.selectedRegion.t0()
                            + viewInfo.selectedRegion.t1()) / 2;

        // If the selection center is off-screen, pick the
        // center of the part that is on-screen.
        if (selCenter < viewInfo.hpos) {
            selCenter = viewInfo.hpos
                        + (viewInfo.selectedRegion.t1() - viewInfo.hpos) / 2;
        }
        if (selCenter > endTime) {
            selCenter = endTime
                        - (endTime - viewInfo.selectedRegion.t0()) / 2;
        }

        // Zoom in
        ZoomBy(multiplier);
        const double newDuration
            =viewInfo.GetScreenEndTime() - viewInfo.hpos;

        // Recenter on selCenter
        SetHorizontalThumb(selCenter - newDuration / 2);
        return;
    }

    double origLeft = viewInfo.hpos;
    double origWidth = duration;
    ZoomBy(multiplier);

    const double newDuration
        =viewInfo.GetScreenEndTime() - viewInfo.hpos;
    double newh = origLeft + (origWidth - newDuration) / 2;

    // MM: Commented this out because it was confusing users
    /*
    // make sure that the *right-hand* end of the selection is
    // no further *left* than 1/3 of the way across the screen
    if (viewInfo.selectedRegion.t1() < newh + viewInfo.screen / 3)
       newh = viewInfo.selectedRegion.t1() - viewInfo.screen / 3;

    // make sure that the *left-hand* end of the selection is
    // no further *right* than 2/3 of the way across the screen
    if (viewInfo.selectedRegion.t0() > newh + viewInfo.screen * 2 / 3)
       newh = viewInfo.selectedRegion.t0() - viewInfo.screen * 2 / 3;
    */

    SetHorizontalThumb(newh);
}

void Viewport::ZoomAboutCenter(double multiplier)
{
    auto& project = mProject;
    auto& viewInfo = ViewInfo::Get(project);

    //Zoom() may change these, so record original values:
    const double origLeft = viewInfo.hpos;
    const double origWidth = viewInfo.GetScreenEndTime() - origLeft;

    ZoomBy(multiplier);
    const double newWidth = viewInfo.GetScreenEndTime() - viewInfo.hpos;

    const double newh = origLeft + (origWidth - newWidth) / 2;
    // newh = (newh > 0) ? newh : 0;
    SetHorizontalThumb(newh);
}

double Viewport::GetZoomOfToFit() const
{
    auto& project = mProject;
    auto& tracks = TrackList::Get(project);
    auto& viewInfo = ViewInfo::Get(project);

    const double end = tracks.GetEndTime();
    const double start = 0;
    const double len = end - start;

    if (len <= 0.0) {
        return viewInfo.GetZoom();
    }

    auto w = viewInfo.GetTracksUsableWidth();
    w -= 10;
    return w / len;
}

void Viewport::ZoomFitHorizontally()
{
    auto& project = mProject;
    auto& viewInfo = ViewInfo::Get(project);
    auto& tracks = TrackList::Get(project);

    const double start = 0;

    Zoom(GetZoomOfToFit());
    SetHorizontalThumb(start);
}

void Viewport::ZoomFitVertically()
{
    if (!mpCallbacks) {
        return;
    }
    auto& project = mProject;
    auto& viewInfo = ViewInfo::Get(project);
    auto& tracks = TrackList::Get(project);

    // Only nonminimized audio tracks will be resized
    // Assume all channels of the track have the same minimization state
    auto range = tracks.Any<AudioTrack>()
                 - [this](const Track * pTrack) {
        return mpCallbacks->IsTrackMinimized(*pTrack);
    };
    auto count = static_cast<int>(range.sum(&Track::NChannels));
    if (count == 0) {
        return;
    }

    // Find total height to apportion
    auto height = viewInfo.GetHeight();
    height -= 28;

    // The height of minimized and non-audio tracks cannot be apportioned
    const auto fn = [this](const Track* pTrack){
        return mpCallbacks->GetTrackHeight(*pTrack);
    };
    height -= tracks.Any().sum(fn) - range.sum(fn);
    height /= count;
    height = std::max<int>(mpCallbacks->MinimumTrackHeight(), height);
    for (auto t : range) {
        mpCallbacks->SetChannelHeights(*t, height);
    }

    ScrollToTop();
}

void Viewport::ExpandAllTracks()
{
    if (!mpCallbacks) {
        return;
    }
    auto& project = mProject;
    auto& tracks = TrackList::Get(project);
    for (auto t : tracks) {
        mpCallbacks->SetMinimized(*t, false);
    }
}

void Viewport::CollapseAllTracks()
{
    if (!mpCallbacks) {
        return;
    }
    auto& project = mProject;
    auto& tracks = TrackList::Get(project);
    for (auto t : tracks) {
        mpCallbacks->SetMinimized(*t, true);
    }
}

void Viewport::Redraw()
{
    // Delay it until after channel views update their Y coordinates in response
    // to TrackList mesages
    BasicUI::CallAfter([wthis = weak_from_this()]{
        if (auto This = wthis.lock()) {
            This->UpdateScrollbarsForTracks();
            This->Publish({ true, false, false });
        }
    });
}

void Viewport::SetToDefaultSize()
{
    if (mpCallbacks) {
        mpCallbacks->SetToDefaultSize();
    }
}

void Viewport::OnUndoPushedModified()
{
    Redraw();
}

void Viewport::OnUndoRedo()
{
    HandleResize();
    Redraw();
}

void Viewport::OnUndoReset()
{
    HandleResize();
    // Redraw();  // Should we do this here too?
}
