/**********************************************************************

Audacity: A Digital Audio Editor

Viewport.h

Paul Licameli split from ProjectWindow.h

**********************************************************************/

#ifndef __AUDACITY_VIEWPORT__
#define __AUDACITY_VIEWPORT__

#include "ClientData.h"
#include "Observer.h"

class AudacityProject;
class Track;
class TrackList;

//! A callback facade hiding GUI toolkit details
class VIEWPORT_API ViewportCallbacks
{
public:
    virtual ~ViewportCallbacks();

    //! Width and height in pixels of proper viewport area (excluding scrollbars)
    virtual std::pair<int, int> ViewportSize() const = 0;

    virtual unsigned MinimumTrackHeight() = 0;
    virtual bool IsTrackMinimized(const Track& track) = 0;
    virtual void SetMinimized(Track& track, bool minimized) = 0;
    virtual int GetTrackHeight(const Track& track) = 0;
    virtual void SetChannelHeights(Track& track, unsigned height) = 0;
    virtual int GetTotalHeight(const TrackList& trackList) = 0;

    virtual int GetHorizontalThumbPosition() const = 0;
    virtual int GetHorizontalThumbSize() const = 0;
    virtual int GetHorizontalRange() const = 0;
    virtual void SetHorizontalThumbPosition(int viewStart) = 0;
    virtual void SetHorizontalScrollbar(int position, int thumbSize, int range, int pageSize, bool refresh) = 0;
    virtual void ShowHorizontalScrollbar(bool shown) = 0;

    virtual int GetVerticalThumbPosition() const = 0;
    virtual int GetVerticalThumbSize() const = 0;
    virtual int GetVerticalRange() const = 0;
    virtual void SetVerticalThumbPosition(int viewStart) = 0;
    virtual void SetVerticalScrollbar(int position, int thumbSize, int range, int pageSize, bool refresh) = 0;
    virtual void ShowVerticalScrollbar(bool shown) = 0;

    virtual void SetToDefaultSize() = 0;
};

struct ViewportMessage {
    const bool rescroll;
    const bool scrollbarVisibilityChanged;
    const bool resize;
};

class VIEWPORT_API Viewport final : public Observer::Publisher<ViewportMessage>, public ClientData::Base,
    public std::enable_shared_from_this<Viewport>
{
public:
    static Viewport& Get(AudacityProject& project);
    static const Viewport& Get(const AudacityProject& project);

    explicit Viewport(AudacityProject& project);
    void SetCallbacks(std::unique_ptr<ViewportCallbacks> pCallbacks);

    double ScrollingLowerBoundTime() const;

    //! Cause refresh of viewport contents after setting scrolling or zooming
    void DoScroll();

    /*!
     This method 'rewinds' the track, by setting the cursor to 0 and
     scrolling the window to fit 0 on the left side of it
     (maintaining current zoom).
     If extend is true, it will extend the left edge of the
     selection to 0 (holding right edge constant), otherwise it will
     move both left and right edge of selection to 0
     */
    void ScrollToStart(bool extend);

    /*!
     This method 'fast-forwards' the track, by setting the cursor to
     the end of the samples on the selected track and scrolling the
     window to fit the end on its right side (maintaining current zoom).
     If extend is true, it will extend the right edge of the
     selection to the end (holding left edge constant), otherwise it will
     move both left and right edge of selection to the end
     */
    void ScrollToEnd(bool extend);

    void ScrollToTop();
    void ScrollToBottom();

    //! Center view horizontally at the given time, if it was not in bounds
    void ScrollIntoView(double pos);

    //! Center the view horizontally at the given pixel position relative to
    //! the left edge, if it was not in bounds
    void ScrollIntoView(int x);

    /// This method handles general left-scrolling, either for drag-scrolling
    /// or when the scrollbar is clicked to the left of the thumb
    void OnScrollLeft();

    /// This method handles general right-scrolling, either for drag-scrolling
    /// or when the scrollbar is clicked to the right of the thumb
    void OnScrollRight();

    ///  This handles the event when the left direction button on the scrollbar
    ///  is depressed
    void OnScrollLeftButton();

    ///  This handles  the event when the right direction button on the scrollbar
    ///  is depressed
    void OnScrollRightButton();

    void OnScroll();

    // Scroll vertically. A positive argument makes the window
    // scroll down, while a negative argument scrolls up.
    bool ScrollUpDown(int delta);

    void SetHorizontalThumb(double scrollto, bool doScroll = true);

    //! Set timeline magnification; unchanged left edge time
    void Zoom(double pixelsPerSecond);

    //! Multiply the magnification; unchanged left edge time
    void ZoomBy(double multiplier);

    //! Multiply timeline magnification, conserving a selected portion that
    //! incompletely fills the width, if possible; else like ZoomAboutCenter
    void ZoomAboutSelection(double multiplier);

    //! Multiply timeline magnification, conserving the midpoint time if possible
    void ZoomAboutCenter(double multiplier);

    //! Fit horizontally; scroll vertically so that the given track (or if that's
    //! null, the first selected track, or if none such, the first track) is
    //! visible
    void ZoomFitHorizontallyAndShowTrack(Track* pTrack);

    void ShowTrack(const Track& track);

    //! Find pixels-per-second that would fit all tracks on the timeline
    double GetZoomOfToFit() const;

    /// Set horizontal zoom according to the extents of the tracks, and scroll
    /// to the start
    void ZoomFitHorizontally();

    /// Give uncollapsed audio tracks equal height, fitting into the view if
    /// possible, and scroll to the top
    void ZoomFitVertically();

    void ExpandAllTracks();
    void CollapseAllTracks();

    //! Change scrollbar bounds in response to changes in the TrackList,
    //! and sometimes rescroll to the top or left and repaint the whole view
    void UpdateScrollbarsForTracks();

    void HandleResize();

    void ReinitScrollbars() { mbInitializingScrollbar = true; }

    void Redraw();

    //! Send a message to the main window PARENT of the viewport, to resize
    void SetToDefaultSize();

private:
    // How many pixels are covered by the period from lowermost scrollable time, to the given time:
    // PRL: Bug1197: we seem to need to compute all in double, to avoid differing results on Mac
    double PixelWidthBeforeTime(double scrollto) const;

    void FinishAutoScroll();

    void OnUndoPushedModified();
    void OnUndoRedo();
    void OnUndoReset();

    AudacityProject& mProject;
    std::unique_ptr<ViewportCallbacks> mpCallbacks{};

    const Observer::Subscription
        mSnappingChangedSubscription,
        mUndoSubscription
    ;

    double total{ 1.0 };               // total width in secs

    // Current horizontal scroll bar positions, in pixels
    wxInt64 sbarH{ 0 };
    wxInt64 sbarScreen{ 1 };
    wxInt64 sbarTotal{ 1 };

    // Internal wxScrollbar positions are only int in range, so multiply
    // the above values with the following member to get the actual
    // scroll bar positions as reported by the horizontal wxScrollbar's members
    // i.e. units are scroll increments per pixel
    double sbarScale{ 1.0 };

    // Vertical scroll step
    int scrollStep{ 16 };

    bool mAutoScrolling{ false };
    bool mbInitializingScrollbar{ false };
};

#endif
