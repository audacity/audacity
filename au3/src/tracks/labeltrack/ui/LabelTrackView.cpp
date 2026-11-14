/**********************************************************************

Audacity: A Digital Audio Editor

LabelTrackView.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "LabelTrackView.h"

#include "LabelTrackVRulerControls.h"
#include "LabelGlyphHandle.h"
#include "LabelTextHandle.h"

#include "LabelTrack.h"

#include "AColor.h"
#include "../../../widgets/BasicMenu.h"
#include "AllThemeResources.h"
#include "../../../HitTestResult.h"
#include "PendingTracks.h"
#include "Project.h"
#include "ProjectHistory.h"
#include "ProjectNumericFormats.h"
#include "ProjectRate.h"
#include "../../../ProjectWindows.h"
#include "../../../RefreshCode.h"
#include "SyncLock.h"
#include "Theme.h"
#include "../../../TrackArt.h"
#include "../../../TrackArtist.h"
#include "TrackFocus.h"
#include "../../../TrackPanel.h"
#include "../../../TrackPanelMouseEvent.h"
#include "UndoManager.h"
#include "ViewInfo.h"
#include "Viewport.h"
#include "AudacityTextEntryDialog.h"
#include "wxWidgetsWindowPlacement.h"

#include <wx/clipbrd.h>
#include <wx/dcclient.h>
#include <wx/font.h>
#include <wx/frame.h>
#include <wx/menu.h>

LabelTrackView::Index::Index()
    :  mIndex(-1),
    mModified(false)
{
}

LabelTrackView::Index::Index(int index)
    :  mIndex(index),
    mModified(false)
{
}

LabelTrackView::Index& LabelTrackView::Index::operator =(int index)
{
    if (index != mIndex) {
        mModified = false;
    }
    mIndex = index;
    return *this;
}

LabelTrackView::Index& LabelTrackView::Index::operator ++()
{
    mModified = false;
    mIndex += 1;
    return *this;
}

LabelTrackView::Index& LabelTrackView::Index::operator --()
{
    mModified = false;
    mIndex -= 1;
    return *this;
}

LabelTrackView::Index::operator int() const
{
    return mIndex;
}

bool LabelTrackView::Index::IsModified() const
{
    return mModified;
}

void LabelTrackView::Index::SetModified(bool modified)
{
    mModified = modified;
}

LabelTrackView::LabelTrackView(const std::shared_ptr<Channel>& pChannel)
    : CommonChannelView{pChannel}
{
    ResetFont();
    CreateCustomGlyphs();
    ResetFlags();

    // Events will be emitted by the track
    const auto pLabelTrack = FindLabelTrack();
    BindTo(pLabelTrack.get());
}

LabelTrackView::~LabelTrackView()
{
}

void LabelTrackView::Reparent(
    const std::shared_ptr<Track>& parent, size_t iChannel)
{
    assert(iChannel == 0);
    auto oldParent = FindLabelTrack();
    auto newParent = track_cast<LabelTrack*>(parent.get());
    if (oldParent.get() != newParent) {
        BindTo(newParent);
    }
    CommonChannelView::Reparent(parent, iChannel);
}

void LabelTrackView::BindTo(LabelTrack* pParent)
{
    // Destroys any previous subscription to another track
    mSubscription = pParent->Subscribe([this](const LabelTrackEvent& e){
        switch (e.type) {
            case LabelTrackEvent::Addition:
                return OnLabelAdded(e);
            case LabelTrackEvent::Deletion:
                return OnLabelDeleted(e);
            case LabelTrackEvent::Permutation:
                return OnLabelPermuted(e);
            case LabelTrackEvent::Selection:
                return OnSelectionChange(e);
            default:
                return;
        }
    });
}

void LabelTrackView::CopyTo(Track& track, size_t iChannel) const
{
    assert(iChannel == 0);
    ChannelView::CopyTo(track, iChannel);
    auto& other = ChannelView::Get(*track.GetChannel(0));
    if (const auto pOther = dynamic_cast<const LabelTrackView*>(&other)) {
        pOther->mNavigationIndex = mNavigationIndex;
        pOther->mInitialCursorPos = mInitialCursorPos;
        pOther->mCurrentCursorPos = mCurrentCursorPos;
        pOther->mTextEditIndex = mTextEditIndex;
        pOther->mUndoLabel = mUndoLabel;
    }
}

LabelTrackView& LabelTrackView::Get(LabelTrack& track)
{
    return static_cast<LabelTrackView&>(ChannelView::Get(track));
}

const LabelTrackView& LabelTrackView::Get(const LabelTrack& track)
{
    return static_cast<const LabelTrackView&>(ChannelView::Get(track));
}

std::shared_ptr<LabelTrack> LabelTrackView::FindLabelTrack()
{
    return FindChannel<LabelTrack>();
}

std::shared_ptr<const LabelTrack> LabelTrackView::FindLabelTrack() const
{
    return const_cast<LabelTrackView*>(this)->FindLabelTrack();
}

std::vector<UIHandlePtr> LabelTrackView::DetailedHitTest
    (const TrackPanelMouseState& st,
    const AudacityProject* WXUNUSED(pProject), int, bool)
{
    UIHandlePtr result;
    std::vector<UIHandlePtr> results;
    const wxMouseState& state = st.state;

    const auto pTrack = FindLabelTrack();
    result = LabelGlyphHandle::HitTest(
        mGlyphHandle, state, pTrack, st.rect);
    if (result) {
        results.push_back(result);
    }

    result = LabelTextHandle::HitTest(
        mTextHandle, state, pTrack);
    if (result) {
        results.push_back(result);
    }

    return results;
}

// static member variables.
bool LabelTrackView::mbGlyphsReady=false;

wxFont LabelTrackView::msFont;

/// We have several variants of the icons (highlighting).
/// The icons are draggable, and you can drag one boundary
/// or all boundaries at the same timecode depending on whether you
/// click the centre (for all) or the arrow part (for one).
/// Currently we have twelve variants but we're only using six.
wxBitmap LabelTrackView::mBoundaryGlyphs[ NUM_GLYPH_CONFIGS * NUM_GLYPH_HIGHLIGHTS ];
int LabelTrackView::mIconHeight;
int LabelTrackView::mIconWidth;
int LabelTrackView::mTextHeight;

int LabelTrackView::mFontHeight=-1;

void LabelTrackView::ResetFlags()
{
    mInitialCursorPos = 1;
    mCurrentCursorPos = 1;
    mTextEditIndex = -1;
    mNavigationIndex = -1;
}

LabelTrackView::Flags LabelTrackView::SaveFlags() const
{
    return {
        mInitialCursorPos, mCurrentCursorPos, mNavigationIndex,
        mTextEditIndex, mUndoLabel
    };
}

void LabelTrackView::RestoreFlags(const Flags& flags)
{
    mInitialCursorPos = flags.mInitialCursorPos;
    mCurrentCursorPos = flags.mCurrentCursorPos;
    mNavigationIndex = flags.mNavigationIndex;
    mTextEditIndex = flags.mTextEditIndex;
}

wxFont LabelTrackView::GetFont(const wxString& faceName, int size)
{
    wxFontEncoding encoding;
    if (faceName.empty()) {
        encoding = wxFONTENCODING_DEFAULT;
    } else {
        encoding = wxFONTENCODING_SYSTEM;
    }

    auto fontInfo = size == 0 ? wxFontInfo() : wxFontInfo(size);
    fontInfo
    .Encoding(encoding)
    .FaceName(faceName);

    return wxFont(fontInfo);
}

void LabelTrackView::ResetFont()
{
    mFontHeight = -1;
    wxString facename = gPrefs->Read(wxT("/GUI/LabelFontFacename"), wxT(""));
    int size = gPrefs->Read(wxT("/GUI/LabelFontSize"), static_cast<int>(DefaultFontSize));
    msFont = GetFont(facename, size);
}

/// ComputeTextPosition is 'smart' about where to display
/// the label text.
///
/// The text must be displayed between its endpoints x and x1
/// We'd also like it centered between them, and we'd like it on
/// screen.  It isn't always possible to achieve all of this,
/// so we do the best we can.
///
/// This function has a number of tests and adjustments to the
/// text start position.  The tests later in the function will
/// take priority over the ones earlier, so because centering
/// is the first thing we do, it's the first thing we lose if
/// we can't do everything we want to.
void LabelTrackView::ComputeTextPosition(const wxRect& r, int index) const
{
    const auto pTrack = FindLabelTrack();
    const auto& mLabels = pTrack->GetLabels();

    const auto& labelStruct = mLabels[index];

    // xExtra is extra space
    // between the text and the endpoints.
    const int xExtra=mIconWidth;
    int x     = labelStruct.x; // left endpoint
    int x1    = labelStruct.x1;// right endpoint.
    int width = labelStruct.width;

    int xText; // This is where the text will end up.

    // Will the text all fit at this zoom?
    bool bTooWideForScreen = width > (r.width - 2 * xExtra);
// bool bSimpleCentering = !bTooWideForScreen;
    bool bSimpleCentering = false;

    //TODO (possibly):
    // Add configurable options as to when to use simple
    // and when complex centering.
    //
    // Simple centering does its best to keep the text
    // centered between the label limits.
    //
    // Complex centering positions the text proportionally
    // to how far we are through the label.
    //
    // If we add preferences for this, we want to be able to
    // choose separately whether:
    //   a) Wide text labels centered simple/complex.
    //   b) Other text labels centered simple/complex.
    //

    if (bSimpleCentering) {
        // Center text between the two end points.
        xText = (x + x1 - width) / 2;
    } else {
        // Calculate xText position to make text line
        // scroll sideways evenly as r moves right.

        // xText is a linear function of r.x.
        // These variables are used to compute that function.
        int rx0, rx1, xText0, xText1;

        // Since we will be using a linear function,
        // we should blend smoothly between left and right
        // aligned text as r, the 'viewport' moves.
        if (bTooWideForScreen) {
            rx0=x;        // when viewport at label start.
            xText0=x + xExtra; // text aligned left.
            rx1=x1 - r.width; // when viewport end at label end
            xText1=x1 - (width + xExtra); // text aligned right.
        } else {
            // when label start + width + extra spacing at viewport end..
            rx0=x - r.width + width + 2 * xExtra;
            // ..text aligned left.
            xText0=x + xExtra;
            // when viewport start + width + extra spacing at label end..
            rx1=x1 - (width + 2 * xExtra);
            // ..text aligned right.
            xText1=x1 - (width + xExtra);
        }

        if (rx1 > rx0) { // Avoid divide by zero case.
            // Compute the blend between left and right aligned.

            // Don't use:
            //
            // xText = xText0 + ((xText1-xText0)*(r.x-rx0))/(rx1-rx0);
            //
            // The problem with the above is that it integer-oveflows at
            // high zoom.

            // Instead use:
            xText = xText0 + (int)((xText1 - xText0) * (((float)(r.x - rx0)) / (rx1 - rx0)));
        } else {
            // Avoid divide by zero by reverting to
            // simple centering.
            //
            // We could also fall into this case if x and x1
            // are swapped, in which case we'll end up
            // left aligned anyway because code later on
            // will catch that.
            xText = (x + x1 - width) / 2;
        }
    }

    // Is the text now appearing partly outside r?
    bool bOffLeft = xText < r.x + xExtra;
    bool bOffRight = xText > r.x + r.width - width - xExtra;

    // IF any part of the text is offscreen
    // THEN we may bring it back.
    if (bOffLeft == bOffRight) {
        //IF both sides on screen, THEN nothing to do.
        //IF both sides off screen THEN don't do
        //anything about it.
        //(because if we did, you'd never get to read
        //all the text by scrolling).
    } else if (bOffLeft != bTooWideForScreen) {
        // IF we're off on the left, OR we're
        // too wide for the screen and off on the right
        // (only) THEN align left.
        xText = r.x + xExtra;
    } else {
        // We're off on the right, OR we're
        // too wide and off on the left (only)
        // SO align right.
        xText =r.x + r.width - width - xExtra;
    }

    // But if we've taken the text out from its endpoints
    // we must move it back so that it's between the endpoints.

    // We test the left end point last because the
    // text might not even fit between the endpoints (at this
    // zoom factor), and in that case we'd like to position
    // the text at the left end point.
    if (xText > (x1 - width - xExtra)) {
        xText=(x1 - width - xExtra);
    }
    if (xText < x + xExtra) {
        xText=x + xExtra;
    }

    labelStruct.xText = xText;
}

/// ComputeLayout determines which row each label
/// should be placed on, and reserves space for it.
/// Function assumes that the labels are sorted.
void LabelTrackView::ComputeLayout(const wxRect& r, const ZoomInfo& zoomInfo) const
{
    int xUsed[MAX_NUM_ROWS];

    int iRow;
    // Rows are the 'same' height as icons or as the text,
    // whichever is taller.
    const int yRowHeight = wxMax(mTextHeight, mIconHeight) + 3;// pixels.
    // Extra space at end of rows.
    // We allow space for one half icon at the start and two
    // half icon widths for extra x for the text frame.
    // [we don't allow half a width space for the end icon since it is
    // allowed to be obscured by the text].
    const int xExtra= (3 * mIconWidth) / 2;

    const int nRows = wxMin((r.height / yRowHeight) + 1, MAX_NUM_ROWS);
    // Initially none of the rows have been used.
    // So set a value that is less than any valid value.
    {
        // Bug 502: With dragging left of zeros, labels can be in
        // negative space.  So set least possible value as starting point.
        const int xStart = INT_MIN;
        for (auto& x : xUsed) {
            x = xStart;
        }
    }
    int nRowsUsed=0;

    const auto pTrack = FindLabelTrack();
    const auto& mLabels = pTrack->GetLabels();

    {
        int i = -1;
        for (const auto& labelStruct : mLabels) {
            ++i;
            const int x = zoomInfo.TimeToPosition(labelStruct.getT0(), r.x);
            const int x1 = zoomInfo.TimeToPosition(labelStruct.getT1(), r.x);
            int y = r.y;

            labelStruct.x=x;
            labelStruct.x1=x1;
            labelStruct.y=-1;// -ve indicates nothing doing.
            iRow=0;
            // Our first preference is a row that ends where we start.
            // (This is to encourage merging of adjacent label boundaries).
            while ((iRow < nRowsUsed) && (xUsed[iRow] != x)) {
                iRow++;
            }

            // IF we didn't find one THEN
            // find any row that can take a span starting at x.
            if (iRow >= nRowsUsed) {
                iRow=0;
                while ((iRow < nRows) && (xUsed[iRow] > x)) {
                    iRow++;
                }
            }
            // IF we found such a row THEN record a valid position.
            if (iRow < nRows) {
                // Possibly update the number of rows actually used.
                if (iRow >= nRowsUsed) {
                    nRowsUsed=iRow + 1;
                }
                // Record the position for this label
                y= r.y + iRow * yRowHeight + (yRowHeight / 2) + 1;
                labelStruct.y=y;
                // On this row we have used up to max of end marker and width.
                // Plus also allow space to show the start icon and
                // some space for the text frame.
                xUsed[iRow]=x + labelStruct.width + xExtra;
                if (xUsed[iRow] < x1) {
                    xUsed[iRow]=x1;
                }
                ComputeTextPosition(r, i);
            }
        }
    }
}

/// Draw vertical lines that go exactly through the position
/// of the start or end of a label.
///   @param  dc the device context
///   @param  r  the LabelTrack rectangle.
void LabelTrackView::DrawLines(
    wxDC& dc, const LabelStruct& ls, const wxRect& r)
{
    auto& x = ls.x;
    auto& x1 = ls.x1;
    auto& y = ls.y;

    // Bug 2388 - Point label and range label can appear identical
    // If the start and end times are not actually the same, but they
    // would appear so when drawn as lines at current zoom, be sure to draw
    // two lines - i.e. displace the second line slightly.
    if (ls.getT0() != ls.getT1()) {
        if (x == x1) {
            x1++;
        }
    }

    // How far out from the centre line should the vertical lines
    // start, i.e. what is the y position of the icon?
    // We adjust this so that the line encroaches on the icon
    // slightly (there is white space in the design).
    const int yIconStart = y - (mIconHeight / 2) + 1 + (mTextHeight + 3) / 2;
    const int yIconEnd   = yIconStart + mIconHeight - 2;

    // If y is positive then it is the center line for the
    // Label.
    if (y >= 0) {
        if ((x >= r.x) && (x <= (r.x + r.width))) {
            // Draw line above and below left dragging widget.
            AColor::Line(dc, x, r.y,  x, yIconStart - 1);
            AColor::Line(dc, x, yIconEnd, x, r.y + r.height);
        }
        if ((x1 >= r.x) && (x1 <= (r.x + r.width))) {
            // Draw line above and below right dragging widget.
            AColor::Line(dc, x1, r.y,  x1, yIconStart - 1);
            AColor::Line(dc, x1, yIconEnd, x1, r.y + r.height);
        }
    } else {
        // Draw the line, even though the widget is off screen
        AColor::Line(dc, x, r.y,  x, r.y + r.height);
        AColor::Line(dc, x1, r.y,  x1, r.y + r.height);
    }
}

/// DrawGlyphs draws the wxIcons at the start and end of a label.
///   @param  dc the device context
///   @param  r  the LabelTrack rectangle.
void LabelTrackView::DrawGlyphs(
    wxDC& dc, const LabelStruct& ls, const wxRect& r,
    int GlyphLeft, int GlyphRight)
{
    auto& y = ls.y;

    const int xHalfWidth=mIconWidth / 2;
    const int yStart=y - mIconHeight / 2 + (mTextHeight + 3) / 2;

    // If y == -1, nothing to draw
    if (y == -1) {
        return;
    }

    auto& x = ls.x;
    auto& x1 = ls.x1;

    if ((x >= r.x) && (x <= (r.x + r.width))) {
        dc.DrawBitmap(GetGlyph(GlyphLeft), x - xHalfWidth, yStart, true);
    }
    // The extra test commented out here would suppress right hand markers
    // when they overlap the left hand marker (e.g. zoomed out) or to the left.
    if ((x1 >= r.x) && (x1 <= (r.x + r.width)) /*&& (x1>x+mIconWidth)*/) {
        dc.DrawBitmap(GetGlyph(GlyphRight), x1 - xHalfWidth, yStart, true);
    }
}

int LabelTrackView::GetTextFrameHeight()
{
    return mTextHeight + TextFramePadding * 2;
}

/// Draw the text of the label and also draw
/// a long thin rectangle for its full extent
/// from x to x1 and a rectangular frame
/// behind the text itself.
///   @param  dc the device context
///   @param  r  the LabelTrack rectangle.
void LabelTrackView::DrawText(wxDC& dc, const LabelStruct& ls, const wxRect& r)
{
    const int yFrameHeight = mTextHeight + TextFramePadding * 2;
    //If y is positive then it is the center line for the
    //text we are about to draw.
    //if it isn't, nothing to draw.

    auto& y = ls.y;
    if (y == -1) {
        return;
    }

    // Draw frame for the text...
    // We draw it half an icon width left of the text itself.
    {
        auto& xText = ls.xText;
        const int xStart=wxMax(r.x, xText - mIconWidth / 2);
        const int xEnd=wxMin(r.x + r.width, xText + ls.width + mIconWidth / 2);
        const int xWidth = xEnd - xStart;

        if ((xStart < (r.x + r.width)) && (xEnd > r.x) && (xWidth > 0)) {
            // Now draw the text itself.
            auto pos = y - LabelBarHeight - yFrameHeight + TextFrameYOffset
                       + (yFrameHeight - mFontHeight) / 2 + dc.GetFontMetrics().ascent;
            dc.DrawText(ls.title, xText, pos);
        }
    }
}

void LabelTrackView::DrawTextBox(
    wxDC& dc, const LabelStruct& ls, const wxRect& r)
{
    // In drawing the bar and the frame, we compute the clipping
    // to the viewport ourselves.  Under Win98 the GDI does its
    // calculations in 16 bit arithmetic, and so gets it completely
    // wrong at higher zooms where the bar can easily be
    // more than 65536 pixels wide.

    // Draw bar for label extent...
    // We don't quite draw from x to x1 because we allow
    // half an icon width at each end.
    const auto textFrameHeight = GetTextFrameHeight();
    auto& xText = ls.xText;
    const int xStart = wxMax(r.x, xText - mIconWidth / 2);
    const int xEnd = wxMin(r.x + r.width, xText + ls.width + mIconWidth / 2);
    const int xWidth = xEnd - xStart;

    if ((xStart < (r.x + r.width)) && (xEnd > r.x) && (xWidth > 0)) {
        wxRect frame(
            xStart, ls.y - (textFrameHeight + LabelBarHeight) / 2 + TextFrameYOffset,
            xWidth, textFrameHeight);
        dc.DrawRectangle(frame);
    }
}

void LabelTrackView::DrawBar(wxDC& dc, const LabelStruct& ls, const wxRect& r)
{
    //If y is positive then it is the center line for the
    //text we are about to draw.
    const int xBarShorten = mIconWidth + 4;
    auto& y = ls.y;
    if (y == -1) {
        return;
    }

    auto& x = ls.x;
    auto& x1 = ls.x1;
    const int xStart = wxMax(r.x, x + xBarShorten / 2);
    const int xEnd = wxMin(r.x + r.width, x1 - xBarShorten / 2);
    const int xWidth = xEnd - xStart;

    if ((xStart < (r.x + r.width)) && (xEnd > r.x) && (xWidth > 0)) {
        wxRect bar(xStart, y - (LabelBarHeight - GetTextFrameHeight()) / 2,
                   xWidth, LabelBarHeight);
        if (x1 > x + xBarShorten) {
            dc.DrawRectangle(bar);
        }
    }
}

/// Draws text-selected region within the label
void LabelTrackView::DrawHighlight(wxDC& dc, const LabelStruct& ls,
                                   int xPos1, int xPos2, int charHeight)
{
    const int yFrameHeight = mTextHeight + TextFramePadding * 2;

    dc.SetPen(*wxTRANSPARENT_PEN);
    wxBrush curBrush = dc.GetBrush();
    curBrush.SetColour(wxString(wxT("BLUE")));
    auto top = ls.y + TextFrameYOffset - (LabelBarHeight + yFrameHeight) / 2 + (yFrameHeight - charHeight) / 2;
    if (xPos1 < xPos2) {
        dc.DrawRectangle(xPos1 - 1, top, xPos2 - xPos1 + 1, charHeight);
    } else {
        dc.DrawRectangle(xPos2 - 1, top, xPos1 - xPos2 + 1, charHeight);
    }
}

namespace {
void getXPos(const LabelStruct& ls, wxDC& dc, int* xPos1, int cursorPos)
{
    *xPos1 = ls.xText;
    if (cursorPos > 0) {
        int partWidth;
        // Calculate the width of the substring and add it to Xpos
        dc.GetTextExtent(ls.title.Left(cursorPos), &partWidth, NULL);
        *xPos1 += partWidth;
    }
}
}

bool LabelTrackView::CalcCursorX(AudacityProject& project, int* x) const
{
    if (IsValidIndex(mTextEditIndex, project)) {
        wxMemoryDC dc;

        if (msFont.Ok()) {
            dc.SetFont(msFont);
        }

        const auto pTrack = FindLabelTrack();
        const auto& mLabels = pTrack->GetLabels();

        getXPos(mLabels[mTextEditIndex], dc, x, mCurrentCursorPos);
        *x += mIconWidth / 2;
        return true;
    }

    return false;
}

void LabelTrackView::CalcHighlightXs(int* x1, int* x2) const
{
    wxMemoryDC dc;

    if (msFont.Ok()) {
        dc.SetFont(msFont);
    }

    int pos1 = mInitialCursorPos, pos2 = mCurrentCursorPos;
    if (pos1 > pos2) {
        std::swap(pos1, pos2);
    }

    const auto pTrack = FindLabelTrack();
    const auto& mLabels = pTrack->GetLabels();
    const auto& labelStruct = mLabels[mTextEditIndex];

    // find the left X pos of highlighted area
    getXPos(labelStruct, dc, x1, pos1);
    // find the right X pos of highlighted area
    getXPos(labelStruct, dc, x2, pos2);
}

#include "LabelGlyphHandle.h"
namespace {
LabelTrackHit* findHit(TrackPanel* pPanel)
{
    if (!pPanel) {
        return nullptr;
    }

    // Fetch the highlighting state
    auto target = pPanel->Target();
    if (target) {
        auto handle = dynamic_cast<LabelGlyphHandle*>(target.get());
        if (handle) {
            return &*handle->mpHit;
        }
    }
    return nullptr;
}
}

#include "../../../TrackPanelDrawingContext.h"
#include "LabelTextHandle.h"

/// Draw calls other functions to draw the LabelTrack.
///   @param  dc the device context
///   @param  r  the LabelTrack rectangle.
void LabelTrackView::Draw(TrackPanelDrawingContext& context, const wxRect& r)
const
{
    auto& dc = context.dc;
    const auto artist = TrackArtist::Get(context);
    const auto& zoomInfo = *artist->pZoomInfo;
    const auto& pendingTracks = *artist->pPendingTracks;

    auto pHit = findHit(artist->parent);

    if (msFont.Ok()) {
        dc.SetFont(msFont);
    }

    if (mFontHeight == -1) {
        calculateFontHeight(dc);
    }

    if (!FindChannel()) {
        return;
    }
    const auto& track = static_cast<const LabelTrack&>(
        pendingTracks.SubstitutePendingChangedChannel(*FindChannel()));
    const auto& mLabels = track.GetLabels();

    TrackArt::DrawBackgroundWithSelection(context, r, track,
                                          AColor::labelSelectedBrush, AColor::labelUnselectedBrush,
                                          SyncLock::IsSelectedOrSyncLockSelected(track));

    wxCoord textWidth, textHeight;

    // Get the text widths.
    // TODO: Make more efficient by only re-computing when a
    // text label title changes.
    for (const auto& labelStruct : mLabels) {
        dc.GetTextExtent(labelStruct.title, &textWidth, &textHeight);
        labelStruct.width = textWidth;
    }

    // TODO: And this only needs to be done once, but we
    // do need the dc to do it.
    // We need to set mTextHeight to something sensible,
    // guarding against the case where there are no
    // labels or all are empty strings, which for example
    // happens with a NEW label track.
    mTextHeight = dc.GetFontMetrics().ascent + dc.GetFontMetrics().descent;
    const int yFrameHeight = mTextHeight + TextFramePadding * 2;

    ComputeLayout(r, zoomInfo);
    dc.SetTextForeground(theTheme.Colour(clrLabelTrackText));
    dc.SetBackgroundMode(wxTRANSPARENT);
    dc.SetBrush(AColor::labelTextNormalBrush);
    dc.SetPen(AColor::labelSurroundPen);
    int GlyphLeft;
    int GlyphRight;
    // Now we draw the various items in this order,
    // so that the correct things overpaint each other.

    // Draw vertical lines that show where the end positions are.
    for (const auto& labelStruct : mLabels) {
        DrawLines(dc, labelStruct, r);
    }

    // Draw the end glyphs.
    {
        int i = -1;
        for (const auto& labelStruct : mLabels) {
            ++i;
            GlyphLeft=0;
            GlyphRight=1;
            if (pHit && i == pHit->mMouseOverLabelLeft) {
                GlyphLeft = (pHit->mEdge & 4) ? 6 : 9;
            }
            if (pHit && i == pHit->mMouseOverLabelRight) {
                GlyphRight = (pHit->mEdge & 4) ? 7 : 4;
            }
            DrawGlyphs(dc, labelStruct, r, GlyphLeft, GlyphRight);
        }
    }

    auto& project = *artist->parent->GetProject();

    // Draw the label boxes.
    {
        int i = -1;
        for (const auto& labelStruct : mLabels) {
            ++i;
            bool highlight = false;

            dc.SetBrush(mNavigationIndex == i || (pHit && pHit->mMouseOverLabel == i)
                        ? AColor::labelTextEditBrush : AColor::labelTextNormalBrush);
            DrawBar(dc, labelStruct, r);

            bool selected = mTextEditIndex == i;

            if (selected) {
                dc.SetBrush(AColor::labelTextEditBrush);
            } else if (highlight) {
                dc.SetBrush(AColor::uglyBrush);
            } else {
                dc.SetBrush(AColor::labelTextNormalBrush);
            }
            DrawTextBox(dc, labelStruct, r);

            dc.SetBrush(AColor::labelTextNormalBrush);
        }
    }

    // Draw highlights
    if ((mInitialCursorPos != mCurrentCursorPos) && IsValidIndex(mTextEditIndex, project)) {
        int xpos1, xpos2;
        CalcHighlightXs(&xpos1, &xpos2);
        DrawHighlight(dc, mLabels[mTextEditIndex],
                      xpos1, xpos2, dc.GetFontMetrics().ascent + dc.GetFontMetrics().descent);
    }

    // Draw the text and the label boxes.
    {
        int i = -1;
        for (const auto& labelStruct : mLabels) {
            ++i;
            if (mTextEditIndex == i) {
                dc.SetBrush(AColor::labelTextEditBrush);
            }
            DrawText(dc, labelStruct, r);
            if (mTextEditIndex == i) {
                dc.SetBrush(AColor::labelTextNormalBrush);
            }
        }
    }

    // Draw the cursor, if there is one.
    if (mInitialCursorPos == mCurrentCursorPos && IsValidIndex(mTextEditIndex, project)) {
        const auto& labelStruct = mLabels[mTextEditIndex];
        int xPos = labelStruct.xText;

        if (mCurrentCursorPos > 0) {
            // Calculate the width of the substring and add it to Xpos
            int partWidth;
            dc.GetTextExtent(labelStruct.title.Left(mCurrentCursorPos), &partWidth, NULL);
            xPos += partWidth;
        }

        wxPen currentPen = dc.GetPen();
        const int CursorWidth=2;
        currentPen.SetWidth(CursorWidth);
        const auto top = labelStruct.y - (LabelBarHeight + yFrameHeight) / 2 + (yFrameHeight - mFontHeight) / 2 + TextFrameYOffset;
        AColor::Line(dc,
                     xPos - 1, top,
                     xPos - 1, top + mFontHeight);
        currentPen.SetWidth(1);
    }
}

void LabelTrackView::Draw(
    TrackPanelDrawingContext& context,
    const wxRect& rect, unsigned iPass)
{
    if (iPass == TrackArtist::PassTracks) {
        Draw(context, rect);
    }
    CommonChannelView::Draw(context, rect, iPass);
}

/// uses GetTextExtent to find the character position
/// corresponding to the x pixel position.
int LabelTrackView::FindCursorPosition(int labelIndex, wxCoord xPos)
{
    int result = -1;
    wxMemoryDC dc;
    if (msFont.Ok()) {
        dc.SetFont(msFont);
    }

    // A bool indicator to see if set the cursor position or not
    bool finished = false;
    int charIndex = 1;
    int partWidth;
    int oneWidth;
    double bound;
    wxString subString;

    const auto pTrack = FindLabelTrack();
    const auto& mLabels = pTrack->GetLabels();
    const auto& labelStruct = mLabels[labelIndex];
    const auto& title = labelStruct.title;
    const int length = title.length();
    while (!finished && (charIndex < length + 1))
    {
        int unichar = (int)title.at(charIndex - 1);
        if ((0xDC00 <= unichar) && (unichar <= 0xDFFF)) {
            charIndex++;
            continue;
        }
        subString = title.Left(charIndex);
        // Get the width of substring
        dc.GetTextExtent(subString, &partWidth, NULL);

        // Get the width of the last character
        dc.GetTextExtent(subString.Right(1), &oneWidth, NULL);
        bound = labelStruct.xText + partWidth - oneWidth * 0.5;

        if (xPos <= bound) {
            // Found
            result = charIndex - 1;
            finished = true;
        } else {
            // Advance
            charIndex++;
        }
    }
    if (!finished) {
        // Cursor should be in the last position
        result = length;
    }

    return result;
}

void LabelTrackView::SetCurrentCursorPosition(int pos)
{
    mCurrentCursorPos = pos;
}

void LabelTrackView::SetTextSelection(int labelIndex, int start, int end)
{
    mTextEditIndex = labelIndex;
    mInitialCursorPos = start;
    mCurrentCursorPos = end;
}

int LabelTrackView::GetTextEditIndex(AudacityProject& project) const
{
    if (IsValidIndex(mTextEditIndex, project)) {
        return mTextEditIndex;
    }
    return -1;
}

void LabelTrackView::ResetTextSelection()
{
    mTextEditIndex = -1;
    mCurrentCursorPos = 1;
    mInitialCursorPos = 1;
}

void LabelTrackView::SetNavigationIndex(int index)
{
    mNavigationIndex = index;
}

int LabelTrackView::GetNavigationIndex(AudacityProject& project) const
{
    if (IsValidIndex(mNavigationIndex, project)) {
        return mNavigationIndex;
    }
    return -1;
}

void LabelTrackView::calculateFontHeight(wxDC& dc)
{
    int charDescent;
    int charLeading;

    // Calculate the width of the substring and add it to Xpos
    dc.GetTextExtent(wxT("(Test String)|[yp]"), NULL, &mFontHeight, &charDescent, &charLeading);

    // The cursor will have height charHeight.  We don't include the descender as
    // part of the height because for phonetic fonts this leads to cursors which are
    // too tall.  We don't include leading either - it is usually 0.
    // To make up for ignoring the descender height, we add one pixel above and below
    // using CursorExtraHeight so that the cursor is just a little taller than the
    // body of the characters.
    const int CursorExtraHeight=2;
    mFontHeight += CursorExtraHeight - (charLeading + charDescent);
}

bool LabelTrackView::IsValidIndex(const Index& index, AudacityProject& project) const
{
    if (index == -1) {
        return false;
    }
    // may make delayed update of mutable mSelIndex after track selection change
    auto track = FindLabelTrack();
    if (track->GetSelected() || (TrackFocus::Get(project).Get() == track.get())) {
        return index >= 0 && index < static_cast<int>(track->GetLabels().size());
    }
    return false;
}

bool LabelTrackView::IsTextSelected(AudacityProject& project) const
{
    return mCurrentCursorPos != mInitialCursorPos && IsValidIndex(mTextEditIndex, project);
}

/// Cut the selected text in the text box
///  @return true if text is selected in text box, false otherwise
bool LabelTrackView::CutSelectedText(AudacityProject& project)
{
    if (!IsTextSelected(project)) {
        return false;
    }

    const auto pTrack = FindLabelTrack();
    const auto& mLabels = pTrack->GetLabels();

    wxString left, right;
    auto labelStruct = mLabels[mTextEditIndex];
    auto& text = labelStruct.title;

    if (!mTextEditIndex.IsModified()) {
        mUndoLabel = text;
    }

    int init = mInitialCursorPos;
    int cur = mCurrentCursorPos;
    if (init > cur) {
        std::swap(init, cur);
    }

    // data for cutting
    wxString data = text.Mid(init, cur - init);

    // get left-remaining text
    if (init > 0) {
        left = text.Left(init);
    }

    // get right-remaining text
    if (cur < (int)text.length()) {
        right = text.Mid(cur);
    }

    // set title to the combination of the two remainders
    text = left + right;

    pTrack->SetLabel(mTextEditIndex, labelStruct);

    // copy data onto clipboard
    if (wxTheClipboard->Open()) {
        // Clipboard owns the data you give it
        wxTheClipboard->SetData(safenew wxTextDataObject(data));
        wxTheClipboard->Close();
    }

    // set cursor positions
    mInitialCursorPos = mCurrentCursorPos = left.length();

    mTextEditIndex.SetModified(true);
    return true;
}

/// Copy the selected text in the text box
///  @return true if text is selected in text box, false otherwise
bool LabelTrackView::CopySelectedText(AudacityProject& project)
{
    if (!IsTextSelected(project)) {
        return false;
    }

    const auto pTrack = FindLabelTrack();
    const auto& mLabels = pTrack->GetLabels();

    const auto& labelStruct = mLabels[mTextEditIndex];

    int init = mInitialCursorPos;
    int cur = mCurrentCursorPos;
    if (init > cur) {
        std::swap(init, cur);
    }

    if (init == cur) {
        return false;
    }

    // data for copying
    wxString data = labelStruct.title.Mid(init, cur - init);

    // copy the data on clipboard
    if (wxTheClipboard->Open()) {
        // Clipboard owns the data you give it
        wxTheClipboard->SetData(safenew wxTextDataObject(data));
        wxTheClipboard->Close();
    }

    return true;
}

// PRL:  should this set other fields of the label selection?
/// Paste the text on the clipboard to text box
///  @return true if mouse is clicked in text box, false otherwise
bool LabelTrackView::PasteSelectedText(
    AudacityProject& project, double sel0, double sel1)
{
    const auto pTrack = FindLabelTrack();

    if (!IsValidIndex(mTextEditIndex, project)) {
        SetTextSelection(AddLabel(SelectedRegion(sel0, sel1)));
    }

    wxString text, left, right;

    // if text data is available
    if (IsTextClipSupported()) {
        if (wxTheClipboard->Open()) {
            wxTextDataObject data;
            wxTheClipboard->GetData(data);
            wxTheClipboard->Close();
            text = data.GetText();
        }

        if (!mTextEditIndex.IsModified()) {
            mUndoLabel = text;
        }

        // Convert control characters to blanks
        for (int i = 0; i < (int)text.length(); i++) {
            if (wxIscntrl(text[i])) {
                text[i] = wxT(' ');
            }
        }
    }

    const auto& mLabels = pTrack->GetLabels();
    auto labelStruct = mLabels[mTextEditIndex];
    auto& title = labelStruct.title;
    int cur = mCurrentCursorPos, init = mInitialCursorPos;
    if (init > cur) {
        std::swap(init, cur);
    }
    left = title.Left(init);
    if (cur < (int)title.length()) {
        right = title.Mid(cur);
    }

    title = left + text + right;

    pTrack->SetLabel(mTextEditIndex, labelStruct);

    mInitialCursorPos =  mCurrentCursorPos = left.length() + text.length();

    mTextEditIndex.SetModified(true);
    return true;
}

bool LabelTrackView::SelectAllText(AudacityProject& project)
{
    if (!IsValidIndex(mTextEditIndex, project)) {
        return false;
    }

    const auto pTrack = FindLabelTrack();

    const auto& mLabels = pTrack->GetLabels();
    auto labelStruct = mLabels[mTextEditIndex];
    auto& title = labelStruct.title;

    mInitialCursorPos = 0;
    mCurrentCursorPos = title.Length();

    return true;
}

/// @return true if the text data is available in the clipboard, false otherwise
bool LabelTrackView::IsTextClipSupported()
{
    return wxTheClipboard->IsSupported(wxDF_UNICODETEXT);
}

/// TODO: Investigate what happens with large
/// numbers of labels, might need a binary search
/// rather than a linear one.
void LabelTrackView::OverGlyph(
    const LabelTrack& track, LabelTrackHit& hit, int x, int y)
{
    //Determine the NEW selection.
    int result=0;
    const int d1=10; //distance in pixels, used for have we hit drag handle.
    const int d2=5; //distance in pixels, used for have we hit drag handle center.

    //If not over a label, reset it
    hit.mMouseOverLabelLeft  = -1;
    hit.mMouseOverLabelRight = -1;
    hit.mMouseOverLabel = -1;
    hit.mEdge = 0;

    const auto pTrack = &track;
    const auto& mLabels = pTrack->GetLabels();
    {
        int i = -1;
        for (const auto& labelStruct : mLabels) {
            ++i;
            // give text box better priority for selecting
            // reset selection state
            if (OverTextBox(&labelStruct, x, y)) {
                result = 0;
                hit.mMouseOverLabel = -1;
                hit.mMouseOverLabelLeft = -1;
                hit.mMouseOverLabelRight = -1;
                break;
            }

            //over left or right selection bound
            //Check right bound first, since it is drawn after left bound,
            //so give it precedence for matching/highlighting.
            if (abs(labelStruct.y - (y - (mTextHeight + 3) / 2)) < d1
                && abs(labelStruct.x1 - d2 - x) < d1) {
                hit.mMouseOverLabelRight = i;
                if (abs(labelStruct.x1 - x) < d2) {
                    result |= 4;
                    // If left and right co-incident at this resolution, then we drag both.
                    // We were more stringent about co-incidence here in the past.
                    if (abs(labelStruct.x1 - labelStruct.x) < 5.0) {
                        result |=1;
                        hit.mMouseOverLabelLeft = i;
                    }
                }
                result |= 2;
            }
            // Use else-if here rather than else to avoid detecting left and right
            // of the same label.
            else if (abs(labelStruct.y - (y - (mTextHeight + 3) / 2)) < d1
                     && abs(labelStruct.x + d2 - x) < d1) {
                hit.mMouseOverLabelLeft = i;
                if (abs(labelStruct.x - x) < d2) {
                    result |= 4;
                }
                result |= 1;
            } else if (x >= labelStruct.x && x <= labelStruct.x1
                       && abs(y - (labelStruct.y + mTextHeight / 2)) < d1) {
                hit.mMouseOverLabel = i;
                result = 3;
            }
        }
    }
    hit.mEdge = result;
}

int LabelTrackView::OverATextBox(const LabelTrack& track, int xx, int yy)
{
    const auto pTrack = &track;
    const auto& mLabels = pTrack->GetLabels();
    for (int nn = (int)mLabels.size(); nn--;) {
        const auto& labelStruct = mLabels[nn];
        if (OverTextBox(&labelStruct, xx, yy)) {
            return nn;
        }
    }

    return -1;
}

// return true if the mouse is over text box, false otherwise
bool LabelTrackView::OverTextBox(const LabelStruct* pLabel, int x, int y)
{
    if ((pLabel->xText - (mIconWidth / 2) < x)
        && (x < pLabel->xText + pLabel->width + (mIconWidth / 2))
        && (abs(pLabel->y - y) < mIconHeight / 2)) {
        return true;
    }
    return false;
}

/// Returns true for keys we capture to start a label.
static bool IsGoodLabelFirstKey(const wxKeyEvent& evt)
{
    int keyCode = evt.GetKeyCode();
    return (keyCode < WXK_START
            && keyCode != WXK_SPACE && keyCode != WXK_DELETE && keyCode != WXK_RETURN)
           || (keyCode >= WXK_NUMPAD0 && keyCode <= WXK_DIVIDE)
           || (keyCode >= WXK_NUMPAD_EQUAL && keyCode <= WXK_NUMPAD_DIVIDE) ||
#if defined(__WXMAC__)
           (keyCode > WXK_RAW_CONTROL) ||
#endif
           (keyCode > WXK_WINDOWS_MENU);
}

/// This returns true for keys we capture for label editing.
static bool IsGoodLabelEditKey(const wxKeyEvent& evt)
{
    int keyCode = evt.GetKeyCode();

    // Accept everything outside of WXK_START through WXK_COMMAND, plus the keys
    // within that range that are usually printable, plus the ones we use for
    // keyboard navigation.
    return keyCode < WXK_START
           || (keyCode >= WXK_END && keyCode < WXK_UP)
           || (keyCode == WXK_RIGHT)
           || (keyCode >= WXK_NUMPAD0 && keyCode <= WXK_DIVIDE)
           || (keyCode >= WXK_NUMPAD_SPACE && keyCode <= WXK_NUMPAD_ENTER)
           || (keyCode >= WXK_NUMPAD_HOME && keyCode <= WXK_NUMPAD_END)
           || (keyCode >= WXK_NUMPAD_DELETE && keyCode <= WXK_NUMPAD_DIVIDE) ||
#if defined(__WXMAC__)
           (keyCode > WXK_RAW_CONTROL) ||
#endif
           (keyCode > WXK_WINDOWS_MENU);
}

// Check for keys that we will process
bool LabelTrackView::DoCaptureKey(
    AudacityProject& project, wxKeyEvent& event)
{
    int mods = event.GetModifiers();
    auto code = event.GetKeyCode();
    const auto pTrack = FindLabelTrack();
    const auto& mLabels = pTrack->GetLabels();

    // Allow hardcoded Ctrl+F2 for renaming the selected label,
    // if we have any labels
    if (code == WXK_F2 && mods == wxMOD_CONTROL && !mLabels.empty()) {
        return true;
    }

    // Check for modifiers and only allow shift
    if (mods != wxMOD_NONE && mods != wxMOD_SHIFT) {
        return false;
    }

    // Always capture the navigation keys, if we have any labels
    if ((code == WXK_TAB || code == WXK_NUMPAD_TAB)
        && !mLabels.empty()) {
        return true;
    }

    if (IsValidIndex(mTextEditIndex, project)) {
        if (IsGoodLabelEditKey(event)) {
            return true;
        }
    } else {
        bool typeToCreateLabel;
        gPrefs->Read(wxT("/GUI/TypeToCreateLabel"), &typeToCreateLabel, false);
        if (IsGoodLabelFirstKey(event) && typeToCreateLabel) {
// The commented out code can prevent label creation, causing bug 1551
// We should only be in DoCaptureKey IF this label track has focus,
// and in that case creating a Label is the expected/intended thing.
#if 0
            // If we're playing, don't capture if the selection is the same as the
            // playback region (this helps prevent label track creation from
            // stealing unmodified kbd. shortcuts)
            auto gAudioIO = AudioIOBase::Get();
            if (pProj->GetAudioIOToken() > 0
                && gAudioIO->IsStreamActive(pProj->GetAudioIOToken())) {
                double t0, t1;
                pProj->GetPlayRegion(&t0, &t1);
                if (pProj->mViewInfo.selectedRegion.t0() == t0
                    && pProj->mViewInfo.selectedRegion.t1() == t1) {
                    return false;
                }
            }
#endif

            // If there's a label there already don't capture
            auto& selectedRegion = ViewInfo::Get(project).selectedRegion;
            if (GetLabelIndex(selectedRegion.t0(),
                              selectedRegion.t1()) != wxNOT_FOUND) {
                return false;
            }

            return true;
        }
    }

    return false;
}

unsigned LabelTrackView::CaptureKey(
    wxKeyEvent& event, ViewInfo&, wxWindow*, AudacityProject* project)
{
    event.Skip(!DoCaptureKey(*project, event));
    return RefreshCode::RefreshNone;
}

unsigned LabelTrackView::KeyDown(
    wxKeyEvent& event, ViewInfo& viewInfo, wxWindow* WXUNUSED(pParent),
    AudacityProject* project)
{
    double bkpSel0 = viewInfo.selectedRegion.t0(),
           bkpSel1 = viewInfo.selectedRegion.t1();

    if (IsValidIndex(mTextEditIndex, *project) && !mTextEditIndex.IsModified()) {
        const auto pTrack = FindLabelTrack();
        const auto& mLabels = pTrack->GetLabels();
        auto labelStruct = mLabels[mTextEditIndex];
        auto& title = labelStruct.title;
        mUndoLabel = title;
    }

    // Pass keystroke to labeltrack's handler and add to history if any
    // updates were done
    if (DoKeyDown(*project, viewInfo.selectedRegion, event)) {
        ProjectHistory::Get(*project).PushState(XO("Modified Label"),
                                                XO("Label Edit"),
                                                mTextEditIndex.IsModified() ? UndoPush::CONSOLIDATE : UndoPush::NONE);

        mTextEditIndex.SetModified(true);
    }

    if (!mTextEditIndex.IsModified()) {
        mUndoLabel.clear();
    }

    // Make sure caret is in view
    int x;
    if (CalcCursorX(*project, &x)) {
        Viewport::Get(*project).ScrollIntoView(x);
    }

    // If selection modified, refresh
    // Otherwise, refresh track display if the keystroke was handled
    if (bkpSel0 != viewInfo.selectedRegion.t0()
        || bkpSel1 != viewInfo.selectedRegion.t1()) {
        return RefreshCode::RefreshAll;
    } else if (!event.GetSkipped()) {
        return RefreshCode::RefreshCell;
    }

    return RefreshCode::RefreshNone;
}

unsigned LabelTrackView::Char(
    wxKeyEvent& event, ViewInfo& viewInfo, wxWindow*, AudacityProject* project)
{
    double bkpSel0 = viewInfo.selectedRegion.t0(),
           bkpSel1 = viewInfo.selectedRegion.t1();
    // Pass keystroke to labeltrack's handler and add to history if any
    // updates were done

    if (IsValidIndex(mTextEditIndex, *project) && !mTextEditIndex.IsModified()) {
        const auto pTrack = FindLabelTrack();
        const auto& mLabels = pTrack->GetLabels();
        auto labelStruct = mLabels[mTextEditIndex];
        auto& title = labelStruct.title;
        mUndoLabel = title;
    }

    if (DoChar(*project, viewInfo.selectedRegion, event)) {
        ProjectHistory::Get(*project).PushState(XO("Modified Label"),
                                                XO("Label Edit"),
                                                mTextEditIndex.IsModified() ? UndoPush::CONSOLIDATE : UndoPush::NONE);

        mTextEditIndex.SetModified(true);
    }

    if (!mTextEditIndex.IsModified()) {
        mUndoLabel.clear();
    }

    // If selection modified, refresh
    // Otherwise, refresh track display if the keystroke was handled
    if (bkpSel0 != viewInfo.selectedRegion.t0()
        || bkpSel1 != viewInfo.selectedRegion.t1()) {
        return RefreshCode::RefreshAll;
    } else if (!event.GetSkipped()) {
        return RefreshCode::RefreshCell;
    }

    return RefreshCode::RefreshNone;
}

/// KeyEvent is called for every keypress when over the label track.
bool LabelTrackView::DoKeyDown(
    AudacityProject& project, NotifyingSelectedRegion& newSel, wxKeyEvent& event)
{
    // Only track true changes to the label
    bool updated = false;

    // Cache the keycode
    int keyCode = event.GetKeyCode();
    const int mods = event.GetModifiers();

    // Check for modifiers and only allow shift
    // except in the case of Ctrl + F2, so hardcoded Ctrl+F2 can
    // be used for renaming a label
    if ((keyCode != WXK_F2 && mods != wxMOD_NONE && mods != wxMOD_SHIFT)
        || (keyCode == WXK_F2 && mods != wxMOD_CONTROL)) {
        event.Skip();
        return updated;
    }

    // All editing keys are only active if we're currently editing a label
    const auto pTrack = FindLabelTrack();
    const auto& mLabels = pTrack->GetLabels();
    if (IsValidIndex(mTextEditIndex, project)) {
        // Do label text changes
        auto labelStruct = mLabels[mTextEditIndex];
        auto& title = labelStruct.title;
        wxUniChar wchar;
        bool more=true;

        switch (keyCode) {
        case WXK_BACK:
        {
            int len = title.length();

            //IF the label is not blank THEN get rid of a letter or letters according to cursor position
            if (len > 0) {
                // IF there are some highlighted letters, THEN DELETE them
                if (mInitialCursorPos != mCurrentCursorPos) {
                    RemoveSelectedText();
                } else {
                    // DELETE one codepoint leftwards
                    while ((mCurrentCursorPos > 0) && more) {
                        wchar = title.at(mCurrentCursorPos - 1);
                        title.erase(mCurrentCursorPos - 1, 1);
                        mCurrentCursorPos--;
                        if (((int)wchar > 0xDFFF) || ((int)wchar < 0xDC00)) {
                            pTrack->SetLabel(mTextEditIndex, labelStruct);
                            more = false;
                        }
                    }
                }
            } else {
                // ELSE no text in text box, so DELETE whole label.
                pTrack->DeleteLabel(mTextEditIndex);
                ResetTextSelection();
            }
            mInitialCursorPos = mCurrentCursorPos;
            updated = true;
        }
        break;

        case WXK_DELETE:
        case WXK_NUMPAD_DELETE:
        {
            int len = title.length();

            //If the label is not blank get rid of a letter according to cursor position
            if (len > 0) {
                // if there are some highlighted letters, DELETE them
                if (mInitialCursorPos != mCurrentCursorPos) {
                    RemoveSelectedText();
                } else {
                    // DELETE one codepoint rightwards
                    while ((mCurrentCursorPos < len) && more) {
                        wchar = title.at(mCurrentCursorPos);
                        title.erase(mCurrentCursorPos, 1);
                        if (((int)wchar > 0xDBFF) || ((int)wchar < 0xD800)) {
                            pTrack->SetLabel(mTextEditIndex, labelStruct);
                            more = false;
                        }
                    }
                }
            } else {
                // DELETE whole label if no text in text box
                pTrack->DeleteLabel(mTextEditIndex);
                ResetTextSelection();
            }
            mInitialCursorPos = mCurrentCursorPos;
            updated = true;
        }
        break;

        case WXK_HOME:
        case WXK_NUMPAD_HOME:
            // Move cursor to beginning of label
            mCurrentCursorPos = 0;
            if (mods == wxMOD_SHIFT) {
            } else {
                mInitialCursorPos = mCurrentCursorPos;
            }
            break;

        case WXK_END:
        case WXK_NUMPAD_END:
            // Move cursor to end of label
            mCurrentCursorPos = (int)title.length();
            if (mods == wxMOD_SHIFT) {
            } else {
                mInitialCursorPos = mCurrentCursorPos;
            }
            break;

        case WXK_LEFT:
        case WXK_NUMPAD_LEFT:
            // Moving cursor left
            if (mods != wxMOD_SHIFT && mCurrentCursorPos != mInitialCursorPos) {
                //put cursor to the left edge of selection
                mInitialCursorPos = mCurrentCursorPos
                                        =std::min(mInitialCursorPos, mCurrentCursorPos);
            } else {
                while ((mCurrentCursorPos > 0) && more) {
                    wchar = title.at(mCurrentCursorPos - 1);
                    more = !(((int)wchar > 0xDFFF) || ((int)wchar < 0xDC00));

                    --mCurrentCursorPos;
                }
                if (mods != wxMOD_SHIFT) {
                    mInitialCursorPos = mCurrentCursorPos;
                }
            }

            break;

        case WXK_RIGHT:
        case WXK_NUMPAD_RIGHT:
            // Moving cursor right
            if (mods != wxMOD_SHIFT && mCurrentCursorPos != mInitialCursorPos) {
                //put cursor to the right edge of selection
                mInitialCursorPos = mCurrentCursorPos
                                        =std::max(mInitialCursorPos, mCurrentCursorPos);
            } else {
                while ((mCurrentCursorPos < (int)title.length()) && more) {
                    wchar = title.at(mCurrentCursorPos);
                    more = !(((int)wchar > 0xDBFF) || ((int)wchar < 0xD800));

                    ++mCurrentCursorPos;
                }
                if (mods != wxMOD_SHIFT) {
                    mInitialCursorPos = mCurrentCursorPos;
                }
            }
            break;

        case WXK_ESCAPE:
            if (mTextEditIndex.IsModified()) {
                title = mUndoLabel;
                pTrack->SetLabel(mTextEditIndex, labelStruct);

                ProjectHistory::Get(project).PushState(XO("Modified Label"),
                                                       XO("Label Edit"),
                                                       mTextEditIndex.IsModified() ? UndoPush::CONSOLIDATE : UndoPush::NONE);
            }

        case WXK_RETURN:
        case WXK_NUMPAD_ENTER:
        case WXK_TAB:
            if (mRestoreFocus >= 0) {
                auto track = *TrackList::Get(project).Any()
                             .begin().advance(mRestoreFocus);
                if (track) {
                    TrackFocus::Get(project).Set(track);
                }
                mRestoreFocus = -2;
            }
            SetNavigationIndex(mTextEditIndex);
            ResetTextSelection();
            break;
        case '\x10': // OSX
        case WXK_MENU:
        case WXK_WINDOWS_MENU:
            ShowContextMenu(project);
            break;

        default:
            if (!IsGoodLabelEditKey(event)) {
                event.Skip();
            }
            break;
        }
    } else {
        // Do navigation
        switch (keyCode) {
        case WXK_ESCAPE:
            mNavigationIndex = -1;
            break;
        case WXK_TAB:
        case WXK_NUMPAD_TAB:
            if (!mLabels.empty()) {
                int len = (int)mLabels.size();
                // The case where the start of selection is the same as the
                // start of a label is handled separately so that if some labels
                // have the same start time, all labels are navigated.
                if (IsValidIndex(mNavigationIndex, project)
                    && mLabels[mNavigationIndex].getT0() == newSel.t0()) {
                    if (event.ShiftDown()) {
                        --mNavigationIndex;
                    } else {
                        ++mNavigationIndex;
                    }
                    mNavigationIndex = (mNavigationIndex + (int)mLabels.size()) % (int)mLabels.size(); // wrap round if necessary
                } else {
                    if (event.ShiftDown()) {
                        //search for the first label starting from the end (and before selection)
                        mNavigationIndex = len - 1;
                        if (newSel.t0() > mLabels[0].getT0()) {
                            while (mNavigationIndex >= 0
                                   && mLabels[mNavigationIndex].getT0() > newSel.t0()) {
                                --mNavigationIndex;
                            }
                        }
                    } else {
                        //search for the first label starting from the beginning (and after selection)
                        mNavigationIndex = 0;
                        if (newSel.t0() < mLabels[len - 1].getT0()) {
                            while (mNavigationIndex < len
                                   && mLabels[mNavigationIndex].getT0() < newSel.t0()) {
                                ++mNavigationIndex;
                            }
                        }
                    }
                }

                if (mNavigationIndex >= 0 && mNavigationIndex < len) {
                    const auto& labelStruct = mLabels[mNavigationIndex];
                    mCurrentCursorPos = labelStruct.title.length();
                    mInitialCursorPos = mCurrentCursorPos;
                    //Set the selection region to be equal to the selection bounds of the tabbed-to label.
                    newSel = labelStruct.selectedRegion;
                    Viewport::Get(project).ScrollIntoView(labelStruct.selectedRegion.t0());
                    // message for screen reader
                    /* i18n-hint:
                       String is replaced by the name of a label,
                       first number gives the position of that label in a sequence
                       of labels,
                       and the last number is the total number of labels in the sequence.
                    */
                    auto message = XO("%s %d of %d")
                                   .Format(labelStruct.title, mNavigationIndex + 1, pTrack->GetNumLabels());
                    TrackFocus::Get(project).MessageForScreenReader(message);
                } else {
                    mNavigationIndex = -1;
                }
            }
            break;
        case WXK_F2:   // Must be Ctrl + F2 to have reached here
            // Hardcoded Ctrl+F2 activates editing of the label
            // pointed to by mNavigationIndex (if valid)
            if (IsValidIndex(mNavigationIndex, project)) {
                SetTextSelection(mNavigationIndex);
            }
            break;
        default:
            if (!IsGoodLabelFirstKey(event)) {
                event.Skip();
            }
            break;
        }
    }

    return updated;
}

/// OnChar is called for incoming characters -- that's any keypress not handled
/// by OnKeyDown.
bool LabelTrackView::DoChar(
    AudacityProject& project, NotifyingSelectedRegion& WXUNUSED(newSel),
    wxKeyEvent& event)
{
    // Check for modifiers and only allow shift.
    //
    // We still need to check this or we will eat the top level menu accelerators
    // on Windows if our capture or key down handlers skipped the event.
    const int mods = event.GetModifiers();
    if (mods != wxMOD_NONE && mods != wxMOD_SHIFT) {
        event.Skip();
        return false;
    }

    // Only track true changes to the label
    //bool updated = false;

    // Cache the character
    wxChar charCode = event.GetUnicodeKey();

    // Skip if it's not a valid unicode character or a control character
    if (charCode == 0 || wxIscntrl(charCode)) {
        event.Skip();
        return false;
    }

    // If we've reached this point and aren't currently editing, add NEW label
    const auto pTrack = FindLabelTrack();
    if (!IsValidIndex(mTextEditIndex, project)) {
        // Don't create a NEW label for a space
        if (wxIsspace(charCode)) {
            event.Skip();
            return false;
        }
        bool useDialog;
        gPrefs->Read(wxT("/GUI/DialogForNameNewLabel"), &useDialog, false);
        auto& selectedRegion = ViewInfo::Get(project).selectedRegion;
        if (useDialog) {
            wxString title;
            if (DialogForLabelName(
                    project, selectedRegion, charCode, title)
                == wxID_CANCEL) {
                return false;
            }
            pTrack->SetSelected(true);
            pTrack->AddLabel(selectedRegion, title);
            ProjectHistory::Get(project)
            .PushState(XO("Added label"), XO("Label"));
            return false;
        } else {
            pTrack->SetSelected(true);
            AddLabel(selectedRegion);
            ProjectHistory::Get(project)
            .PushState(XO("Added label"), XO("Label"));
        }
    }

    if (!IsValidIndex(mTextEditIndex, project)) {
        return false;
    }

    //
    // Now we are definitely in a label; append the incoming character
    //

    // Test if cursor is in the end of string or not
    if (mInitialCursorPos != mCurrentCursorPos) {
        RemoveSelectedText();
    }

    const auto& mLabels = pTrack->GetLabels();
    auto labelStruct = mLabels[mTextEditIndex];
    auto& title = labelStruct.title;

    if (mCurrentCursorPos < (int)title.length()) {
        // Get substring on the righthand side of cursor
        wxString rightPart = title.Mid(mCurrentCursorPos);
        // Set title to substring on the lefthand side of cursor
        title = title.Left(mCurrentCursorPos);
        //append charcode
        title += charCode;
        //append the right part substring
        title += rightPart;
    } else {
        //append charCode
        title += charCode;
    }

    pTrack->SetLabel(mTextEditIndex, labelStruct);

    //moving cursor position forward
    mInitialCursorPos = ++mCurrentCursorPos;

    return true;
}

enum
{
    OnCutSelectedTextID = 1,     // OSX doesn't like a 0 menu id
    OnCopySelectedTextID,
    OnPasteSelectedTextID,
    OnDeleteSelectedLabelID,
    OnEditSelectedLabelID,
};

void LabelTrackView::ShowContextMenu(AudacityProject& project)
{
    wxWindow* parent = wxWindow::FindFocus();

    // Bug 2044.  parent can be nullptr after a context switch.
    if (!parent) {
        parent = &GetProjectFrame(project);
    }

    if (parent) {
        wxMenu menu;
        menu.Bind(wxEVT_MENU,
                  [this, &project]( wxCommandEvent& event ){
            OnContextMenu(project, event);
        }
                  );

        menu.Append(OnCutSelectedTextID, _("Cu&t Label text"));
        menu.Append(OnCopySelectedTextID, _("&Copy Label text"));
        menu.Append(OnPasteSelectedTextID, _("&Paste"));
        menu.Append(OnDeleteSelectedLabelID, _("&Delete Label"));
        menu.Append(OnEditSelectedLabelID, _("&Edit Label..."));

        menu.Enable(OnCutSelectedTextID, IsTextSelected(project));
        menu.Enable(OnCopySelectedTextID, IsTextSelected(project));
        menu.Enable(OnPasteSelectedTextID, IsTextClipSupported());
        menu.Enable(OnDeleteSelectedLabelID, true);
        menu.Enable(OnEditSelectedLabelID, true);

        if (!IsValidIndex(mTextEditIndex, project)) {
            return;
        }

        const auto pTrack = FindLabelTrack();
        const LabelStruct* ls = pTrack->GetLabel(mTextEditIndex);

        wxClientDC dc(parent);

        if (msFont.Ok()) {
            dc.SetFont(msFont);
        }

        int x = 0;
        bool success = CalcCursorX(project, &x);
        wxASSERT(success);
        static_cast<void>(success); // Suppress unused variable warning if debug mode is disabled

        // Bug #2571: Hackage alert! For some reason wxGTK does not like
        // displaying the LabelDialog from within the PopupMenu "context".
        // So, workaround it by editing the label AFTER the popup menu is
        // closed. It's really ugly, but it works.  :-(
        mEditIndex = -1;
        BasicMenu::Handle{ &menu }.Popup(
            wxWidgetsWindowPlacement { parent },
            { x, ls->y + (mIconHeight / 2) - 1 }
            );
        if (mEditIndex >= 0) {
            DoEditLabels(project, FindLabelTrack().get(), mEditIndex);
        }
    }
}

void LabelTrackView::OnContextMenu(
    AudacityProject& project, wxCommandEvent& evt)
{
    auto& selectedRegion = ViewInfo::Get(project).selectedRegion;

    switch (evt.GetId()) {
    /// Cut selected text if cut menu item is selected
    case OnCutSelectedTextID:
        if (CutSelectedText(project)) {
            ProjectHistory::Get(project).PushState(XO("Modified Label"),
                                                   XO("Label Edit"),
                                                   mTextEditIndex.IsModified() ? UndoPush::CONSOLIDATE : UndoPush::NONE);
        }
        break;

    /// Copy selected text if copy menu item is selected
    case OnCopySelectedTextID:
        CopySelectedText(project);
        break;

    /// paste selected text if paste menu item is selected
    case OnPasteSelectedTextID:
        if (PasteSelectedText(
                project, selectedRegion.t0(), selectedRegion.t1())) {
            ProjectHistory::Get(project).PushState(XO("Modified Label"),
                                                   XO("Label Edit"),
                                                   mTextEditIndex.IsModified() ? UndoPush::CONSOLIDATE : UndoPush::NONE);
        }
        break;

    /// DELETE selected label
    case OnDeleteSelectedLabelID: {
        if (IsValidIndex(mTextEditIndex, project)) {
            const auto pTrack = FindLabelTrack();
            pTrack->DeleteLabel(mTextEditIndex);
            ProjectHistory::Get(project).PushState(XO("Deleted Label"),
                                                   XO("Label Edit"),
                                                   UndoPush::CONSOLIDATE);
        }
    }
    break;

    case OnEditSelectedLabelID: {
        // Bug #2571: See above
        if (IsValidIndex(mTextEditIndex, project)) {
            mEditIndex = mTextEditIndex;
        }
    }
    break;
    }
}

void LabelTrackView::RemoveSelectedText()
{
    wxString left, right;

    int init = mInitialCursorPos;
    int cur = mCurrentCursorPos;
    if (init > cur) {
        std::swap(init, cur);
    }

    const auto pTrack = FindLabelTrack();
    const auto& mLabels = pTrack->GetLabels();
    auto labelStruct = mLabels[mTextEditIndex];
    auto& title = labelStruct.title;

    if (init > 0) {
        left = title.Left(init);
    }

    if (cur < (int)title.length()) {
        right = title.Mid(cur);
    }

    title = left + right;
    pTrack->SetLabel(mTextEditIndex, labelStruct);
    mInitialCursorPos = mCurrentCursorPos = left.length();
}

/*
bool LabelTrackView::HasSelectedLabel( AudacityProject &project ) const
{
   const auto selIndex = GetSelectionIndex( project );
   return (selIndex >= 0 &&
      selIndex < (int)FindLabelTrack()->GetLabels().size());
}*/

int LabelTrackView::GetLabelIndex(double t, double t1)
{
    //We'd have liked to have times in terms of samples,
    //because then we're doing an intrger comparison.
    //Never mind.  Instead we look for near enough.
    //This level of (in)accuracy is only a problem if we
    //deal with sounds in the MHz range.
    const double delta = 1.0e-7;
    const auto pTrack = FindLabelTrack();
    const auto& mLabels = pTrack->GetLabels();
    {
        int i = -1;
        for (const auto& labelStruct : mLabels) {
            ++i;
            if (fabs(labelStruct.getT0() - t) > delta) {
                continue;
            }
            if (fabs(labelStruct.getT1() - t1) > delta) {
                continue;
            }
            return i;
        }
    }

    return wxNOT_FOUND;
}

// restoreFocus of -1 is the default, and sets the focus to this label.
// restoreFocus of -2 or other value leaves the focus unchanged.
// restoreFocus >= 0 will later cause focus to move to that track (counting
// tracks, not channels)
int LabelTrackView::AddLabel(const SelectedRegion& selectedRegion,
                             const wxString& title, int restoreFocus)
{
    const auto pTrack = FindLabelTrack();
    mRestoreFocus = restoreFocus;
    auto pos = pTrack->AddLabel(selectedRegion, title);
    return pos;
}

void LabelTrackView::OnLabelAdded(const LabelTrackEvent& e)
{
    if (e.mpTrack.lock() != FindLabelTrack()) {
        return;
    }

    const auto& title = e.mTitle;
    const auto pos = e.mPresentPosition;

    mInitialCursorPos = mCurrentCursorPos = title.length();

    // restoreFocus is -2 e.g. from Nyquist label creation, when we should not
    // even lose the focus and open the label to edit in the first place.
    // -1 means we don't need to restore it to anywhere.
    // 0 or above is the track to restore to after editing the label is complete.
    if (mRestoreFocus >= -1) {
        mTextEditIndex = pos;
    }

    if (mRestoreFocus < 0) {
        mRestoreFocus = -2;
    }
}

void LabelTrackView::OnLabelDeleted(const LabelTrackEvent& e)
{
    if (e.mpTrack.lock() != FindLabelTrack()) {
        return;
    }

    auto index = e.mFormerPosition;

    // IF we've deleted the selected label
    // THEN set no label selected.
    if (mTextEditIndex == index) {
        ResetTextSelection();
    }
    // IF we removed a label before the selected label
    // THEN the NEW selected label number is one less.
    else if (index < mTextEditIndex) {
        --mTextEditIndex;//NB: Keep cursor selection region
    }
}

void LabelTrackView::OnLabelPermuted(const LabelTrackEvent& e)
{
    if (e.mpTrack.lock() != FindLabelTrack()) {
        return;
    }

    auto former = e.mFormerPosition;
    auto present = e.mPresentPosition;

    auto fix = [&](Index& index) {
        if (index == former) {
            index = present;
        } else if (former < index && index <= present) {
            --index;
        } else if (former > index && index >= present) {
            ++index;
        }
    };
    fix(mNavigationIndex);
    fix(mTextEditIndex);
}

void LabelTrackView::OnSelectionChange(const LabelTrackEvent& e)
{
    if (e.mpTrack.lock() != FindLabelTrack()) {
        return;
    }

    if (!FindLabelTrack()->GetSelected()) {
        SetNavigationIndex(-1);
        ResetTextSelection();
    }
}

wxBitmap& LabelTrackView::GetGlyph(int i)
{
    return theTheme.Bitmap(i + bmpLabelGlyph0);
}

// This one XPM spec is used to generate a number of
// different wxIcons.
/* XPM */
static const char* const GlyphXpmRegionSpec[] = {
/* columns rows colors chars-per-pixel */
    "15 23 7 1",
/* Default colors, with first color transparent */
    ". c none",
    "2 c black",
    "3 c black",
    "4 c black",
    "5 c #BEBEF0",
    "6 c #BEBEF0",
    "7 c #BEBEF0",
/* pixels */
    "...............",
    "...............",
    "...............",
    "....333.444....",
    "...3553.4774...",
    "...3553.4774...",
    "..35553.47774..",
    "..35522222774..",
    ".3552666662774.",
    ".3526666666274.",
    "355266666662774",
    "355266666662774",
    "355266666662774",
    ".3526666666274.",
    ".3552666662774.",
    "..35522222774..",
    "..35553.47774..",
    "...3553.4774...",
    "...3553.4774...",
    "....333.444....",
    "...............",
    "...............",
    "..............."
};

/// CreateCustomGlyphs() creates the mBoundaryGlyph array.
/// It's a bit like painting by numbers!
///
/// Schematically the glyphs we want will 'look like':
///   <O,  O>   and   <O>
/// for a left boundary to a label, a right boundary and both.
/// we're creating all three glyphs using the one Xpm Spec.
///
/// When we hover over a glyph we highlight the
/// inside of either the '<', the 'O' or the '>' or none,
/// giving 3 x 4 = 12 combinations.
///
/// Two of those combinations aren't used, but
/// treating them specially would make other code more
/// complicated.
void LabelTrackView::CreateCustomGlyphs()
{
    int iConfig;
    int iHighlight;
    int index;
    const int nSpecRows
        =sizeof(GlyphXpmRegionSpec) / sizeof(GlyphXpmRegionSpec[0]);
    const char* XmpBmp[nSpecRows];

    // The glyphs are declared static wxIcon; so we only need
    // to create them once, no matter how many LabelTracks.
    if (mbGlyphsReady) {
        return;
    }

    // We're about to tweak the basic color spec to get 12 variations.
    for ( iConfig=0; iConfig < NUM_GLYPH_CONFIGS; iConfig++) {
        for ( iHighlight=0; iHighlight < NUM_GLYPH_HIGHLIGHTS; iHighlight++) {
            index = iConfig + NUM_GLYPH_CONFIGS * iHighlight;
            // Copy the basic spec...
            memcpy(XmpBmp, GlyphXpmRegionSpec, sizeof(GlyphXpmRegionSpec));
            // The highlighted region (if any) is white...
            if (iHighlight == 1) {
                XmpBmp[5]="5 c #FFFFFF";
            }
            if (iHighlight == 2) {
                XmpBmp[6]="6 c #FFFFFF";
            }
            if (iHighlight == 3) {
                XmpBmp[7]="7 c #FFFFFF";
            }
            // For left or right arrow the other side of the glyph
            // is the transparent color.
            if (iConfig == 0) {
                XmpBmp[3]="3 c none";
                XmpBmp[5]="5 c none";
            }
            if (iConfig == 1) {
                XmpBmp[4]="4 c none";
                XmpBmp[7]="7 c none";
            }
            // Create the icon from the tweaked spec.
            mBoundaryGlyphs[index] = wxBitmap(XmpBmp);
            // Create the mask
            // SetMask takes ownership
            mBoundaryGlyphs[index].SetMask(safenew wxMask(mBoundaryGlyphs[index], wxColour(192, 192, 192)));
        }
    }

    mIconWidth  = mBoundaryGlyphs[0].GetWidth();
    mIconHeight = mBoundaryGlyphs[0].GetHeight();
    mTextHeight = mIconHeight; // until proved otherwise...
    // The icon should have an odd width so that the
    // line goes exactly down the middle.
    wxASSERT((mIconWidth % 2) == 1);

    mbGlyphsReady=true;
}

#include "../../../LabelDialog.h"

void LabelTrackView::DoEditLabels
    (AudacityProject& project, LabelTrack* lt, int index)
{
    const auto& formats = ProjectNumericFormats::Get(project);
    auto format = formats.GetSelectionFormat(),
         freqFormat = formats.GetFrequencySelectionFormatName();
    auto& tracks = TrackList::Get(project);
    auto& viewInfo = ViewInfo::Get(project);
    auto& window = GetProjectFrame(project);

    LabelDialog dlg(&window, project, &tracks,
                    lt, index,
                    viewInfo,
                    format, freqFormat);
#ifdef __WXGTK__
    dlg.Raise();
#endif

    if (dlg.ShowModal() == wxID_OK) {
        ProjectHistory::Get(project)
        .PushState(XO("Edited labels"), XO("Label"));
    }
}

int LabelTrackView::DialogForLabelName(
    AudacityProject& project,
    const SelectedRegion& region, const wxString& initialValue, wxString& value)
{
    auto& trackFocus = TrackFocus::Get(project);
    auto& trackPanel = TrackPanel::Get(project);
    auto& viewInfo = ViewInfo::Get(project);

    wxPoint position
        =trackPanel.FindTrackRect(trackFocus.Get()).GetBottomLeft();
    // The start of the text in the text box will be roughly in line with the label's position
    // if it's a point label, or the start of its region if it's a region label.
    position.x += viewInfo.GetLeftOffset()
                  + std::max(0, static_cast<int>(viewInfo.TimeToPosition(region.t0())))
                  - 39;
    position.y += 2; // just below the bottom of the track
    position = trackPanel.ClientToScreen(position);
    auto& window = GetProjectFrame(project);
    AudacityTextEntryDialog dialog{ &window,
                                    XO("Name:"),
                                    XO("New label"),
                                    initialValue,
                                    wxOK | wxCANCEL,
                                    position };

    // keep the dialog within Audacity's window, so that the dialog is always fully visible
    wxRect dialogScreenRect = dialog.GetScreenRect();
    wxRect projScreenRect = window.GetScreenRect();
    wxPoint max = projScreenRect.GetBottomRight() + wxPoint{ -dialogScreenRect.width, -dialogScreenRect.height };
    if (dialogScreenRect.x > max.x) {
        position.x = max.x;
        dialog.Move(position);
    }
    if (dialogScreenRect.y > max.y) {
        position.y = max.y;
        dialog.Move(position);
    }

    dialog.SetInsertionPointEnd();     // because, by default, initial text is selected
    int status = dialog.ShowModal();
    if (status != wxID_CANCEL) {
        value = dialog.GetValue();
        value.Trim(true).Trim(false);
    }

    return status;
}

using DoGetLabelTrackView = DoGetView::Override<LabelTrack>;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(DoGetLabelTrackView) {
    return [](LabelTrack& track, size_t) {
        return std::make_shared<LabelTrackView>(
            track.SharedPointer<LabelTrack>());
    };
}

std::shared_ptr<ChannelVRulerControls> LabelTrackView::DoGetVRulerControls()
{
    return
        std::make_shared<LabelTrackVRulerControls>(shared_from_this());
}

using GetLabelTrackSyncLockPolicy
    =GetSyncLockPolicy::Override< const LabelTrack >;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(GetLabelTrackSyncLockPolicy) {
    return [](auto&) { return SyncLockPolicy::EndSeparator; };
}
