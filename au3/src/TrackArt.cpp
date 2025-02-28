/**********************************************************************

  Audacity: A Digital Audio Editor

  @file TrackArt.cpp

  Paul Licameli split from TrackArtist.cpp

**********************************************************************/

#include "TrackArt.h"

#include "AColor.h"
#include "AllThemeResources.h"
#include "SelectedRegion.h"
#include "SyncLock.h"
#include "Theme.h"
#include "TimeAndPitchInterface.h"
#include "Track.h"
#include "TrackArtist.h"
#include "TrackPanelDrawingContext.h"
#include "ZoomInfo.h"
#include "TimeDisplayMode.h"
#include "ProjectTimeRuler.h"

#include "ProjectTimeSignature.h"

#include "widgets/BeatsFormat.h"

#include <wx/app.h>
#include <wx/dc.h>

#include <cassert>
#include <cstdint>
#include <cmath>
#include <utility>

static constexpr int ClipSelectionStrokeSize{ 1 };//px

namespace {
//Helper function that takes affordance rectangle as
//argument and returns rectangle to be used for title
//drawings
wxRect GetAffordanceTitleRect(const wxRect& rect)
{
    constexpr int FrameThickness{ 1 };
    return wxRect(
        rect.GetLeft() + TrackArt::ClipFrameRadius,
        rect.GetTop() + ClipSelectionStrokeSize + FrameThickness,
        rect.GetWidth() - TrackArt::ClipFrameRadius * 2,
        rect.GetHeight() - ClipSelectionStrokeSize - FrameThickness);
}

wxString
GetTruncatedTitle(wxDC& dc, const wxString& title, const wxRect& affordanceRect)
{
    if (affordanceRect.IsEmpty() || title.empty()) {
        return wxEmptyString;
    }
    return TrackArt::TruncateText(dc, title, affordanceRect.GetWidth());
}
}

/// Takes a value between min and max and returns a value between
/// height and 0
int GetWaveYPos(float value, float min, float max,
                int height, bool dB, bool outer,
                float dBr, bool clip)
{
    if (dB) {
        if (height == 0) {
            return 0;
        }

        float sign = (value >= 0 ? 1 : -1);

        if (value != 0.) {
            float db = LINEAR_TO_DB(fabs(value));
            value = (db + dBr) / dBr;
            if (!outer) {
                value -= 0.5;
            }
            if (value < 0.0) {
                value = 0.0;
            }
            value *= sign;
        }
    } else {
        if (!outer) {
            if (value >= 0.0) {
                value -= 0.5;
            } else {
                value += 0.5;
            }
        }
    }

    if (clip) {
        if (value < min) {
            value = min;
        }
        if (value > max) {
            value = max;
        }
    }

    value = (max - value) / (max - min);
    return (int)(value * (height - 1) + 0.5);
}

float FromDB(float value, double dBRange)
{
    if (value == 0) {
        return 0;
    }

    double sign = (value >= 0 ? 1 : -1);
    return DB_TO_LINEAR((fabs(value) * dBRange) - dBRange) * sign;
}

float ValueOfPixel(int yy, int height, bool offset,
                   bool dB, double dBRange, float zoomMin, float zoomMax)
{
    wxASSERT(height > 0);
    // Map 0 to max and height - 1 (not height) to min
    float v
        =height == 1 ? (zoomMin + zoomMax) / 2
          : zoomMax - (yy / (float)(height - 1)) * (zoomMax - zoomMin);
    if (offset) {
        if (v > 0.0) {
            v += .5;
        } else {
            v -= .5;
        }
    }

    if (dB) {
        v = FromDB(v, dBRange);
    }

    return v;
}

void TrackArt::DrawNegativeOffsetTrackArrows(
    TrackPanelDrawingContext& context, const wxRect& rect)
{
    auto& dc = context.dc;

    // Draws two black arrows on the left side of the track to
    // indicate the user that the track has been time-shifted
    // to the left beyond t=0.0.

    dc.SetPen(*wxBLACK_PEN);
    AColor::Line(dc,
                 rect.x + 2, rect.y + 6,
                 rect.x + 8, rect.y + 6);
    AColor::Line(dc,
                 rect.x + 2, rect.y + 6,
                 rect.x + 6, rect.y + 2);
    AColor::Line(dc,
                 rect.x + 2, rect.y + 6,
                 rect.x + 6, rect.y + 10);
    AColor::Line(dc,
                 rect.x + 2, rect.y + rect.height - 8,
                 rect.x + 8, rect.y + rect.height - 8);
    AColor::Line(dc,
                 rect.x + 2, rect.y + rect.height - 8,
                 rect.x + 6, rect.y + rect.height - 4);
    AColor::Line(dc,
                 rect.x + 2, rect.y + rect.height - 8,
                 rect.x + 6, rect.y + rect.height - 12);
}

wxString TrackArt::TruncateText(wxDC& dc, const wxString& text, const int maxWidth)
{
    static const wxString ellipsis = "\u2026";

    if (dc.GetTextExtent(text).GetWidth() <= maxWidth) {
        return text;
    }

    auto left = 0;
    //no need to check text + '...'
    auto right = static_cast<int>(text.Length() - 2);

    while (left <= right)
    {
        auto middle = (left + right) / 2;
        auto str = text.SubString(0, middle).Trim() + ellipsis;
        auto strWidth = dc.GetTextExtent(str).GetWidth();
        if (strWidth < maxWidth) {
            //if left == right (== middle), then exit loop
            //with right equals to the last known index for which
            //strWidth < maxWidth
            left = middle + 1;
        } else if (strWidth > maxWidth) {
            //if right == left (== middle), then exit loop with
            //right equals to (left - 1), which is the last known
            //index for which (strWidth < maxWidth) or -1
            right = middle - 1;
        } else {
            return str;
        }
    }
    if (right >= 0) {
        return text.SubString(0, right).Trim() + ellipsis;
    }

    return wxEmptyString;
}

namespace {
wxRect GetClipAffordanceRect(
    wxDC& dc, const wxRect& clipRect, std::optional<wxRect>& clippedClipRect)
{
    wxRect tmp;
    const auto hasClipRect = dc.GetClippingBox(tmp);
    if (hasClipRect) {
        clippedClipRect = tmp;
    }
    return hasClipRect
           ? // avoid drawing text outside the clipping rectangle if possible
           GetAffordanceTitleRect(clipRect.Intersect(*clippedClipRect))
           : GetAffordanceTitleRect(clipRect);
}
}

wxRect TrackArt::DrawClipAffordance(
    wxDC& dc, const wxRect& clipRect, bool highlight, bool selected)
{
    //To make sure that roundings do not overlap each other
    auto clipFrameRadius = std::min(ClipFrameRadius, clipRect.width / 2);

    std::optional<wxRect> clippedClipRect;
    const auto affordanceRect
        =::GetClipAffordanceRect(dc, clipRect, clippedClipRect);
    //Fix #1689: visual glitches appear on attempt to draw a rectangle
    //larger than 0x7FFFFFF pixels wide (value was discovered
    //by manual testing, and maybe depends on OS being used), but
    //it's very unlikely that such huge rectangle will be ever fully visible
    //on the screen, so we can safely reduce its size to be slightly larger than
    //clipping rectangle, and avoid that problem
    auto drawingRect = clipRect;
    if (clippedClipRect) {
        //to make sure that rounding happends outside the clipping rectangle
        drawingRect.SetLeft(std::max(
                                clipRect.GetLeft(),
                                clippedClipRect->GetLeft() - clipFrameRadius - 1));
        drawingRect.SetRight(std::min(
                                 clipRect.GetRight(),
                                 clippedClipRect->GetRight() + clipFrameRadius + 1));
    }

    if (selected) {
        wxRect strokeRect{
            drawingRect.x - ClipSelectionStrokeSize,
            drawingRect.y,
            drawingRect.width + ClipSelectionStrokeSize * 2,
            drawingRect.height + clipFrameRadius };
        dc.SetBrush(*wxTRANSPARENT_BRUSH);
        AColor::UseThemeColour(&dc, clrClipAffordanceStroke, clrClipAffordanceStroke);
        dc.DrawRoundedRectangle(strokeRect, clipFrameRadius);
    }

    AColor::UseThemeColour(&dc, highlight ? clrClipAffordanceActiveBrush : clrClipAffordanceInactiveBrush, clrClipAffordanceOutlinePen);
    dc.DrawRoundedRectangle(
        wxRect(
            drawingRect.x,
            drawingRect.y + ClipSelectionStrokeSize,
            drawingRect.width,
            drawingRect.height + clipFrameRadius
            ), clipFrameRadius
        );

    return affordanceRect;
}

namespace {
wxRect GetClipTruncatedTitleRect(
    wxDC& dc, const wxRect& affordanceRect, const wxString& title)
{
    const auto alignLeft
        =wxTheApp->GetLayoutDirection() == wxLayout_LeftToRight;
    const auto width = dc.GetTextExtent(title).GetWidth();
    const auto left
        =alignLeft ? affordanceRect.GetLeft() : affordanceRect.GetRight() - width;
    return { left, affordanceRect.GetTop(), width, affordanceRect.GetHeight() };
}
} // namespace

bool TrackArt::DrawClipTitle(
    wxDC& dc, const wxRect& affordanceRect, const wxString& title)
{
    if (affordanceRect.IsEmpty()) {
        return false;
    } else if (title.empty()) {
        return true;
    }
    const auto truncatedTitle = ::GetTruncatedTitle(dc, title, affordanceRect);
    if (truncatedTitle.empty()) {
        return false;
    }
    const auto titleRect
        =GetClipTruncatedTitleRect(dc, affordanceRect, truncatedTitle);
    const auto alignLeft
        =wxTheApp->GetLayoutDirection() == wxLayout_LeftToRight;
    dc.DrawLabel(
        truncatedTitle, titleRect,
        (alignLeft ? wxALIGN_LEFT : wxALIGN_RIGHT) | wxALIGN_CENTER_VERTICAL);
    return true;
}

void TrackArt::DrawClipEdges(wxDC& dc, const wxRect& clipRect, bool selected)
{
    dc.SetBrush(*wxTRANSPARENT_BRUSH);
    {
        AColor::UseThemeColour(&dc, -1, clrClipAffordanceOutlinePen);
        AColor::Line(dc,
                     clipRect.GetLeft(), clipRect.GetTop(),
                     clipRect.GetLeft(), clipRect.GetBottom());
        AColor::Line(dc,
                     clipRect.GetRight(), clipRect.GetTop(),
                     clipRect.GetRight(), clipRect.GetBottom());
    }
    if (selected) {
        if constexpr (ClipSelectionStrokeSize == 1) {
            AColor::UseThemeColour(&dc, -1, clrClipAffordanceStroke);
            AColor::Line(dc,
                         clipRect.GetLeft() - ClipSelectionStrokeSize, clipRect.GetTop(),
                         clipRect.GetLeft() - ClipSelectionStrokeSize, clipRect.GetBottom());
            AColor::Line(dc,
                         clipRect.GetRight() + ClipSelectionStrokeSize, clipRect.GetTop(),
                         clipRect.GetRight() + ClipSelectionStrokeSize, clipRect.GetBottom());
        } else if constexpr (ClipSelectionStrokeSize > 1) {
            AColor::UseThemeColour(&dc, clrClipAffordanceStroke, clrClipAffordanceStroke);
            dc.DrawRectangle(wxRect(
                                 clipRect.GetLeft() - ClipSelectionStrokeSize, clipRect.GetTop(),
                                 ClipSelectionStrokeSize, clipRect.GetHeight()));
            dc.DrawRectangle(wxRect(
                                 clipRect.GetRight() + 1, clipRect.GetTop(),
                                 ClipSelectionStrokeSize, clipRect.GetHeight()));
        }
    }
}

void TrackArt::DrawClipFolded(wxDC& dc, const wxRect& rect)
{
    AColor::UseThemeColour(&dc, clrClipAffordanceOutlinePen);
    dc.DrawRectangle(rect);
}

// Draws the sync-lock bitmap, tiled; always draws stationary relative to the DC
//
// AWD: now that the tiles don't link together, we're drawing a tilted grid, at
// two steps down for every one across. This creates a pattern that repeats in
// 5-step by 5-step boxes. Because we're only drawing in 5/25 possible positions
// we have a grid spacing somewhat smaller than the image dimensions. Thus we
// achieve lower density than with a square grid and eliminate edge cases where
// no tiles are displayed.
//
// The pattern draws in tiles at (0,0), (2,1), (4,2), (1,3), and (3,4) in each
// 5x5 box.
//
// There may be a better way to do this, or a more appealing pattern.
void TrackArt::DrawSyncLockTiles(
    TrackPanelDrawingContext& context, const wxRect& rect)
{
    const auto dc = &context.dc;

    wxBitmap syncLockBitmap(theTheme.Image(bmpSyncLockSelTile));

    // Grid spacing is a bit smaller than actual image size
    int gridW = syncLockBitmap.GetWidth() - 6;
    int gridH = syncLockBitmap.GetHeight() - 8;

    // Horizontal position within the grid, modulo its period
    int blockX = (rect.x / gridW) % 5;

    // Amount to offset drawing of first column
    int xOffset = rect.x % gridW;
    if (xOffset < 0) {
        xOffset += gridW;
    }

    // Check if we're missing an extra column to the left (this can happen
    // because the tiles are bigger than the grid spacing)
    bool extraCol = false;
    if (syncLockBitmap.GetWidth() - gridW > xOffset) {
        extraCol = true;
        xOffset += gridW;
        blockX = (blockX - 1) % 5;
    }
    // Make sure blockX is non-negative
    if (blockX < 0) {
        blockX += 5;
    }

    int xx = 0;
    while (xx < rect.width) {
        int width = syncLockBitmap.GetWidth() - xOffset;
        if (xx + width > rect.width) {
            width = rect.width - xx;
        }

        //
        // Draw each row in this column
        //

        // Vertical position in the grid, modulo its period
        int blockY = (rect.y / gridH) % 5;

        // Amount to offset drawing of first row
        int yOffset = rect.y % gridH;
        if (yOffset < 0) {
            yOffset += gridH;
        }

        // Check if we're missing an extra row on top (this can happen because
        // the tiles are bigger than the grid spacing)
        bool extraRow = false;
        if (syncLockBitmap.GetHeight() - gridH > yOffset) {
            extraRow = true;
            yOffset += gridH;
            blockY = (blockY - 1) % 5;
        }
        // Make sure blockY is non-negative
        if (blockY < 0) {
            blockY += 5;
        }

        int yy = 0;
        while (yy < rect.height)
        {
            int height = syncLockBitmap.GetHeight() - yOffset;
            if (yy + height > rect.height) {
                height = rect.height - yy;
            }

            // AWD: draw blocks according to our pattern
            if ((blockX == 0 && blockY == 0) || (blockX == 2 && blockY == 1)
                || (blockX == 4 && blockY == 2) || (blockX == 1 && blockY == 3)
                || (blockX == 3 && blockY == 4)) {
                // Do we need to get a sub-bitmap?
                if (width != syncLockBitmap.GetWidth() || height != syncLockBitmap.GetHeight()) {
                    wxBitmap subSyncLockBitmap
                        =syncLockBitmap.GetSubBitmap(wxRect(xOffset, yOffset, width, height));
                    dc->DrawBitmap(subSyncLockBitmap, rect.x + xx, rect.y + yy, true);
                } else {
                    dc->DrawBitmap(syncLockBitmap, rect.x + xx, rect.y + yy, true);
                }
            }

            // Updates for next row
            if (extraRow) {
                // Second offset row, still at y = 0; no more extra rows
                yOffset -= gridH;
                extraRow = false;
            } else {
                // Move on in y, no more offset rows
                yy += gridH - yOffset;
                yOffset = 0;
            }
            blockY = (blockY + 1) % 5;
        }

        // Updates for next column
        if (extraCol) {
            // Second offset column, still at x = 0; no more extra columns
            xOffset -= gridW;
            extraCol = false;
        } else {
            // Move on in x, no more offset rows
            xx += gridW - xOffset;
            xOffset = 0;
        }
        blockX = (blockX + 1) % 5;
    }
}

namespace {
constexpr double minSubdivisionWidth = 12.0;

const AudacityProject& GetProject(const Track& track)
{
    // Track is expected to have owner
    assert(track.GetOwner());
    // TrackList is expected to have owner
    assert(track.GetOwner()->GetOwner());

    return *track.GetOwner()->GetOwner();
}

struct BeatsGridlinePainter final
{
    const ZoomInfo& zoomInfo;
    const bool enabled;

    const BeatsFormat& beatsRulerFormat;

    const BeatsFormat::Tick majorTick;
    const BeatsFormat::Tick minorTick;

    // Note duration in seconds
    const double noteDuration;
    // Note width in pixels
    const double noteWidth;
    // How many "notes" should be colored with a single color?
    const int64_t notesInBeat;

    BeatsGridlinePainter(const ZoomInfo& zoomInfo, const AudacityProject& project)
    noexcept
        : zoomInfo{zoomInfo}
        , enabled{TimeDisplayModePreference.ReadEnum()
                  == TimeDisplayMode::BeatsAndMeasures}
        , beatsRulerFormat{ProjectTimeRuler::Get(project).GetBeatsFormat()}
        , majorTick{beatsRulerFormat.GetSubdivision().major}
        , minorTick{GetMinorTick()}
        , noteDuration{minorTick.duration}
        , noteWidth{zoomInfo.TimeRangeToPixelWidth(noteDuration)}
        , notesInBeat{CalculateNotesInBeat()}
    {
    }

    void DrawSeparators(
        wxDC& dc, const wxRect& rect, const wxPen& beatSepearatorPen, const wxPen& barSeparatorPen) const
    {
        dc.SetPen(beatSepearatorPen);

        const auto [firstNote, lastNote] = GetBoundaries(
            rect, rect, noteWidth);

        for (auto noteIndex = firstNote; noteIndex < lastNote; ++noteIndex) {
            const auto position = GetPositionInRect(noteIndex, rect, noteDuration);

            if (position < rect.GetLeft() || position >= rect.GetRight()) {
                continue;
            }

            dc.SetPen(IsFirstInMajorTick(noteIndex) ? barSeparatorPen : beatSepearatorPen);
            dc.DrawLine(position, rect.GetTop(), position, rect.GetBottom());
        }
    }

    void DrawBackground(
        wxDC& dc, const wxRect& subRect, const wxRect& fullRect, const wxBrush& strongBeatBrush,
        const wxBrush& weakBeatBrush) const
    {
        if (!UseAlternatingColors()) {
            dc.SetBrush(strongBeatBrush);
            dc.DrawRectangle(subRect);
            return;
        }

        auto [firstIndex, lastIndex]
            =GetBoundaries(subRect, fullRect, noteWidth);

        // Make first index to be on notesInBeatBoundary
        firstIndex = (firstIndex / notesInBeat) * notesInBeat;

        const auto beatDuration = noteDuration;

        const auto top = fullRect.GetTop();
        const auto height = fullRect.GetHeight();

        bool strongBeat = (firstIndex / notesInBeat) % 2 == 0;
        for (auto index = firstIndex; index < lastIndex; index += notesInBeat, strongBeat = !strongBeat) {
            const auto left = std::max<int>(
                GetPositionInRect(index, fullRect, beatDuration),
                subRect.GetLeft());
            const auto right = std::min<int>(
                GetPositionInRect(index + notesInBeat, fullRect, beatDuration),
                subRect.GetRight());

            const auto& brush = strongBeat ? strongBeatBrush : weakBeatBrush;

            dc.SetBrush(brush);
            dc.DrawRectangle(left, top, right - left + 1, height);
        }
    }

private:
    int64_t CalculateNotesInBeat() const
    {
        if (UseAlternatingColors()) {
            return minorTick.lower / 4;
        }

        return 1;
    }

    bool IsFirstInMajorTick(int64_t noteIndex) const
    {
        const auto notesInMajorTick
            =minorTick.lower * majorTick.upper / majorTick.lower / minorTick.upper;

        if (notesInMajorTick == 0) {
            return false;
        }

        return noteIndex % notesInMajorTick == 0;
    }

    bool UseAlternatingColors() const
    {
        return minorTick.lower >= 4 && minorTick.upper == 1;
    }

    double GetPositionInRect(int64_t index, const wxRect& rect, double duration) const
    {
        return zoomInfo.TimeToPosition(index * duration) + rect.x;
    }

    std::pair<int64_t, int64_t> GetBoundaries(const wxRect& subRect, const wxRect& fullRect, double width) const
    {
        const auto offset = subRect.x - fullRect.x;

        return { std::floor(zoomInfo.GetAbsoluteOffset(offset) / width),
                 std::ceil(
                     zoomInfo.GetAbsoluteOffset(offset + subRect.GetWidth())
                     / width) };
    }

    BeatsFormat::Tick GetMinorTick() const
    {
        const auto& subdivision = beatsRulerFormat.GetSubdivision();

        auto tick = subdivision.minorMinor;

        auto minorMinorLength
            =zoomInfo.TimeRangeToPixelWidth(subdivision.minorMinor.duration);

        const auto nextSubdivision = subdivision.minor.duration == 0.0
                                     ? subdivision.major
                                     : subdivision.minor;

        while (minorMinorLength <= minSubdivisionWidth)
        {
            tick.lower /= 2;
            tick.duration *= 2.0;
            minorMinorLength *= 2.0;

            if (nextSubdivision.lower >= tick.lower) {
                return nextSubdivision;
            }
        }

        return tick;
    }
};
}

void TrackArt::DrawBackgroundWithSelection(
    TrackPanelDrawingContext& context, const wxRect& rect,
    const Channel& channel, const wxBrush& selBrush, const wxBrush& unselBrush,
    bool useSelection, bool useBeatsAlternateColor)
{
    const auto dc = &context.dc;
    const auto artist = TrackArtist::Get(context);
    const auto& selectedRegion = *artist->pSelectedRegion;
    const auto& zoomInfo = *artist->pZoomInfo;

    //MM: Draw background. We should optimize that a bit more.
    const double sel0 = useSelection ? selectedRegion.t0() : 0.0;
    const double sel1 = useSelection ? selectedRegion.t1() : 0.0;

    auto pTrack = dynamic_cast<const Track*>(&channel.GetChannelGroup());
    if (!pTrack) {
        return;
    }
    auto& track = *pTrack;
    BeatsGridlinePainter gridlinePainter(zoomInfo, GetProject(track));

    dc->SetPen(*wxTRANSPARENT_PEN);

    const auto& beatStrongBrush = artist->beatStrongBrush[useBeatsAlternateColor];
    const auto& beatStrongSelBrush = artist->beatStrongSelBrush[useBeatsAlternateColor];
    const auto& beatWeakBrush = artist->beatWeakBrush[useBeatsAlternateColor];
    const auto& beatWeakSelBrush = artist->beatWeakSelBrush[useBeatsAlternateColor];
    const auto& beatSepearatorPen = artist->beatSepearatorPen[useBeatsAlternateColor];
    const auto& barSepearatorPen = artist->barSepearatorPen[useBeatsAlternateColor];

    auto drawBgRect = [dc, &gridlinePainter, &rect](
        const wxBrush& regularBrush,
        const wxBrush& beatStrongBrush,
        const wxBrush& beatWeakBrush, const wxRect& subRect)
    {
        if (!gridlinePainter.enabled) {
            // Track not selected; just draw background
            dc->SetBrush(regularBrush);
            dc->DrawRectangle(subRect);
        } else {
            gridlinePainter.DrawBackground(
                *dc, subRect, rect, beatStrongBrush, beatWeakBrush);
        }
    };

    if (SyncLock::IsSelectedOrSyncLockSelected(track)) {
        // Rectangles before, within, after the selection
        wxRect before = rect;
        wxRect within = rect;
        wxRect after = rect;

        before.width = (int)(zoomInfo.TimeToPosition(sel0));
        if (before.GetRight() > rect.GetRight()) {
            before.width = rect.width;
        }

        if (before.width > 0) {
            drawBgRect(unselBrush, beatStrongBrush, beatWeakBrush, before);

            within.x = 1 + before.GetRight();
        }
        within.width = rect.x + (int)(zoomInfo.TimeToPosition(sel1)) - within.x - 1;

        if (within.GetRight() > rect.GetRight()) {
            within.width = 1 + rect.GetRight() - within.x;
        }

        // Bug 2389 - Selection can disappear
        // This handles case where no waveform is visible.
        if (within.width < 1) {
            within.width = 1;
        }

        if (within.width > 0) {
            if (track.GetSelected()) {
                drawBgRect(selBrush, beatStrongSelBrush, beatWeakSelBrush, within);
            } else {
                // Per condition above, track must be sync-lock selected
                drawBgRect(
                    unselBrush, beatStrongBrush, beatWeakBrush, within);
                DrawSyncLockTiles(context, within);
            }

            after.x = 1 + within.GetRight();
        } else {
            // `within` not drawn; start where it would have gone
            after.x = within.x;
        }

        after.width = 1 + rect.GetRight() - after.x;
        if (after.width > 0) {
            drawBgRect(
                unselBrush, beatStrongBrush, beatWeakBrush, after);
        }
    } else {
        drawBgRect(
            unselBrush, beatStrongBrush, beatWeakBrush, rect);
    }

    if (gridlinePainter.enabled) {
        gridlinePainter.DrawSeparators(*dc, rect, beatSepearatorPen, barSepearatorPen);
    }
}

void TrackArt::DrawCursor(TrackPanelDrawingContext& context,
                          const wxRect& rect, const Track* track)
{
    const auto dc = &context.dc;
    const auto artist = TrackArtist::Get(context);
    const auto& selectedRegion = *artist->pSelectedRegion;

    if (selectedRegion.isPoint()) {
        const auto& zoomInfo = *artist->pZoomInfo;
        auto x = static_cast<int>(zoomInfo.TimeToPosition(selectedRegion.t0(), rect.x));
        if (x >= rect.GetLeft() && x <= rect.GetRight()) {
            AColor::CursorColor(dc);
            AColor::Line(*dc, x, rect.GetTop(), x, rect.GetBottom());
        }
    }
}

void TrackArt::DrawSnapLines(wxDC* dc, wxInt64 snap0, wxInt64 snap1)
{
    AColor::SnapGuidePen(dc);
    if (snap0 >= 0) {
        AColor::Line(*dc, (int)snap0, 0, (int)snap0, 30000);
    }
    if (snap1 >= 0) {
        AColor::Line(*dc, (int)snap1, 0, (int)snap1, 30000);
    }
}
