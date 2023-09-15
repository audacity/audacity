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

namespace
{
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
   }
   else {
      if (!outer) {
         if (value >= 0.0) {
            value -= 0.5;
         }
         else {
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
   return (int) (value * (height - 1) + 0.5);
}

float FromDB(float value, double dBRange)
{
   if (value == 0)
      return 0;

   double sign = (value >= 0 ? 1 : -1);
   return DB_TO_LINEAR((fabs(value) * dBRange) - dBRange) * sign;
}

float ValueOfPixel(int yy, int height, bool offset,
   bool dB, double dBRange, float zoomMin, float zoomMax)
{
   wxASSERT(height > 0);
   // Map 0 to max and height - 1 (not height) to min
   float v =
      height == 1 ? (zoomMin + zoomMax) / 2 :
      zoomMax - (yy / (float)(height - 1)) * (zoomMax - zoomMin);
   if (offset) {
      if (v > 0.0)
         v += .5;
      else
         v -= .5;
   }

   if (dB)
      v = FromDB(v, dBRange);

   return v;
}

void TrackArt::DrawNegativeOffsetTrackArrows(
   TrackPanelDrawingContext &context, const wxRect &rect )
{
   auto &dc = context.dc;

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

   if (dc.GetTextExtent(text).GetWidth() <= maxWidth)
       return text;

   auto left = 0;
   //no need to check text + '...'
   auto right = static_cast<int>(text.Length() - 2);

   while (left <= right)
   {
      auto middle = (left + right) / 2;
      auto str = text.SubString(0, middle).Trim() + ellipsis;
      auto strWidth = dc.GetTextExtent(str).GetWidth();
      if (strWidth < maxWidth)
         //if left == right (== middle), then exit loop
         //with right equals to the last known index for which
         //strWidth < maxWidth
         left = middle + 1;
      else if (strWidth > maxWidth)
         //if right == left (== middle), then exit loop with
         //right equals to (left - 1), which is the last known
         //index for which (strWidth < maxWidth) or -1
         right = middle - 1;
      else
         return str;
   }
   if (right >= 0)
      return text.SubString(0, right).Trim() + ellipsis;

   return wxEmptyString;
}

wxRect TrackArt::DrawClipAffordance(wxDC& dc, const wxRect& rect, bool highlight, bool selected)
{
   //To make sure that roundings do not overlap each other
   auto clipFrameRadius = std::min(ClipFrameRadius, rect.width / 2);

   wxRect clipRect;
   bool hasClipRect = dc.GetClippingBox(clipRect);
   //Fix #1689: visual glitches appear on attempt to draw a rectangle
   //larger than 0x7FFFFFF pixels wide (value was discovered
   //by manual testing, and maybe depends on OS being used), but
   //it's very unlikely that such huge rectangle will be ever fully visible
   //on the screen, so we can safely reduce its size to be slightly larger than
   //clipping rectangle, and avoid that problem
   auto drawingRect = rect;
   if (hasClipRect)
   {
       //to make sure that rounding happends outside the clipping rectangle
       drawingRect.SetLeft(std::max(rect.GetLeft(), clipRect.GetLeft() - clipFrameRadius - 1));
       drawingRect.SetRight(std::min(rect.GetRight(), clipRect.GetRight() + clipFrameRadius + 1));
   }

   if (selected)
   {
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

   auto titleRect = hasClipRect ?
      //avoid drawing text outside the clipping rectangle if possible
      GetAffordanceTitleRect(rect.Intersect(clipRect)) :
      GetAffordanceTitleRect(rect);

   return titleRect;
}

namespace{
wxString GetPlaybackSpeedFullText(double clipStretchRatio)
{
   // clang-format off
   // We reckon that most of the time, a rounded percentage value is sufficient.
   // There are two exceptions:
   // - The clip is only slightly stretched such that the rounded value is 100.
   //   There should be an indicator if and only if the clip is stretched, this
   //   must be reliable. Yet showing "100%" would be confusing. Hence in that
   //   case we constrain the values to [99.1, ..., 99.9, 100.1,
   //   ..., 100.9], i.e., omitting 100.0.
   // - The clip is stretched so much that the playback speed is less than 1%.
   //   Make sure in that case that we never show 0%, but always at least 0.1%.
   // clang-format on

   const auto playbackSpeed = 100 / clipStretchRatio;
   wxString fullText;

   // We compare with .95 rather than 1, since playback speeds within [100.95,
   // 101) get rounded to 101.0 and (99, 99.05] to 99.0 by `wxString::Format`.
   // Let these be processed by the integer-display branch of this if statement
   // instead.
   if (fabs(playbackSpeed - 100.) < .95)
      // Never show 100.0%
      fullText = wxString::Format(
         "%.1f%% speed", playbackSpeed > 100 ?
                              std::max(playbackSpeed, 100.1) :
                              std::min(playbackSpeed, 99.9));
   else if (playbackSpeed < 1)
      // Never show 0.0%
      fullText =
         wxString::Format("%.1f%% speed", std::max(playbackSpeed, 0.1));
   else {
      const auto roundedPlaybackSpeed =
         static_cast<int>(std::round(playbackSpeed));
      fullText = wxString::Format("%d%% speed", roundedPlaybackSpeed);
   }
   return fullText;
}

enum class HAlign
{
   left,
   right
};

struct ClipTitle {
   const wxString text;
   const HAlign alignment;
};

std::optional<ClipTitle> DoDrawAudioTitle(
   wxDC& dc, const wxRect& titleRect, const wxString& title)
{
   if(titleRect.IsEmpty())
      return std::nullopt;
   const auto hAlign = wxTheApp->GetLayoutDirection() == wxLayout_RightToLeft ?
                          HAlign::right :
                          HAlign::left;
   if(title.empty())
      return ClipTitle { "", hAlign  };
   auto truncatedTitle = TrackArt::TruncateText(dc, title, titleRect.GetWidth());
   if (!truncatedTitle.empty())
   {
      dc.DrawLabel(
         truncatedTitle, titleRect,
         (hAlign == HAlign::left ? wxALIGN_LEFT : wxALIGN_RIGHT) |
            wxALIGN_CENTER_VERTICAL);
      return ClipTitle{ truncatedTitle, hAlign };
   }
   return std::nullopt;
}

}

bool TrackArt::DrawClipTitle(
   wxDC& dc, const wxRect& titleRect, const wxString& title)
{
   return DoDrawAudioTitle(dc, titleRect, title).has_value();
}

bool TrackArt::DrawAudioClipTitle(
   wxDC& dc, const wxRect& titleRect, const wxString& title,
   double clipStretchRatio)
{
   const auto clipTitle = DoDrawAudioTitle(dc, titleRect, title);
   if (!clipTitle.has_value())
      return false;
   if (!TimeAndPitchInterface::IsPassThroughMode(clipStretchRatio))
   {
      const auto fullText = GetPlaybackSpeedFullText(clipStretchRatio);
      constexpr auto minSpaceBetweenTitleAndSpeed = 12; // pixels
      const auto remainingWidth = std::max(
         titleRect.GetWidth() - dc.GetTextExtent(clipTitle->text).GetWidth() -
            minSpaceBetweenTitleAndSpeed,
         0);
      const auto truncatedText =
         TrackArt::TruncateText(dc, fullText, remainingWidth);
      if (truncatedText.find('%') != std::string::npos)
         // Only show if there is room for the % sign, or else it can be hard to
         // interpret.
         dc.DrawLabel(
            TrackArt::TruncateText(dc, fullText, remainingWidth), titleRect,
            (clipTitle->alignment == HAlign::left ? wxALIGN_RIGHT :
                                                    wxALIGN_LEFT) |
               wxALIGN_CENTER_VERTICAL);
   }
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
   if(selected)
   {
      if constexpr (ClipSelectionStrokeSize == 1)
      {
         AColor::UseThemeColour(&dc, -1, clrClipAffordanceStroke);
         AColor::Line(dc,
            clipRect.GetLeft() - ClipSelectionStrokeSize, clipRect.GetTop(),
            clipRect.GetLeft() - ClipSelectionStrokeSize, clipRect.GetBottom());
         AColor::Line(dc,
            clipRect.GetRight() + ClipSelectionStrokeSize, clipRect.GetTop(),
            clipRect.GetRight() + ClipSelectionStrokeSize, clipRect.GetBottom());
      }
      else if constexpr (ClipSelectionStrokeSize > 1)
      {
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
   TrackPanelDrawingContext &context, const wxRect &rect )
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
   if (xOffset < 0) xOffset += gridW;

   // Check if we're missing an extra column to the left (this can happen
   // because the tiles are bigger than the grid spacing)
   bool extraCol = false;
   if (syncLockBitmap.GetWidth() - gridW > xOffset) {
      extraCol = true;
      xOffset += gridW;
      blockX = (blockX - 1) % 5;
   }
   // Make sure blockX is non-negative
   if (blockX < 0) blockX += 5;

   int xx = 0;
   while (xx < rect.width) {
      int width = syncLockBitmap.GetWidth() - xOffset;
      if (xx + width > rect.width)
         width = rect.width - xx;

      //
      // Draw each row in this column
      //

      // Vertical position in the grid, modulo its period
      int blockY = (rect.y / gridH) % 5;

      // Amount to offset drawing of first row
      int yOffset = rect.y % gridH;
      if (yOffset < 0) yOffset += gridH;

      // Check if we're missing an extra row on top (this can happen because
      // the tiles are bigger than the grid spacing)
      bool extraRow = false;
      if (syncLockBitmap.GetHeight() - gridH > yOffset) {
         extraRow = true;
         yOffset += gridH;
         blockY = (blockY - 1) % 5;
      }
      // Make sure blockY is non-negative
      if (blockY < 0) blockY += 5;

      int yy = 0;
      while (yy < rect.height)
      {
         int height = syncLockBitmap.GetHeight() - yOffset;
         if (yy + height > rect.height)
            height = rect.height - yy;

         // AWD: draw blocks according to our pattern
         if ((blockX == 0 && blockY == 0) || (blockX == 2 && blockY == 1) ||
             (blockX == 4 && blockY == 2) || (blockX == 1 && blockY == 3) ||
             (blockX == 3 && blockY == 4))
         {

            // Do we need to get a sub-bitmap?
            if (width != syncLockBitmap.GetWidth() || height != syncLockBitmap.GetHeight()) {
               wxBitmap subSyncLockBitmap =
                  syncLockBitmap.GetSubBitmap(wxRect(xOffset, yOffset, width, height));
               dc->DrawBitmap(subSyncLockBitmap, rect.x + xx, rect.y + yy, true);
            }
            else {
               dc->DrawBitmap(syncLockBitmap, rect.x + xx, rect.y + yy, true);
            }
         }

         // Updates for next row
         if (extraRow) {
            // Second offset row, still at y = 0; no more extra rows
            yOffset -= gridH;
            extraRow = false;
         }
         else {
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
      }
      else {
         // Move on in x, no more offset rows
         xx += gridW - xOffset;
         xOffset = 0;
      }
      blockX = (blockX + 1) % 5;
   }
}

namespace
{
constexpr double minSubdivisionWidth = 12.0;

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


BeatsGridlinePainter(const ZoomInfo& zoomInfo, const Track& track) noexcept
       : zoomInfo { zoomInfo }
       , enabled { TimeDisplayModePreference.ReadEnum() ==
                   TimeDisplayMode::BeatsAndMeasures }
       , beatsRulerFormat { ProjectTimeRuler::Get(GetProject(track))
                               .GetBeatsFormat() }
       , majorTick { beatsRulerFormat.GetSubdivision().major }
       , minorTick { GetMinorTick() }
       , noteDuration { minorTick.duration }
       , noteWidth { zoomInfo.TimeRangeToPixelWidth(noteDuration) }
       , notesInBeat { CalculateNotesInBeat() }
   {
   }

void DrawSeparators (
   wxDC& dc, const wxRect& rect, const wxPen& beatSepearatorPen, const wxPen& barSeparatorPen) const
{
   dc.SetPen (beatSepearatorPen);

   const auto majorTick = beatsRulerFormat.GetSubdivision().major;
   const auto minorTick = GetMinorTick();

   const auto [firstNote, lastNote] = GetBoundaries(
      rect, rect, noteWidth);

   for (auto noteIndex = firstNote; noteIndex < lastNote; ++noteIndex)
   {
      const auto position = GetPositionInRect (noteIndex, rect, noteDuration);

      if (position < rect.GetLeft () || position >= rect.GetRight ())
         continue;

      dc.SetPen(IsFirstInMajorTick(noteIndex) ? barSeparatorPen : beatSepearatorPen);
      dc.DrawLine (position, rect.GetTop (), position, rect.GetBottom () + 1);
   }
}

void DrawBackground (
   wxDC& dc, const wxRect& subRect, const wxRect& fullRect, const wxBrush& strongBeatBrush,
   const wxBrush& weakBeatBrush) const
{
   if (!UseAlternatingColors ())
   {
      dc.SetBrush(strongBeatBrush);
      dc.DrawRectangle(subRect);
      return;
   }

   auto [firstIndex, lastIndex] =
      GetBoundaries (subRect, fullRect, noteWidth);

   // Make first index to be on notesInBeatBoundary
   firstIndex = (firstIndex / notesInBeat) * notesInBeat;

   const auto beatDuration = noteDuration;

   const auto top = fullRect.GetTop ();
   const auto height = fullRect.GetHeight ();

   bool strongBeat = (firstIndex / notesInBeat) % 2 == 0;
   for (auto index = firstIndex; index < lastIndex; index += notesInBeat, strongBeat = !strongBeat)
   {
      const auto left = std::max<int> (
         GetPositionInRect(index, fullRect, beatDuration),
         subRect.GetLeft());
      const auto right = std::min<int> (
         GetPositionInRect(index + notesInBeat, fullRect, beatDuration),
         subRect.GetRight());

      const auto& brush = strongBeat ? strongBeatBrush : weakBeatBrush;

      dc.SetBrush (brush);
      dc.DrawRectangle (left, top, right - left + 1, height);
   }
}

private:
   const AudacityProject& GetProject(const Track& track) const
   {
      // Track is expected to have owner
      assert(track.GetOwner());
      // TracList is expected to have owner
      assert(track.GetOwner()->GetOwner());

      return *track.GetOwner()->GetOwner();
   }

   int64_t CalculateNotesInBeat() const
   {
      if (UseAlternatingColors())
         return minorTick.lower / 4;

      return 1;
   }

   bool IsFirstInMajorTick(int64_t noteIndex) const
   {
      const auto notesInMajorTick =
         minorTick.lower * majorTick.upper / majorTick.lower / minorTick.upper;

      if (notesInMajorTick == 0)
         return false;

      return noteIndex % notesInMajorTick == 0;
   }

   bool UseAlternatingColors() const
   {
      return minorTick.lower >= 4 && minorTick.upper == 1;
   }

   double GetPositionInRect(int64_t index, const wxRect& rect, double duration) const
   {
      return zoomInfo.TimeToPosition(index * duration) + rect.x + 1;
   }

   std::pair<int64_t, int64_t> GetBoundaries(const wxRect& subRect, const wxRect& fullRect, double width) const
   {
      const auto offset = subRect.x - fullRect.x;

      return { std::floor(zoomInfo.GetAbsoluteOffset(offset) / width),
               std::ceil(
                  zoomInfo.GetAbsoluteOffset(offset + subRect.GetWidth()) /
                  width) };
   }

   BeatsFormat::Tick GetMinorTick() const
   {
      const auto& subdivision = beatsRulerFormat.GetSubdivision();

      auto tick = subdivision.minorMinor;

      auto minorMinorLength =
         zoomInfo.TimeRangeToPixelWidth(subdivision.minorMinor.duration);

      const auto nextSubdivision = subdivision.minor.duration == 0.0 ?
                                subdivision.major :
                                subdivision.minor;

      while (minorMinorLength <= minSubdivisionWidth)
      {
         tick.lower /= 2;
         tick.duration *= 2.0;
         minorMinorLength *= 2.0;

         if (nextSubdivision.lower >= tick.lower)
            return nextSubdivision;
      }

      return tick;
   }
};
}

void TrackArt::DrawBackgroundWithSelection(
   TrackPanelDrawingContext &context, const wxRect &rect,
   const Track *track, const wxBrush &selBrush, const wxBrush &unselBrush,
   bool useSelection)
{
   const auto dc = &context.dc;
   const auto artist = TrackArtist::Get( context );
   const auto &selectedRegion = *artist->pSelectedRegion;
   const auto& zoomInfo = *artist->pZoomInfo;


   //MM: Draw background. We should optimize that a bit more.
   const double sel0 = useSelection ? selectedRegion.t0() : 0.0;
   const double sel1 = useSelection ? selectedRegion.t1() : 0.0;

   BeatsGridlinePainter gridlinePainter(zoomInfo, *track);

   dc->SetPen(*wxTRANSPARENT_PEN);

   auto drawBgRect = [dc, &gridlinePainter, artist, &rect](
                        const wxBrush& regularBrush,
                        const wxBrush& beatStrongBrush,
                        const wxBrush& beatWeakBrush, const wxRect& subRect)
   {
      if (!gridlinePainter.enabled)
      {
         // Track not selected; just draw background
         dc->SetBrush(regularBrush);
         dc->DrawRectangle(subRect);
      }
      else
      {
         gridlinePainter.DrawBackground(
            *dc, subRect, rect, beatStrongBrush, beatWeakBrush);
      }
   };

   if (SyncLock::IsSelectedOrSyncLockSelected(track))
   {
      // Rectangles before, within, after the selection
      wxRect before = rect;
      wxRect within = rect;
      wxRect after = rect;

      before.width = (int)(zoomInfo.TimeToPosition(sel0) );
      if (before.GetRight() > rect.GetRight()) {
         before.width = rect.width;
      }

      if (before.width > 0) {
         drawBgRect(unselBrush, artist->beatStrongBrush, artist->beatWeakBrush, before);

         within.x = 1 + before.GetRight();
      }
      within.width = rect.x + (int)(zoomInfo.TimeToPosition(sel1) ) - within.x -1;

      if (within.GetRight() > rect.GetRight()) {
         within.width = 1 + rect.GetRight() - within.x;
      }

      // Bug 2389 - Selection can disappear
      // This handles case where no waveform is visible.
      if (within.width < 1)
         within.width = 1;

      if (within.width > 0) {
         if (track->GetSelected()) {
            drawBgRect(selBrush, artist->beatStrongSelBrush, artist->beatWeakSelBrush, within);
         }
         else {
            // Per condition above, track must be sync-lock selected
            drawBgRect(
               unselBrush, artist->beatStrongBrush, artist->beatWeakBrush, within);
            DrawSyncLockTiles( context, within );
         }

         after.x = 1 + within.GetRight();
      }
      else {
         // `within` not drawn; start where it would have gone
         after.x = within.x;
      }

      after.width = 1 + rect.GetRight() - after.x;
      if (after.width > 0)
         drawBgRect(
            unselBrush, artist->beatStrongBrush, artist->beatWeakBrush, after);
   }
   else
   {
      drawBgRect(
         unselBrush, artist->beatStrongBrush, artist->beatWeakBrush, rect);
   }

   if (gridlinePainter.enabled)
      gridlinePainter.DrawSeparators(*dc, rect, artist->beatSepearatorPen, artist->barSepearatorPen);
}

void TrackArt::DrawCursor(TrackPanelDrawingContext& context,
   const wxRect& rect, const Track* track)
{
   const auto dc = &context.dc;
   const auto artist = TrackArtist::Get(context);
   const auto& selectedRegion = *artist->pSelectedRegion;

   if (selectedRegion.isPoint())
   {
       const auto& zoomInfo = *artist->pZoomInfo;
       auto x = static_cast<int>(zoomInfo.TimeToPosition(selectedRegion.t0(), rect.x));
       if (x >= rect.GetLeft() && x <= rect.GetRight())
       {
          AColor::CursorColor(dc);
          AColor::Line(*dc, x, rect.GetTop(), x, rect.GetBottom());
       }
   }
}
