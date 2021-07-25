/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackArtist.cpp

  Dominic Mazzoni


*******************************************************************//*!

\class TrackArtist
\brief   This class handles the actual rendering of WaveTracks (both
  waveforms and spectra), NoteTracks, LabelTracks and TimeTracks.

  It's actually a little harder than it looks, because for
  waveforms at least it needs to cache the samples that are
  currently on-screen.

<b>How Audacity Redisplay Works \n
 Roger Dannenberg</b> \n
Oct 2010 \n

In my opinion, the bitmap should contain only the waveform, note, and
label images along with gray selection highlights. The track info
(sliders, buttons, title, etc.), track selection highlight, cursor, and
indicator should be drawn in the normal way, and clipping regions should
be used to avoid excessive copying of bitmaps (say, when sliders move),
or excessive redrawing of track info widgets (say, when scrolling occurs).
This is a fairly tricky code change since it requires careful specification
of what and where redraw should take place when any state changes. One
surprising finding is that NoteTrack display is slow compared to WaveTrack
display. Each note takes some time to gather attributes and select colors,
and while audio draws two amplitudes per horizontal pixels, large MIDI
scores can have more notes than horizontal pixels. This can make slider
changes very sluggish, but this can also be a problem with many
audio tracks.

*//*******************************************************************/


#include "TrackArtist.h"



#include "AColor.h"
#include "AllThemeResources.h"
#include "prefs/GUIPrefs.h"
#include "Theme.h"
#include "Track.h"
#include "TrackPanelDrawingContext.h"
#include "ViewInfo.h"

#include "prefs/GUISettings.h"
#include "prefs/TracksPrefs.h"

#include <wx/dc.h>

//Thickness of the clip frame outline, shown when clip is dragged
static constexpr int ClipSelectionStrokeSize{ 1 };//px

TrackArtist::TrackArtist( TrackPanel *parent_ )
   : parent( parent_ )
{
   mdBrange = ENV_DB_RANGE;
   mShowClipping = false;
   mSampleDisplay = 1;// Stem plots by default.

   SetColours(0);

   UpdatePrefs();
}

TrackArtist::~TrackArtist()
{
}

TrackArtist * TrackArtist::Get( TrackPanelDrawingContext &context )
{
   return static_cast< TrackArtist* >( context.pUserData );
}

void TrackArtist::SetColours( int iColorIndex)
{
   theTheme.SetBrushColour( blankBrush,      clrBlank );
   theTheme.SetBrushColour( unselectedBrush, clrUnselected);
   theTheme.SetBrushColour( selectedBrush,   clrSelected);
   theTheme.SetBrushColour( sampleBrush,     clrSample);
   theTheme.SetBrushColour( selsampleBrush,  clrSelSample);
   theTheme.SetBrushColour( dragsampleBrush, clrDragSample);
   theTheme.SetBrushColour( blankSelectedBrush, clrBlankSelected);

   theTheme.SetPenColour(   blankPen,        clrBlank);
   theTheme.SetPenColour(   unselectedPen,   clrUnselected);
   theTheme.SetPenColour(   selectedPen,     clrSelected);
   theTheme.SetPenColour(   muteSamplePen,   clrMuteSample);
   theTheme.SetPenColour(   odProgressDonePen, clrProgressDone);
   theTheme.SetPenColour(   odProgressNotYetPen, clrProgressNotYet);
   theTheme.SetPenColour(   shadowPen,       clrShadow);
   theTheme.SetPenColour(   clippedPen,      clrClipped);
   theTheme.SetPenColour(   muteClippedPen,  clrMuteClipped);
   theTheme.SetPenColour(   blankSelectedPen,clrBlankSelected);

   theTheme.SetPenColour(   selsamplePen,    clrSelSample);
   theTheme.SetPenColour(   muteRmsPen,      clrMuteRms);

   switch( iColorIndex %4 )
   {
      default:
      case 0:
         theTheme.SetPenColour(   samplePen,       clrSample);
         theTheme.SetPenColour(   rmsPen,          clrRms);
         break;
      case 1: // RED
         samplePen.SetColour( wxColor( 160,10,10 ) );
         rmsPen.SetColour( wxColor( 230,80,80 ) );
         break;
      case 2: // GREEN
         samplePen.SetColour( wxColor( 35,110,35 ) );
         rmsPen.SetColour( wxColor( 75,200,75 ) );
         break;
      case 3: //BLACK
         samplePen.SetColour( wxColor( 0,0,0 ) );
         rmsPen.SetColour( wxColor( 100,100,100 ) );
         break;

   }
}

/// Takes a value between min and max and returns a value between
/// height and 0
/// \todo  Should this function move int GuiWaveTrack where it can
/// then use the zoomMin, zoomMax and height values without having
/// to have them passed in to it??
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


#ifdef USE_MIDI
#endif // USE_MIDI


void TrackArtist::UpdateSelectedPrefs( int id )
{
   if( id == ShowClippingPrefsID())
      mShowClipping = gPrefs->Read(wxT("/GUI/ShowClipping"), mShowClipping);
   if( id == ShowTrackNameInWaveformPrefsID())
      mbShowTrackNameInTrack = gPrefs->ReadBool(wxT("/GUI/ShowTrackNameInWaveform"), false);
}

void TrackArtist::UpdatePrefs()
{
   mdBrange = gPrefs->Read(ENV_DB_KEY, mdBrange);
   mSampleDisplay = TracksPrefs::SampleViewChoice();

   UpdateSelectedPrefs( ShowClippingPrefsID() );
   UpdateSelectedPrefs( ShowTrackNameInWaveformPrefsID() );

   SetColours(0);
}

void TrackArt::DrawClipAffordance(wxDC& dc, const wxRect& rect, bool highlight, bool selected)
{
   if (selected)
   {
      wxRect strokeRect{
         rect.x - ClipSelectionStrokeSize,
         rect.y,
         rect.width + ClipSelectionStrokeSize * 2,
         rect.height + ClipFrameRadius };
      dc.SetBrush(*wxTRANSPARENT_BRUSH);
      AColor::UseThemeColour(&dc, clrClipAffordanceStroke, clrClipAffordanceStroke);
      dc.DrawRoundedRectangle(strokeRect, ClipFrameRadius);
   }
   AColor::UseThemeColour(&dc, highlight ? clrClipAffordanceActiveBrush : clrClipAffordanceInactiveBrush, clrClipAffordanceOutlinePen);
   dc.DrawRoundedRectangle(wxRect(rect.x, rect.y + ClipSelectionStrokeSize, rect.width, rect.height + ClipFrameRadius), ClipFrameRadius);
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

void TrackArt::DrawBackgroundWithSelection(
   TrackPanelDrawingContext &context, const wxRect &rect,
   const Track *track, const wxBrush &selBrush, const wxBrush &unselBrush,
   bool useSelection)
{
   const auto dc = &context.dc;
   const auto artist = TrackArtist::Get( context );
   const auto &selectedRegion = *artist->pSelectedRegion;
   const auto &zoomInfo = *artist->pZoomInfo;

   //MM: Draw background. We should optimize that a bit more.
   const double sel0 = useSelection ? selectedRegion.t0() : 0.0;
   const double sel1 = useSelection ? selectedRegion.t1() : 0.0;

   dc->SetPen(*wxTRANSPARENT_PEN);
   if (track->GetSelected() || track->IsSyncLockSelected())
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
         dc->SetBrush(unselBrush);
         dc->DrawRectangle(before);

         within.x = 1 + before.GetRight();
      }
      within.width = rect.x + (int)(zoomInfo.TimeToPosition(sel1) ) - within.x -1;

      if (within.GetRight() > rect.GetRight()) {
         within.width = 1 + rect.GetRight() - within.x;
      }

      // Bug 2389 - Selection can disappear
      // This handles case where no waveform is visible.
      if (within.width < 1)
      {
         within.width = 1;
      }

      if (within.width > 0) {
         if (track->GetSelected()) {
            dc->SetBrush(selBrush);
            dc->DrawRectangle(within);
         }
         else {
            // Per condition above, track must be sync-lock selected
            dc->SetBrush(unselBrush);
            dc->DrawRectangle(within);
            DrawSyncLockTiles( context, within );
         }

         after.x = 1 + within.GetRight();
      }
      else {
         // `within` not drawn; start where it would have gone
         after.x = within.x;
      }

      after.width = 1 + rect.GetRight() - after.x;
      if (after.width > 0) {
         dc->SetBrush(unselBrush);
         dc->DrawRectangle(after);
      }
   }
   else
   {
      // Track not selected; just draw background
      dc->SetBrush(unselBrush);
      dc->DrawRectangle(rect);
   }
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

