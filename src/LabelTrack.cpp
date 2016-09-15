/**********************************************************************

  Audacity: A Digital Audio Editor

  LabelTrack.cpp

  Dominic Mazzoni
  James Crook
  Jun Wan

*******************************************************************//**

\class LabelTrack
\brief A LabelTrack is a Track that holds labels (LabelStruct).

These are used to annotate a waveform.
Each label has a start time and an end time.
The text of the labels is editable and the
positions of the end points are draggable.

*//****************************************************************//**

\class LabelStruct
\brief A LabelStruct holds information for ONE label in a LabelTrack

LabelStruct also has label specific functions, mostly functions
for drawing different aspects of the label and its text box.

*//*******************************************************************/

#include "Audacity.h"
#include "LabelTrack.h"

#include <stdio.h>
#include <algorithm>

#include <wx/bitmap.h>
#include <wx/brush.h>
#include <wx/clipbrd.h>
#include <wx/dataobj.h>
#include <wx/dc.h>
#include <wx/dcclient.h>
#include <wx/event.h>
#include <wx/intl.h>
#include <wx/log.h>
#include <wx/msgdlg.h>
#include <wx/pen.h>
#include <wx/string.h>
#include <wx/textfile.h>
#include <wx/tokenzr.h>
#include <wx/utils.h>

#include "AudioIO.h"
#include "DirManager.h"
#include "Internat.h"
#include "Prefs.h"
#include "Theme.h"
#include "AllThemeResources.h"
#include "AColor.h"
#include "Project.h"
#include "TrackArtist.h"
#include "TrackPanel.h"
#include "UndoManager.h"
#include "commands/CommandManager.h"

#include "effects/TimeWarper.h"

enum
{
   OnCutSelectedTextID = 1,      // OSX doesn't like a 0 menu id
   OnCopySelectedTextID,
   OnPasteSelectedTextID,
   OnDeleteSelectedLabelID,
   OnEditSelectedLabelID,
};

wxFont LabelTrack::msFont;

// static member variables.
bool LabelTrack::mbGlyphsReady=false;

/// We have several variants of the icons (highlighting).
/// The icons are draggable, and you can drag one boundary
/// or all boundaries at the same timecode depending on whether you
/// click the centre (for all) or the arrow part (for one).
/// Currently we have twelve variants but we're only using six.
wxBitmap LabelTrack::mBoundaryGlyphs[ NUM_GLYPH_CONFIGS * NUM_GLYPH_HIGHLIGHTS ];
int LabelTrack::mIconHeight;
int LabelTrack::mIconWidth;
int LabelTrack::mTextHeight;

int LabelTrack::mFontHeight=-1;

LabelTrack::Holder TrackFactory::NewLabelTrack()
{
   return std::make_unique<LabelTrack>(mDirManager);
}

LabelTrack::LabelTrack(const std::shared_ptr<DirManager> &projDirManager):
   Track(projDirManager),
   mbHitCenter(false),
   mOldEdge(-1),
   mSelIndex(-1),
   mMouseOverLabelLeft(-1),
   mMouseOverLabelRight(-1),
   mRestoreFocus(-1),
   mClipLen(0.0),
   mIsAdjustingLabel(false)
{
   SetDefaultName(_("Label Track"));
   SetName(GetDefaultName());

   // Label tracks are narrow
   // Default is to allow two rows so that NEW users get the
   // idea that labels can 'stack' when they would overlap.
   SetHeight(73);

   ResetFont();
   CreateCustomGlyphs();

   // reset flags
   ResetFlags();
}

LabelTrack::LabelTrack(const LabelTrack &orig) :
   Track(orig),
   mbHitCenter(false),
   mOldEdge(-1),
   mSelIndex(-1),
   mMouseOverLabelLeft(-1),
   mMouseOverLabelRight(-1),
   mClipLen(0.0),
   mIsAdjustingLabel(false)
{
   for (auto &original: orig.mLabels) {
      LabelStruct l { original.selectedRegion, original.title };
      mLabels.push_back(l);
   }
   mSelIndex = orig.mSelIndex;

   // reset flags
   ResetFlags();
}

LabelTrack::~LabelTrack()
{
}

void LabelTrack::SetOffset(double dOffset)
{
   for (auto &labelStruct: mLabels)
      labelStruct.selectedRegion.move(dOffset);
}

bool LabelTrack::Clear(double b, double e)
{
   // May DELETE labels, so use subscripts to iterate
   for (size_t i = 0; i < mLabels.size(); ++i) {
      auto &labelStruct = mLabels[i];
      LabelStruct::TimeRelations relation =
                        labelStruct.RegionRelation(b, e, this);
      if (relation == LabelStruct::BEFORE_LABEL)
         labelStruct.selectedRegion.move(- (e-b));
      else if (relation == LabelStruct::SURROUNDS_LABEL) {
         DeleteLabel( i );
         --i;
      }
      else if (relation == LabelStruct::ENDS_IN_LABEL)
         labelStruct.selectedRegion.setTimes(
            b,
            labelStruct.getT1() - (e - b));
      else if (relation == LabelStruct::BEGINS_IN_LABEL)
         labelStruct.selectedRegion.setT1(b);
      else if (relation == LabelStruct::WITHIN_LABEL)
         labelStruct.selectedRegion.moveT1( - (e-b));
   }

   return true;
}

#if 0
//used when we want to use clear only on the labels
bool LabelTrack::SplitDelete(double b, double e)
{
   // May DELETE labels, so use subscripts to iterate
   for (size_t i = 0, len = mLabels.size(); i < len; ++i) {
      auto &labelStruct = mLabels[i];
      LabelStruct::TimeRelations relation =
                        labelStruct.RegionRelation(b, e, this);
      if (relation == LabelStruct::SURROUNDS_LABEL) {
         DeleteLabel(i);
         --i;
      }
      else if (relation == LabelStruct::WITHIN_LABEL)
         labelStruct.selectedRegion.moveT1( - (e-b));
      else if (relation == LabelStruct::ENDS_IN_LABEL)
         labelStruct.selectedRegion.setT0(e);
      else if (relation == LabelStruct::BEGINS_IN_LABEL)
         labelStruct.selectedRegion.setT1(b);
   }

   return true;
}
#endif

void LabelTrack::ShiftLabelsOnInsert(double length, double pt)
{
   for (auto &labelStruct: mLabels) {
      LabelStruct::TimeRelations relation =
                        labelStruct.RegionRelation(pt, pt, this);

      if (relation == LabelStruct::BEFORE_LABEL)
         labelStruct.selectedRegion.move(length);
      else if (relation == LabelStruct::WITHIN_LABEL)
         labelStruct.selectedRegion.moveT1(length);
   }
}

void LabelTrack::ChangeLabelsOnReverse(double b, double e)
{
   for (auto &labelStruct: mLabels) {
      if (labelStruct.RegionRelation(b, e, this) ==
                                    LabelStruct::SURROUNDS_LABEL)
      {
         double aux     = b + (e - labelStruct.getT1());
         labelStruct.selectedRegion.setTimes(
            aux,
            e - (labelStruct.getT0() - b));
      }
   }
   SortLabels();
}

void LabelTrack::ScaleLabels(double b, double e, double change)
{
   for (auto &labelStruct: mLabels) {
      labelStruct.selectedRegion.setTimes(
         AdjustTimeStampOnScale(labelStruct.getT0(), b, e, change),
         AdjustTimeStampOnScale(labelStruct.getT1(), b, e, change));
   }
}

double LabelTrack::AdjustTimeStampOnScale(double t, double b, double e, double change)
{
//t is the time stamp we'll be changing
//b and e are the selection start and end

   if (t < b){
      return t;
   }else if (t > e){
      double shift = (e-b)*change - (e-b);
      return t + shift;
   }else{
      double shift = (t-b)*change - (t-b);
      return t + shift;
   }
}

// Move the labels in the track according to the given TimeWarper.
// (If necessary this could be optimised by ignoring labels that occur before a
// specified time, as in most cases they don't need to move.)
void LabelTrack::WarpLabels(const TimeWarper &warper) {
   for (auto &labelStruct: mLabels) {
      labelStruct.selectedRegion.setTimes(
         warper.Warp(labelStruct.getT0()),
         warper.Warp(labelStruct.getT1()));
   }
}

void LabelTrack::ResetFlags()
{
   mInitialCursorPos = 1;
   mCurrentCursorPos = 1;
   mRightDragging = false;
   mDrawCursor = false;
}

wxFont LabelTrack::GetFont(const wxString &faceName, int size)
{
   wxFontEncoding encoding;
   if (faceName == wxT(""))
      encoding = wxFONTENCODING_DEFAULT;
   else
      encoding = wxFONTENCODING_SYSTEM;
   return wxFont(size, wxFONTFAMILY_DEFAULT, wxFONTSTYLE_NORMAL,
                 wxFONTWEIGHT_NORMAL, false, faceName, encoding);
}

void LabelTrack::ResetFont()
{
   mFontHeight = -1;
   wxString facename = gPrefs->Read(wxT("/GUI/LabelFontFacename"), wxT(""));
   int size = gPrefs->Read(wxT("/GUI/LabelFontSize"), DefaultFontSize);
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
void LabelTrack::ComputeTextPosition(const wxRect & r, int index) const
{
   auto &labelStruct = mLabels[index];

   // xExtra is extra space
   // between the text and the endpoints.
   const int xExtra=mIconWidth;
   int x     = labelStruct.x;  // left endpoint
   int x1    = labelStruct.x1; // right endpoint.
   int width = labelStruct.width;

   int xText; // This is where the text will end up.

   // Will the text all fit at this zoom?
   bool bTooWideForScreen = width > (r.width-2*xExtra);
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

   if( bSimpleCentering )
   {
      // Center text between the two end points.
      xText = (x+x1-width)/2;
   }
   else
   {
      // Calculate xText position to make text line
      // scroll sideways evenly as r moves right.

      // xText is a linear function of r.x.
      // These variables are used to compute that function.
      int rx0,rx1,xText0,xText1;

      // Since we will be using a linear function,
      // we should blend smoothly between left and right
      // aligned text as r, the 'viewport' moves.
      if( bTooWideForScreen )
      {
         rx0=x;           // when viewport at label start.
         xText0=x+xExtra; // text aligned left.
         rx1=x1-r.width;  // when viewport end at label end
         xText1=x1-(width+xExtra); // text aligned right.
      }
      else
      {
         // when label start + width + extra spacing at viewport end..
         rx0=x-r.width+width+2*xExtra;
         // ..text aligned left.
         xText0=x+xExtra;
         // when viewport start + width + extra spacing at label end..
         rx1=x1-(width+2*xExtra);
         // ..text aligned right.
         xText1=x1-(width+xExtra);
      }

      if( rx1 > rx0 ) // Avoid divide by zero case.
      {
         // Compute the blend between left and right aligned.

         // Don't use:
         //
         // xText = xText0 + ((xText1-xText0)*(r.x-rx0))/(rx1-rx0);
         //
         // The problem with the above is that it integer-oveflows at
         // high zoom.

         // Instead use:
         xText = xText0 + (int)((xText1-xText0)*(((float)(r.x-rx0))/(rx1-rx0)));
      }
      else
      {
         // Avoid divide by zero by reverting to
         // simple centering.
         //
         // We could also fall into this case if x and x1
         // are swapped, in which case we'll end up
         // left aligned anyway because code later on
         // will catch that.
         xText = (x+x1-width)/2;
      }
   }

   // Is the text now appearing partly outside r?
   bool bOffLeft = xText < r.x+xExtra;
   bool bOffRight = xText > r.x+r.width-width-xExtra;

   // IF any part of the text is offscreen
   // THEN we may bring it back.
   if( bOffLeft == bOffRight )
   {
      //IF both sides on screen, THEN nothing to do.
      //IF both sides off screen THEN don't do
      //anything about it.
      //(because if we did, you'd never get to read
      //all the text by scrolling).
   }
   else if( bOffLeft != bTooWideForScreen)
   {
      // IF we're off on the left, OR we're
      // too wide for the screen and off on the right
      // (only) THEN align left.
      xText = r.x+xExtra;
   }
   else
   {
      // We're off on the right, OR we're
      // too wide and off on the left (only)
      // SO align right.
      xText =r.x+r.width-width-xExtra;
   }

   // But if we've taken the text out from its endpoints
   // we must move it back so that it's between the endpoints.

   // We test the left end point last because the
   // text might not even fit between the endpoints (at this
   // zoom factor), and in that case we'd like to position
   // the text at the left end point.
   if( xText > (x1-width-xExtra))
      xText=(x1-width-xExtra);
   if( xText < x+xExtra )
      xText=x+xExtra;

   labelStruct.xText = xText;
}

/// ComputeLayout determines which row each label
/// should be placed on, and reserves space for it.
/// Function assumes that the labels are sorted.
void LabelTrack::ComputeLayout(const wxRect & r, const ZoomInfo &zoomInfo) const
{
   int xUsed[MAX_NUM_ROWS];

   int iRow;
   // Rows are the 'same' height as icons or as the text,
   // whichever is taller.
   const int yRowHeight = wxMax(mTextHeight,mIconHeight)+3;// pixels.
   // Extra space at end of rows.
   // We allow space for one half icon at the start and two
   // half icon widths for extra x for the text frame.
   // [we don't allow half a width space for the end icon since it is
   // allowed to be obscured by the text].
   const int xExtra= (3 * mIconWidth)/2;

   const int nRows = wxMin((r.height / yRowHeight) + 1, MAX_NUM_ROWS);
   // Initially none of the rows have been used.
   // So set a value that is less than any valid value.
   {
      const int xStart = zoomInfo.TimeToPosition(0.0, r.x) - 100;
      for (auto &x : xUsed)
         x = xStart;
   }
   int nRowsUsed=0;

   { int i = -1; for (auto &labelStruct : mLabels) { ++i;
      const int x = zoomInfo.TimeToPosition(labelStruct.getT0(), r.x);
      const int x1 = zoomInfo.TimeToPosition(labelStruct.getT1(), r.x);
      int y = r.y;

      labelStruct.x=x;
      labelStruct.x1=x1;
      labelStruct.y=-1;// -ve indicates nothing doing.
      iRow=0;
      // Our first preference is a row that ends where we start.
      // (This is to encourage merging of adjacent label boundaries).
      while( (iRow<nRowsUsed) && (xUsed[iRow] != x ))
         iRow++;
      // IF we didn't find one THEN
      // find any row that can take a span starting at x.
      if( iRow >= nRowsUsed )
      {
         iRow=0;
         while( (iRow<nRows) && (xUsed[iRow] > x ))
            iRow++;
      }
      // IF we found such a row THEN record a valid position.
      if( iRow<nRows )
      {
         // Possibly update the number of rows actually used.
         if( iRow >= nRowsUsed )
            nRowsUsed=iRow+1;
         // Record the position for this label
         y= r.y + iRow * yRowHeight +(yRowHeight/2)+1;
         labelStruct.y=y;
         // On this row we have used up to max of end marker and width.
         // Plus also allow space to show the start icon and
         // some space for the text frame.
         xUsed[iRow]=x+labelStruct.width+xExtra;
         if( xUsed[iRow] < x1 ) xUsed[iRow]=x1;
         ComputeTextPosition( r, i );
      }
   }}
}

LabelStruct::LabelStruct(const SelectedRegion &region,
                         const wxString& aTitle)
: selectedRegion(region)
, title(aTitle)
{
   updated = false;
   width = 0;
   x = 0;
   x1 = 0;
   xText = 0;
   y = 0;
}

LabelStruct::LabelStruct(const SelectedRegion &region,
                         double t0, double t1,
                         const wxString& aTitle)
: selectedRegion(region)
, title(aTitle)
{
   // Overwrite the times
   selectedRegion.setTimes(t0, t1);

   updated = false;
   width = 0;
   x = 0;
   x1 = 0;
   xText = 0;
   y = 0;
}

/// Draw vertical lines that go exactly through the position
/// of the start or end of a label.
///   @param  dc the device context
///   @param  r  the LabelTrack rectangle.
void LabelStruct::DrawLines(wxDC & dc, const wxRect & r) const
{
   // How far out from the centre line should the vertical lines
   // start, i.e. what is the y position of the icon?
   // We adjust this so that the line encroaches on the icon
   // slightly (there is white space in the design).
   const int yIconStart = y - (LabelTrack::mIconHeight /2)+1+(LabelTrack::mTextHeight+3)/2;
   const int yIconEnd   = yIconStart + LabelTrack::mIconHeight-2;

   // If y is positive then it is the center line for the
   // Label.
   if( y >= 0 )
   {
      if((x  >= r.x) && (x  <= (r.x+r.width)))
      {
         // Draw line above and below left dragging widget.
         AColor::Line(dc, x, r.y,  x, yIconStart - 1);
         AColor::Line(dc, x, yIconEnd, x, r.y + r.height);
      }
      if((x1 >= r.x) && (x1 <= (r.x+r.width)))
      {
         // Draw line above and below right dragging widget.
         AColor::Line(dc, x1, r.y,  x1, yIconStart - 1);
         AColor::Line(dc, x1, yIconEnd, x1, r.y + r.height);
      }
   }
   else
   {
      // Draw the line, even though the widget is off screen
      AColor::Line(dc, x, r.y,  x, r.y + r.height);
      AColor::Line(dc, x1, r.y,  x1, r.y + r.height);
   }
}

/// DrawGlyphs draws the wxIcons at the start and end of a label.
///   @param  dc the device context
///   @param  r  the LabelTrack rectangle.
void LabelStruct::DrawGlyphs
   (wxDC & dc, const wxRect & r, int GlyphLeft, int GlyphRight) const
{
   const int xHalfWidth=LabelTrack::mIconWidth/2;
   const int yStart=y-LabelTrack::mIconHeight/2+(LabelTrack::mTextHeight+3)/2;

   // If y == -1, nothing to draw
   if( y == -1 )
      return;

   if((x  >= r.x) && (x  <= (r.x+r.width)))
      dc.DrawBitmap(LabelTrack::GetGlyph(GlyphLeft), x-xHalfWidth,yStart, true);
   // The extra test commented out here would suppress right hand markers
   // when they overlap the left hand marker (e.g. zoomed out) or to the left.
   if((x1 >= r.x) && (x1 <= (r.x+r.width)) /*&& (x1>x+LabelTrack::mIconWidth)*/)
      dc.DrawBitmap(LabelTrack::GetGlyph(GlyphRight), x1-xHalfWidth,yStart, true);
}

/// Draw the text of the label and also draw
/// a long thin rectangle for its full extent
/// from x to x1 and a rectangular frame
/// behind the text itself.
///   @param  dc the device context
///   @param  r  the LabelTrack rectangle.
void LabelStruct::DrawText(wxDC & dc, const wxRect & r) const
{
   //If y is positive then it is the center line for the
   //text we are about to draw.
   //if it isn't, nothing to draw.

   if( y == -1 )
      return;

   // Draw frame for the text...
   // We draw it half an icon width left of the text itself.
   {
      const int xStart=wxMax(r.x,xText-LabelTrack::mIconWidth/2);
      const int xEnd=wxMin(r.x+r.width,xText+width+LabelTrack::mIconWidth/2);
      const int xWidth = xEnd-xStart;

      if( (xStart < (r.x+r.width)) && (xEnd > r.x) && (xWidth>0))
      {
         // Now draw the text itself.
         dc.DrawText(title, xText, y-LabelTrack::mTextHeight/2);
      }
   }

}

void LabelStruct::DrawTextBox(wxDC & dc, const wxRect & r) const
{
   //If y is positive then it is the center line for the
   //text we are about to draw.
   const int yBarHeight=3;
   const int yFrameHeight = LabelTrack::mTextHeight+3;
   const int xBarShorten  = LabelTrack::mIconWidth+4;
   if( y == -1 )
      return;

   {
      const int xStart=wxMax(r.x,x+xBarShorten/2);
      const int xEnd=wxMin(r.x+r.width,x1-xBarShorten/2);
      const int xWidth = xEnd-xStart;

      if( (xStart < (r.x+r.width)) && (xEnd > r.x) && (xWidth>0))
      {

         wxRect bar( xStart,y-yBarHeight/2+yFrameHeight/2,
            xWidth,yBarHeight);
         if( x1 > x+xBarShorten )
            dc.DrawRectangle(bar);
      }
   }

   // In drawing the bar and the frame, we compute the clipping
   // to the viewport ourselves.  Under Win98 the GDI does its
   // calculations in 16 bit arithmetic, and so gets it completely
   // wrong at higher zooms where the bar can easily be
   // more than 65536 pixels wide.

   // Draw bar for label extent...
   // We don't quite draw from x to x1 because we allow
   // half an icon width at each end.
   {
      const int xStart=wxMax(r.x,xText-LabelTrack::mIconWidth/2);
      const int xEnd=wxMin(r.x+r.width,xText+width+LabelTrack::mIconWidth/2);
      const int xWidth = xEnd-xStart;

      if( (xStart < (r.x+r.width)) && (xEnd > r.x) && (xWidth>0))
      {
          wxRect frame(
            xStart,y-yFrameHeight/2,
            xWidth,yFrameHeight );
         dc.DrawRectangle(frame);
      }
   }
}

/// Draws text-selected region within the label
void LabelStruct::DrawHighlight
   ( wxDC & dc, int xPos1, int xPos2, int charHeight) const
{
   wxPen curPen = dc.GetPen();
   curPen.SetColour(wxString(wxT("BLUE")));
   wxBrush curBrush = dc.GetBrush();
   curBrush.SetColour(wxString(wxT("BLUE")));
   if (xPos1 < xPos2)
      dc.DrawRectangle(xPos1-1, y-charHeight/2, xPos2-xPos1+1, charHeight);
   else
      dc.DrawRectangle(xPos2-1, y-charHeight/2, xPos1-xPos2+1, charHeight);
}

void LabelStruct::getXPos( wxDC & dc, int * xPos1, int cursorPos) const
{
   *xPos1 = xText;
   if( cursorPos > 0)
   {
      int partWidth;
      // Calculate the width of the substring and add it to Xpos
      dc.GetTextExtent(title.Left(cursorPos), &partWidth, NULL);
      *xPos1 += partWidth;
   }
}

bool LabelTrack::CalcCursorX(int * x) const
{
   if (mSelIndex >= 0) {
      wxMemoryDC dc;

      if (msFont.Ok()) {
         dc.SetFont(msFont);
      }

      mLabels[mSelIndex].getXPos(dc, x, mCurrentCursorPos);
      *x += LabelTrack::mIconWidth / 2;
      return true;
   }

   return false;
}

void LabelTrack::CalcHighlightXs(int *x1, int *x2) const
{
   wxMemoryDC dc;

   if (msFont.Ok()) {
      dc.SetFont(msFont);
   }

   int pos1 = mInitialCursorPos, pos2 = mCurrentCursorPos;
   if (pos1 > pos2)
      std::swap(pos1, pos2);

   const auto &labelStruct = mLabels[mSelIndex];

   // find the left X pos of highlighted area
   labelStruct.getXPos(dc, x1, pos1);
   // find the right X pos of highlighted area
   labelStruct.getXPos(dc, x2, pos2);
}

/// Draw calls other functions to draw the LabelTrack.
///   @param  dc the device context
///   @param  r  the LabelTrack rectangle.
void LabelTrack::Draw(wxDC & dc, const wxRect & r,
   const SelectedRegion &selectedRegion,
   const ZoomInfo &zoomInfo) const
{
   if(msFont.Ok())
      dc.SetFont(msFont);

   if (mFontHeight == -1)
      calculateFontHeight(dc);

   TrackArtist::DrawBackgroundWithSelection(&dc, r, this,
         AColor::labelSelectedBrush, AColor::labelUnselectedBrush,
         selectedRegion, zoomInfo);

   wxCoord textWidth, textHeight;

   // Get the text widths.
   // TODO: Make more efficient by only re-computing when a
   // text label title changes.
   for (auto &labelStruct : mLabels) {
      dc.GetTextExtent(labelStruct.title, &textWidth, &textHeight);
      labelStruct.width = textWidth;
   }

   // TODO: And this only needs to be done once, but we
   // do need the dc to do it.
   // We need to set mTextHeight to something sensible,
   // guarding against the case where there are no
   // labels or all are empty strings, which for example
   // happens with a NEW label track.
   dc.GetTextExtent(wxT("Demo Text x^y"), &textWidth, &textHeight);
   mTextHeight = (int)textHeight;
   ComputeLayout( r, zoomInfo );
   dc.SetTextForeground(theTheme.Colour( clrLabelTrackText));
   dc.SetBackgroundMode(wxTRANSPARENT);
   dc.SetBrush(AColor::labelTextNormalBrush);
   dc.SetPen(AColor::labelSurroundPen);
   int GlyphLeft;
   int GlyphRight;
   // Now we draw the various items in this order,
   // so that the correct things overpaint each other.

   // Draw vertical lines that show where the end positions are.
   for (auto &labelStruct : mLabels)
      labelStruct.DrawLines( dc, r );

   // Draw the end glyphs.
   { int i = -1; for (auto &labelStruct : mLabels) { ++i;
      GlyphLeft=0;
      GlyphRight=1;
      if( i==mMouseOverLabelLeft )
         GlyphLeft = mbHitCenter ? 6:9;
      if( i==mMouseOverLabelRight )
         GlyphRight = mbHitCenter ? 7:4;
      labelStruct.DrawGlyphs( dc, r, GlyphLeft, GlyphRight );
   }}

   // Draw the label boxes.
   { int i = -1; for (auto &labelStruct : mLabels) { ++i;
      if( mSelIndex==i)
         dc.SetBrush(AColor::labelTextEditBrush);
      labelStruct.DrawTextBox( dc, r );
      if( mSelIndex==i)
         dc.SetBrush(AColor::labelTextNormalBrush);
   }}

   // Draw highlights
   if ((mInitialCursorPos != mCurrentCursorPos) && (mSelIndex >= 0 ))
   {
      int xpos1, xpos2;
      CalcHighlightXs(&xpos1, &xpos2);
      mLabels[mSelIndex].DrawHighlight(dc, xpos1, xpos2, mFontHeight);
   }

   // Draw the text and the label boxes.
   { int i = -1; for (auto &labelStruct : mLabels) { ++i;
      if( mSelIndex==i)
         dc.SetBrush(AColor::labelTextEditBrush);
      labelStruct.DrawText( dc, r );
      if( mSelIndex==i)
         dc.SetBrush(AColor::labelTextNormalBrush);
   }}

   // Draw the cursor, if there is one.
   if( mDrawCursor && mSelIndex >=0 )
   {
      const auto &labelStruct = mLabels[mSelIndex];
      int xPos = labelStruct.xText;

      if( mCurrentCursorPos > 0)
      {
         // Calculate the width of the substring and add it to Xpos
         int partWidth;
         dc.GetTextExtent(labelStruct.title.Left(mCurrentCursorPos), &partWidth, NULL);
         xPos += partWidth;
      }

      wxPen currentPen = dc.GetPen();
      const int CursorWidth=2;
      currentPen.SetWidth(CursorWidth);
      AColor::Line(dc,
                   xPos-1, labelStruct.y - mFontHeight/2 + 1,
                   xPos-1, labelStruct.y + mFontHeight/2 - 1);
      currentPen.SetWidth(1);
   }
}

/// uses GetTextExtent to find the character position
/// corresponding to the x pixel position.
int LabelTrack::FindCurrentCursorPosition(int xPos)
{
   int result = -1;
   wxMemoryDC dc;
   if(msFont.Ok())
      dc.SetFont(msFont);

   // A bool indicator to see if set the cursor position or not
   bool finished = false;
   int charIndex = 1;
   int partWidth;
   int oneWidth;
   double bound;
   wxString subString;
   const auto &labelStruct = mLabels[mSelIndex];
   const auto &title = labelStruct.title;
   const int length = title.length();
   while (!finished && (charIndex < length + 1))
   {
      subString = title.Left(charIndex);
      // Get the width of substring
      dc.GetTextExtent(subString, &partWidth, NULL);

      // Get the width of the last character
      dc.GetTextExtent(subString.Right(1), &oneWidth, NULL);
      bound = labelStruct.xText + partWidth - oneWidth * 0.5;

      if (xPos <= bound)
      {
         // Found
         result = charIndex - 1;
         finished = true;
      }
      else
      {
         // Advance
         charIndex++;
      }
   }
   if (!finished)
      // Cursor should be in the last position
      result = length;

   return result;
}

/// Set the cursor position according to x position of mouse
void LabelTrack::SetCurrentCursorPosition(int xPos)
{
   mCurrentCursorPos = FindCurrentCursorPosition(xPos);
}

void LabelTrack::calculateFontHeight(wxDC & dc) const
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
   mFontHeight += CursorExtraHeight - (charLeading+charDescent);
}

bool LabelTrack::IsTextSelected()
{
   if (mSelIndex == -1)
      return false;
   if (mCurrentCursorPos == mInitialCursorPos)
      return false;
   return true;
}

/// Cut the selected text in the text box
///  @return true if text is selected in text box, false otherwise
bool LabelTrack::CutSelectedText()
{
   if (!IsTextSelected())
      return false;

   wxString left, right;
   auto &labelStruct = mLabels[mSelIndex];
   auto &text = labelStruct.title;

   int init = mInitialCursorPos;
   int cur = mCurrentCursorPos;
   if (init > cur)
      std::swap(init, cur);

   // data for cutting
   wxString data = text.Mid(init, cur - init);

   // get left-remaining text
   if (init > 0)
      left = text.Left(init);

   // get right-remaining text
   if (cur < (int)text.Length())
      right = text.Mid(cur);

   // set title to the combination of the two remainders
   text = left + right;

   // copy data onto clipboard
   if (wxTheClipboard->Open()) {
      // Clipboard owns the data you give it
      wxTheClipboard->SetData(safenew wxTextDataObject(data));
      wxTheClipboard->Close();
   }

   // set cursor positions
   mInitialCursorPos = mCurrentCursorPos = left.Length();
   return true;
}

/// Copy the selected text in the text box
///  @return true if text is selected in text box, false otherwise
bool LabelTrack::CopySelectedText()
{
   if (mSelIndex == -1)
      return false;

   const auto &labelStruct = mLabels[mSelIndex];

   int init = mInitialCursorPos;
   int cur = mCurrentCursorPos;
   if (init > cur)
      std::swap(init, cur);

   if (init == cur)
      return false;

   // data for copying
   wxString data = labelStruct.title.Mid(init, cur-init);

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
bool LabelTrack::PasteSelectedText(double sel0, double sel1)
{
   if (mSelIndex == -1)
      AddLabel(SelectedRegion(sel0, sel1), wxT(""));

   wxString text, left, right;

   // if text data is available
   if (IsTextClipSupported()) {
      if (wxTheClipboard->Open()) {
         wxTextDataObject data;
         wxTheClipboard->GetData(data);
         wxTheClipboard->Close();
         text = data.GetText();
      }

      // Convert control characters to blanks
      for (int i = 0; i < (int)text.Length(); i++) {
         if (wxIscntrl(text[i])) {
            text[i] = wxT(' ');
         }
      }
   }

   auto &labelStruct = mLabels[mSelIndex];
   auto &title = labelStruct.title;
   int cur = mCurrentCursorPos, init = mInitialCursorPos;
   if (init > cur)
      std::swap(init, cur);
   left = title.Left(init);
   if (cur < (int)title.Length())
      right = title.Mid(cur);

   title = left + text + right;
   mInitialCursorPos =  mCurrentCursorPos = left.Length() + text.Length();
   return true;
}


/// @return true if the text data is available in the clipboard, false otherwise
bool LabelTrack::IsTextClipSupported()
{
   return wxTheClipboard->IsSupported(wxDF_TEXT);
}


double LabelTrack::GetOffset() const
{
   return mOffset;
}

double LabelTrack::GetStartTime() const
{
   if (mLabels.empty())
      return 0.0;
   else
      return mLabels[0].getT0();
}

double LabelTrack::GetEndTime() const
{
   //we need to scan through all the labels, because the last
   //label might not have the right-most end (if there is overlap).
   if (mLabels.empty())
      return 0.0;

   double end = 0.0;
   for (auto &labelStruct: mLabels) {
      const double t1 = labelStruct.getT1();
      if(t1 > end)
         end = t1;
   }
   return end;
}

Track::Holder LabelTrack::Duplicate() const
{
   return std::make_unique<LabelTrack>( *this );
}

void LabelTrack::SetSelected(bool s)
{
   Track::SetSelected(s);
   if (!s)
      Unselect();
}

/// OverGlyph returns 0 if not over a glyph,
/// 1 if over the left-hand glyph, and
/// 2 if over the right-hand glyph on a label.
/// 3 if over both right and left.
///
/// It also sets up member variables:
///   mMouseLabelLeft - index of any left label hit
///   mMouseLabelRight - index of any right label hit
///   mbHitCenter     - if (x,y) 'hits the spot'.
///
/// TODO: Investigate what happens with large
/// numbers of labels, might need a binary search
/// rather than a linear one.
int LabelTrack::OverGlyph(int x, int y)
{
   //Determine the NEW selection.
   int result=0;
   const int d1=10; //distance in pixels, used for have we hit drag handle.
   const int d2=5;  //distance in pixels, used for have we hit drag handle center.

   //If not over a label, reset it
   mMouseOverLabelLeft  = -1;
   mMouseOverLabelRight = -1;
   mbHitCenter = false;
   { int i = -1; for (auto &labelStruct : mLabels) { ++i;
      //over left or right selection bound
      //Check right bound first, since it is drawn after left bound,
      //so give it precedence for matching/highlighting.
      if( abs(labelStruct.y - (y - (LabelTrack::mTextHeight+3)/2)) < d1 &&
               abs(labelStruct.x1 - d2 -x) < d1)
      {
         mMouseOverLabelRight = i;
         if(abs(labelStruct.x1 - x) < d2 )
         {
            mbHitCenter = true;
            // If left and right co-incident at this resolution, then we drag both.
            // We could be a little less stringent about co-incidence here if we liked.
            if( abs(labelStruct.x1-labelStruct.x) < 1.0 )
            {
               result |=1;
               mMouseOverLabelLeft = i;
            }
         }
         result |= 2;
      }
      // Use else-if here rather than else to avoid detecting left and right
      // of the same label.
      else if(   abs(labelStruct.y - (y - (LabelTrack::mTextHeight+3)/2)) < d1 &&
            abs(labelStruct.x + d2 - x) < d1 )
      {
         mMouseOverLabelLeft = i;
         if(abs(labelStruct.x - x) < d2 )
            mbHitCenter = true;
         result |= 1;
      }

      // give text box better priority for selecting
      if(OverTextBox(&labelStruct, x, y))
      {
         result = 0;
      }

   }}
   return result;
}

int LabelTrack::OverATextBox(int xx, int yy) const
{
   for (int nn = (int)mLabels.size(); nn--;) {
      const auto &labelStruct = mLabels[nn];
      if (OverTextBox(&labelStruct, xx, yy))
         return nn;
   }

   return -1;
}

// return true if the mouse is over text box, false otherwise
bool LabelTrack::OverTextBox(const LabelStruct *pLabel, int x, int y) const
{
   if( (pLabel->xText-(mIconWidth/2) < x) &&
            (x<pLabel->xText+pLabel->width+(mIconWidth/2)) &&
            (abs(pLabel->y-y)<mIconHeight/2))
   {
      return true;
   }
   return false;
}

// Adjust label's left or right boundary, depending which is requested.
// Return true iff the label flipped.
bool LabelStruct::AdjustEdge( int iEdge, double fNewTime)
{
   updated = true;
   if( iEdge < 0 )
      return selectedRegion.setT0(fNewTime);
   else
      return selectedRegion.setT1(fNewTime);
}

// We're moving the label.  Adjust both left and right edge.
void LabelStruct::MoveLabel( int iEdge, double fNewTime)
{
   double fTimeSpan = getDuration();

   if( iEdge < 0 )
   {
      selectedRegion.setTimes(fNewTime, fNewTime+fTimeSpan);
   }
   else
   {
      selectedRegion.setTimes(fNewTime-fTimeSpan, fNewTime);
   }
   updated = true;
}

LabelStruct LabelStruct::Import(wxTextFile &file, int &index)
{
   SelectedRegion sr;
   wxString title;
   static const wxString continuation{ wxT("\\") };

   wxString firstLine = file.GetLine(index++);

   {
      // Assume tab is an impossible character within the exported text
      // of the label, so can be only a delimiter.  But other white space may
      // be part of the label text.
      wxStringTokenizer toker { firstLine, wxT("\t") };

      //get the timepoint of the left edge of the label.
      auto token = toker.GetNextToken();

      double t0;
      if (!Internat::CompatibleToDouble(token, &t0))
         throw BadFormatException{};

      token = toker.GetNextToken();

      double t1;
      if (!Internat::CompatibleToDouble(token, &t1))
         //s1 is not a number.
         t1 = t0;  //This is a one-sided label; t1 == t0.
      else
         token = toker.GetNextToken();

      sr.setTimes( t0, t1 );
      
      title = token;
   }

   // Newer selection fields are written on additional lines beginning with
   // '\' which is an impossible numerical character that older versions of
   // audacity will ignore.  Test for the presence of such a line and then
   // parse it if we can.

   // There may also be additional continuation lines from future formats that
   // we ignore.

   // Advance index over all continuation lines first, before we might throw
   // any exceptions.
   int index2 = index;
   while (index < file.GetLineCount() &&
          file.GetLine(index).StartsWith(continuation))
      ++index;

   if (index2 < index) {
      wxStringTokenizer toker { file.GetLine(index2++), wxT("\t") };
      auto token = toker.GetNextToken();
      if (token != continuation)
         throw BadFormatException{};

      token = toker.GetNextToken();
      double f0;
      if (!Internat::CompatibleToDouble(token, &f0))
         throw BadFormatException{};

      token = toker.GetNextToken();
      double f1;
      if (!Internat::CompatibleToDouble(token, &f1))
         throw BadFormatException{};

      sr.setFrequencies(f0, f1);
   }

   return LabelStruct{ sr, title };
}

void LabelStruct::Export(wxTextFile &file) const
{
   file.AddLine(wxString::Format(wxT("%f\t%f\t%s"),
      getT0(),
      getT1(), 
      title.c_str()
   ));

   // Do we need more lines?
   auto f0 = selectedRegion.f0();
   auto f1 = selectedRegion.f1();
   if (f0 == SelectedRegion::UndefinedFrequency &&
       f1 == SelectedRegion::UndefinedFrequency)
      return;

   // Write a \ character at the start of a second line,
   // so that earlier versions of Audacity ignore it.
   file.AddLine(wxString::Format(wxT("\\\t%f\t%f"),
      f0,
      f1
   ));

   // Additional lines in future formats should also start with '\'.
}

auto LabelStruct::RegionRelation(
      double reg_t0, double reg_t1, const LabelTrack * WXUNUSED(parent)) const
-> TimeRelations
{
   bool retainLabels = false;

   wxASSERT(reg_t0 <= reg_t1);
   gPrefs->Read(wxT("/GUI/RetainLabels"), &retainLabels);

   if(retainLabels) {

      // Desired behavior for edge cases: The length of the selection is smaller
      // than the length of the label if the selection is within the label or
      // matching exactly a (region) label.

      if (reg_t0 < getT0() && reg_t1 > getT1())
         return SURROUNDS_LABEL;
      else if (reg_t1 < getT0())
         return BEFORE_LABEL;
      else if (reg_t0 > getT1())
         return AFTER_LABEL;

      else if (reg_t0 >= getT0() && reg_t0 <= getT1() &&
               reg_t1 >= getT0() && reg_t1 <= getT1())
         return WITHIN_LABEL;

      else if (reg_t0 >= getT0() && reg_t0 <= getT1())
         return BEGINS_IN_LABEL;
      else
         return ENDS_IN_LABEL;

   } else {

      // AWD: Desired behavior for edge cases: point labels bordered by the
      // selection are included within it. Region labels are included in the
      // selection to the extent that the selection covers them; specifically,
      // they're not included at all if the selection borders them, and they're
      // fully included if the selection covers them fully, even if it just
      // borders their endpoints. This is just one of many possible schemes.

      // The first test catches bordered point-labels and selected-through
      // region-labels; move it to third and selection edges become inclusive
      // WRT point-labels.
      if (reg_t0 <= getT0() && reg_t1 >= getT1())
         return SURROUNDS_LABEL;
      else if (reg_t1 <= getT0())
         return BEFORE_LABEL;
      else if (reg_t0 >= getT1())
         return AFTER_LABEL;

      // At this point, all point labels should have returned.

      else if (reg_t0 > getT0() && reg_t0 < getT1() &&
               reg_t1 > getT0() && reg_t1 < getT1())
         return WITHIN_LABEL;

      // Knowing that none of the other relations match simplifies remaining
      // tests
      else if (reg_t0 > getT0() && reg_t0 < getT1())
         return BEGINS_IN_LABEL;
      else
         return ENDS_IN_LABEL;

   }
}

/// If the index is for a real label, adjust its left or right boundary.
/// @iLabel - index of label, -1 for none.
/// @iEdge - which edge is requested to move, -1 for left +1 for right.
/// @bAllowSwapping - if we can switch which edge is being dragged.
/// fNewTime - the NEW time for this edge of the label.
void LabelTrack::MayAdjustLabel( int iLabel, int iEdge, bool bAllowSwapping, double fNewTime)
{
   if( iLabel < 0 )
      return;
   LabelStruct &labelStruct = mLabels[ iLabel ];

   // Adjust the requested edge.
   bool flipped = labelStruct.AdjustEdge( iEdge, fNewTime );
   // If the edges did not swap, then we are done.
   if( ! flipped )
      return;

   // If swapping's not allowed we must also move the edge
   // we didn't move.  Then we're done.
   if( !bAllowSwapping )
   {
      labelStruct.AdjustEdge( -iEdge, fNewTime );
      return;
   }

   // Swap our record of what we are dragging.
   int Temp = mMouseOverLabelLeft;
   mMouseOverLabelLeft = mMouseOverLabelRight;
   mMouseOverLabelRight = Temp;
}

// If the index is for a real label, adjust its left and right boundary.
void LabelTrack::MayMoveLabel( int iLabel, int iEdge, double fNewTime)
{
   if( iLabel < 0 )
      return;
   mLabels[ iLabel ].MoveLabel( iEdge, fNewTime );
}

// Constrain function, as in processing/arduino.
// returned value will be between min and max (inclusive).
static int Constrain( int value, int min, int max )
{
   wxASSERT( min <= max );
   int result=value;
   if( result < min )
      result=min;
   if( result > max )
      result=max;
   return result;
}

bool LabelTrack::HandleGlyphDragRelease(const wxMouseEvent & evt,
   wxRect & r, const ZoomInfo &zoomInfo,
   SelectedRegion *newSel)
{
   if(evt.LeftUp())
   {
      bool lupd = false, rupd = false;
      if(mMouseOverLabelLeft>=0) {
         auto &labelStruct = mLabels[mMouseOverLabelLeft];
         lupd = labelStruct.updated;
         labelStruct.updated = false;
      }
      if(mMouseOverLabelRight>=0) {
         auto &labelStruct = mLabels[mMouseOverLabelRight];
         rupd = labelStruct.updated;
         labelStruct.updated = false;
      }

      mIsAdjustingLabel = false;
      mMouseOverLabelLeft  = -1;
      mMouseOverLabelRight = -1;
      return lupd || rupd;
   }

   if(evt.Dragging())
   {
      //If we are currently adjusting a label,
      //just reset its value and redraw.
      // LL:  Constrain to inside track rectangle for now.  Should be changed
      //      to allow scrolling while dragging labels
      int x = Constrain( evt.m_x + mxMouseDisplacement - r.x, 0, r.width);

      // If exactly one edge is selected we allow swapping
      bool bAllowSwapping = (mMouseOverLabelLeft >=0 ) ^ ( mMouseOverLabelRight >= 0);
      // If we're on the 'dot' and nowe're moving,
      // Though shift-down inverts that.
      // and if both edges the same, then we're always moving the label.
      bool bLabelMoving = mbIsMoving;
      bLabelMoving ^= evt.ShiftDown();
      bLabelMoving |= mMouseOverLabelLeft==mMouseOverLabelRight;
      double fNewX = zoomInfo.PositionToTime(x, 0);
      if( bLabelMoving )
      {
         MayMoveLabel( mMouseOverLabelLeft,  -1, fNewX );
         MayMoveLabel( mMouseOverLabelRight, +1, fNewX );
      }
      else
      {
         MayAdjustLabel( mMouseOverLabelLeft,  -1, bAllowSwapping, fNewX );
         MayAdjustLabel( mMouseOverLabelRight, +1, bAllowSwapping, fNewX );
      }

      if( mSelIndex >=0 )
      {
         //Set the selection region to be equal to
         //the NEW size of the label.
         *newSel = mLabels[mSelIndex].selectedRegion;
      }
      SortLabels();
   }

   return false;
}

void LabelTrack::HandleTextDragRelease(const wxMouseEvent & evt)
{
   if(evt.LeftUp())
   {
#if 0
      // AWD: Due to wxWidgets bug #7491 (fix not ported to 2.8 branch) we
      // should never write the primary selection. We can enable this block
      // when we move to the 3.0 branch (or if a fixed 2.8 version is released
      // and we can do a runtime version check)
#if defined (__WXGTK__) && defined (HAVE_GTK)
      // On GTK, if we just dragged out a text selection, set the primary
      // selection
      if (mInitialCursorPos != mCurrentCursorPos) {
         wxTheClipboard->UsePrimarySelection(true);
         CopySelectedText();
         wxTheClipboard->UsePrimarySelection(false);
      }
#endif
#endif

      return;
   }

   if(evt.Dragging())
   {
      if (!mRightDragging)
         // Update drag end
         SetCurrentCursorPosition(evt.m_x);

      return;
   }

   if (evt.RightUp()) {
      if ((mSelIndex != -1) && OverTextBox(GetLabel(mSelIndex), evt.m_x, evt.m_y)) {
         // popup menu for editing
         ShowContextMenu();
      }
   }

   return;
}

void LabelTrack::HandleClick(const wxMouseEvent & evt,
   const wxRect & r, const ZoomInfo &zoomInfo,
   SelectedRegion *newSel)
{
   if (evt.ButtonDown())
   {
      //OverGlyph sets mMouseOverLabel to be the chosen label.
      int iGlyph = OverGlyph(evt.m_x, evt.m_y);
      mIsAdjustingLabel = evt.Button(wxMOUSE_BTN_LEFT) &&
         iGlyph != 0;

      if (mIsAdjustingLabel)
      {
         float t = 0.0;
         // We move if we hit the centre, we adjust one edge if we hit a chevron.
         // This is if we are moving just one edge.
         mbIsMoving = mbHitCenter;
         // When we start dragging the label(s) we don't want them to jump.
         // so we calculate the displacement of the mouse from the drag center
         // and use that in subsequent dragging calculations.  The mouse stays
         // at the same relative displacement throughout dragging.

         // However, if two label's edges are being dragged
         // then the displacement is relative to the initial average
         // position of them, and in that case there can be a jump of at most
         // a few pixels to bring the two label boundaries to exactly the same
         // position when we start dragging.

         // Dragging of three label edges at the same time is not supported (yet).
         if( (mMouseOverLabelRight >=0) &&
             (mMouseOverLabelLeft >=0)
           )
         {
            t = (mLabels[mMouseOverLabelRight].getT1() +
                 mLabels[mMouseOverLabelLeft].getT0()) / 2.0f;
            // If we're moving two edges, then it's a move (label size preserved)
            // if both edges are the same label, and it's an adjust (label sizes change)
            // if we're on a boundary between two different labels.
            mbIsMoving = (mMouseOverLabelLeft == mMouseOverLabelRight);
         }
         else if(mMouseOverLabelRight >=0)
         {
            t = mLabels[mMouseOverLabelRight].getT1();
         }
         else if(mMouseOverLabelLeft >=0)
         {
            t = mLabels[mMouseOverLabelLeft].getT0();
         }
         mxMouseDisplacement = zoomInfo.TimeToPosition(t, r.x) - evt.m_x;
         return;
      }

      mSelIndex = OverATextBox(evt.m_x, evt.m_y);
      if (mSelIndex != -1) {
         auto &labelStruct = mLabels[mSelIndex];
         *newSel = labelStruct.selectedRegion;

         if (evt.LeftDown()) {
            // Find the NEW drag end
            auto position = FindCurrentCursorPosition(evt.m_x);

            // Anchor shift-drag at the farther end of the previous highlight
            // that is farther from the click, on Mac, for consistency with
            // its text editors, but on the others, re-use the previous
            // anchor.
            if (evt.ShiftDown()) {
#ifdef __WXMAC__
               // Set the drag anchor at the end of the previous selection
               // that is farther from the NEW drag end
               if (abs(position - mCurrentCursorPos) >
                   abs(position - mInitialCursorPos))
                  mInitialCursorPos = mCurrentCursorPos;
#else
               // mInitialCursorPos remains as before
#endif
            }
            else
               mInitialCursorPos = position;

            mCurrentCursorPos = position;
            
            mDrawCursor = true;
            mRightDragging = false;
         }
         else
            // Actually this might be right or middle down
            mRightDragging = true;

         // reset the highlight indicator
         wxRect highlightedRect;
         {
            int xpos1, xpos2;
            CalcHighlightXs(&xpos1, &xpos2);

            wxASSERT(mFontHeight >= 0); // should have been set up while drawing
            // the rectangle of highlighted area
            highlightedRect = {
               xpos1, labelStruct.y - mFontHeight / 2,
               (int)(xpos2 - xpos1 + 0.5), mFontHeight
            };
         }

         // Middle click on GTK: paste from primary selection
#if defined(__WXGTK__) && (HAVE_GTK)
         if (evt.MiddleDown()) {
            // Check for a click outside of the selected label's text box; in this
            // case PasteSelectedText() will start a NEW label at the click
            // location
            if (!OverTextBox(&labelStruct, evt.m_x, evt.m_y))
               mSelIndex = -1;
            double t = zoomInfo.PositionToTime(evt.m_x, r.x);
            *newSel = SelectedRegion(t, t);
         }
#endif
      }

#if defined(__WXGTK__) && (HAVE_GTK)
      if (evt.MiddleDown()) {
         // Paste text, making a NEW label if none is selected.
         wxTheClipboard->UsePrimarySelection(true);
         PasteSelectedText(newSel->t0(), newSel->t1());
         wxTheClipboard->UsePrimarySelection(false);
      }
#endif
   }
}

// Check for keys that we will process
bool LabelTrack::CaptureKey(wxKeyEvent & event)
{
   // Check for modifiers and only allow shift
   int mods = event.GetModifiers();
   if (mods != wxMOD_NONE && mods != wxMOD_SHIFT) {
      return false;
   }

   // Always capture the navigation keys, if we have any labels
   auto code = event.GetKeyCode();
   if ((code == WXK_TAB || code == WXK_NUMPAD_TAB) &&
       !mLabels.empty())
      return true;

   if (mSelIndex >= 0) {
      if (IsGoodLabelEditKey(event)) {
         return true;
      }
   }
   else {
      if (IsGoodLabelFirstKey(event)) {
         AudacityProject * pProj = GetActiveProject();

         // If we're playing, don't capture if the selection is the same as the
         // playback region (this helps prevent label track creation from
         // stealing unmodified kbd. shortcuts)
         if (pProj->GetAudioIOToken() > 0 &&
               gAudioIO->IsStreamActive(pProj->GetAudioIOToken()))
         {
            double t0, t1;
            pProj->GetPlayRegion(&t0, &t1);
            if (pProj->mViewInfo.selectedRegion.t0() == t0 &&
                pProj->mViewInfo.selectedRegion.t1() == t1) {
               return false;
            }
         }

         // If there's a label there already don't capture
         if( GetLabelIndex(pProj->mViewInfo.selectedRegion.t0(),
                           pProj->mViewInfo.selectedRegion.t1()) != wxNOT_FOUND ) {
            return false;
         }

         return true;
      }
   }

   return false;
}

/// KeyEvent is called for every keypress when over the label track.
bool LabelTrack::OnKeyDown(SelectedRegion &newSel, wxKeyEvent & event)
{
   // Only track true changes to the label
   bool updated = false;

   // Cache the keycode
   int keyCode = event.GetKeyCode();
   const int mods = event.GetModifiers();

   // Check for modifiers and only allow shift
   if (mods != wxMOD_NONE && mods != wxMOD_SHIFT) {
      event.Skip();
      return updated;
   }

   // All editing keys are only active if we're currently editing a label
   if (mSelIndex >= 0) {
      auto &labelStruct = mLabels[mSelIndex];
      auto &title = labelStruct.title;
      switch (keyCode) {

      case WXK_BACK:
         {
            int len = title.Length();

            //IF the label is not blank THEN get rid of a letter or letters according to cursor position
            if (len > 0)
            {
               // IF there are some highlighted letters, THEN DELETE them
               if (mInitialCursorPos != mCurrentCursorPos)
                  RemoveSelectedText();
               else
               {
                  // DELETE one letter
                  if (mCurrentCursorPos > 0) {
                     title.Remove(mCurrentCursorPos-1, 1);
                     mCurrentCursorPos--;
                  }
               }
            }
            else
            {
               // ELSE no text in text box, so DELETE whole label.
               DeleteLabel( mSelIndex );
            }
            mInitialCursorPos = mCurrentCursorPos;
            updated = true;
         }
         break;

      case WXK_DELETE:
      case WXK_NUMPAD_DELETE:
         {
            int len = title.Length();

            //If the label is not blank get rid of a letter according to cursor position
            if (len > 0)
            {
               // if there are some highlighted letters, DELETE them
               if (mInitialCursorPos != mCurrentCursorPos)
                  RemoveSelectedText();
               else
               {
                  // DELETE one letter
                  if (mCurrentCursorPos < len) {
                     title.Remove(mCurrentCursorPos, 1);
                  }
               }
            }
            else
            {
               // DELETE whole label if no text in text box
               DeleteLabel( mSelIndex );
            }
            mInitialCursorPos = mCurrentCursorPos;
            updated = true;
         }
         break;

      case WXK_HOME:
      case WXK_NUMPAD_HOME:
         // Move cursor to beginning of label
         mCurrentCursorPos = 0;
         if (mods == wxMOD_SHIFT)
            ;
         else
            mInitialCursorPos = mCurrentCursorPos;
         break;

      case WXK_END:
      case WXK_NUMPAD_END:
         // Move cursor to end of label
         mCurrentCursorPos = (int)title.length();
         if (mods == wxMOD_SHIFT)
            ;
         else
            mInitialCursorPos = mCurrentCursorPos;
         break;

      case WXK_LEFT:
      case WXK_NUMPAD_LEFT:
         // Moving cursor left
         if (mCurrentCursorPos > 0) {
            mCurrentCursorPos--;
            if (mods == wxMOD_SHIFT)
               ;
            else
               mInitialCursorPos = mCurrentCursorPos =
                  std::min(mInitialCursorPos, mCurrentCursorPos);
         }
         break;

      case WXK_RIGHT:
      case WXK_NUMPAD_RIGHT:
         // Moving cursor right
         if (mCurrentCursorPos < (int)title.length()) {
            mCurrentCursorPos++;
            if (mods == wxMOD_SHIFT)
               ;
            else
               mInitialCursorPos = mCurrentCursorPos =
                  std::max(mInitialCursorPos, mCurrentCursorPos);
         }
         break;

      case WXK_RETURN:
      case WXK_NUMPAD_ENTER:

      case WXK_ESCAPE:
         if (mRestoreFocus >= 0) {
            TrackListIterator iter(GetActiveProject()->GetTracks());
            Track *track = iter.First();
            while (track && mRestoreFocus--)
               track = iter.Next();
            if (track)
               GetActiveProject()->GetTrackPanel()->SetFocusedTrack(track);
            mRestoreFocus = -1;
         }
         mSelIndex = -1;
         break;

      case WXK_TAB:
      case WXK_NUMPAD_TAB:
         if (event.ShiftDown()) {
               mSelIndex--;
         } else {
               mSelIndex++;
         }

         if (mSelIndex >= 0 && mSelIndex < (int)mLabels.size()) {
            LabelStruct &newLabel = mLabels[mSelIndex];
            mCurrentCursorPos = newLabel.title.Length();
            //Set the selection region to be equal to the selection bounds of the tabbed-to label.
            newSel = newLabel.selectedRegion;
         }
         else {
            newSel = SelectedRegion{};
            mSelIndex = -1;
         }
         break;

      case '\x10':   // OSX
      case WXK_MENU:
      case WXK_WINDOWS_MENU:
         ShowContextMenu();
         break;

      default:
         if (!IsGoodLabelEditKey(event)) {
            event.Skip();
         }
         break;
      }
   }
   else
   {
      switch (keyCode) {

      case WXK_TAB:
      case WXK_NUMPAD_TAB:
         if (!mLabels.empty()) {
            int len = (int) mLabels.size();
            if (event.ShiftDown()) {
               mSelIndex = len - 1;
               if (newSel.t0() > mLabels[0].getT0()) {
                  while (mSelIndex >= 0 &&
                         mLabels[mSelIndex].getT0() > newSel.t0()) {
                     mSelIndex--;
                  }
               }
            } else {
               mSelIndex = 0;
               if (newSel.t0() < mLabels[len - 1].getT0()) {
                  while (mSelIndex < len &&
                         mLabels[mSelIndex].getT0() < newSel.t0()) {
                     mSelIndex++;
                  }
               }
            }

            if (mSelIndex >= 0 && mSelIndex < len) {
               const auto &labelStruct = mLabels[mSelIndex];
               mCurrentCursorPos = labelStruct.title.Length();
               //Set the selection region to be equal to the selection bounds of the tabbed-to label.
               newSel = labelStruct.selectedRegion;
            }
            else {
               mSelIndex = -1;
            }
         }
         break;

      default:
         if (!IsGoodLabelFirstKey(event)) {
            event.Skip();
         }
         break;
      }
   }

   // Make sure the caret is visible
   mDrawCursor = true;

   return updated;
}

/// OnChar is called for incoming characters -- that's any keypress not handled
/// by OnKeyDown.
bool LabelTrack::OnChar(SelectedRegion &WXUNUSED(newSel), wxKeyEvent & event)
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
   bool updated = false;

   // Cache the character
   wxChar charCode = event.GetUnicodeKey();

   // Skip if it's not a valid unicode character or a control character
   if (charCode == 0 || wxIscntrl(charCode)) {
      event.Skip();
      return false;
   }
   
   // If we've reached this point and aren't currently editing, add NEW label
   if (mSelIndex < 0) {
      // Don't create a NEW label for a space
      if (wxIsspace(charCode)) {
         event.Skip();
         return false;
      }
      SetSelected(true);
      AudacityProject *p = GetActiveProject();
      AddLabel(p->mViewInfo.selectedRegion);
      p->PushState(_("Added label"), _("Label"));
   }

   //
   // Now we are definitely in a label; append the incoming character
   //

   auto &labelStruct = mLabels[mSelIndex];
   auto &title = labelStruct.title;

   // Test if cursor is in the end of string or not
   if (mInitialCursorPos != mCurrentCursorPos)
      RemoveSelectedText();

   if (mCurrentCursorPos < (int)title.length()) {
      // Get substring on the righthand side of cursor
      wxString rightPart = title.Mid(mCurrentCursorPos);
      // Set title to substring on the lefthand side of cursor
      title = title.Left(mCurrentCursorPos);
      //append charcode
      title += charCode;
      //append the right part substring
      title += rightPart;
   }
   else
      //append charCode
      title += charCode;

   //moving cursor position forward
   mInitialCursorPos = ++mCurrentCursorPos;
   updated = true;

   // Make sure the caret is visible
   mDrawCursor = true;

   return updated;
}

void LabelTrack::ShowContextMenu()
{
   wxWindow *parent = wxWindow::FindFocus();

   {
      wxMenu menu;
      menu.Bind(wxEVT_MENU, &LabelTrack::OnContextMenu, this);

      menu.Append(OnCutSelectedTextID, _("Cu&t"));
      menu.Append(OnCopySelectedTextID, _("&Copy"));
      menu.Append(OnPasteSelectedTextID, _("&Paste"));
      menu.Append(OnDeleteSelectedLabelID, _("&Delete Label"));
      menu.Append(OnEditSelectedLabelID, _("&Edit..."));

      menu.Enable(OnCutSelectedTextID, IsTextSelected());
      menu.Enable(OnCopySelectedTextID, IsTextSelected());
      menu.Enable(OnPasteSelectedTextID, IsTextClipSupported());
      menu.Enable(OnDeleteSelectedLabelID, true);
      menu.Enable(OnEditSelectedLabelID, true);

      wxASSERT(mSelIndex >= 0);
      const LabelStruct *ls = GetLabel(mSelIndex);

      wxClientDC dc(parent);

      if (msFont.Ok())
      {
         dc.SetFont(msFont);
      }

      int x = 0;
      bool success = CalcCursorX(&x);
      wxASSERT(success);

      parent->PopupMenu(&menu, x, ls->y + (mIconHeight / 2) - 1);
   }
}

void LabelTrack::OnContextMenu(wxCommandEvent & evt)
{
   AudacityProject *p = GetActiveProject();

   switch (evt.GetId())
   {
   /// Cut selected text if cut menu item is selected
   case OnCutSelectedTextID:
      if (CutSelectedText())
      {
         p->PushState(_("Modified Label"),
                      _("Label Edit"),
                      UndoPush::CONSOLIDATE);
      }
      break;

   /// Copy selected text if copy menu item is selected
   case OnCopySelectedTextID:
      CopySelectedText();
      break;

   /// paste selected text if paste menu item is selected
   case OnPasteSelectedTextID:
      if (PasteSelectedText(p->GetSel0(), p->GetSel1()))
      {
         p->PushState(_("Modified Label"),
                      _("Label Edit"),
                      UndoPush::CONSOLIDATE);
      }
      break;

   /// DELETE selected label
   case OnDeleteSelectedLabelID: {
      int ndx = GetLabelIndex(p->GetSel0(), p->GetSel1());
      if (ndx != -1)
      {
         DeleteLabel(ndx);
         p->PushState(_("Deleted Label"),
                      _("Label Edit"),
                      UndoPush::CONSOLIDATE);
      }
   }
      break;

   case OnEditSelectedLabelID: {
      int ndx = GetLabelIndex(p->GetSel0(), p->GetSel1());
      if (ndx != -1)
         p->DoEditLabels(this, ndx);
   }
      break;
   }
}

void LabelTrack::RemoveSelectedText()
{
   wxString left, right;

   int init = mInitialCursorPos;
   int cur = mCurrentCursorPos;
   if (init > cur)
      std::swap(init, cur);

   auto &labelStruct = mLabels[mSelIndex];
   auto &title = labelStruct.title;

   if (init > 0)
      left = title.Left(init);

   if (cur < (int)title.Length())
      right = title.Mid(cur);

   title = left + right;
   mInitialCursorPos = mCurrentCursorPos = left.Length();
}

void LabelTrack::Unselect()
{
   mSelIndex = -1;
}

bool LabelTrack::IsSelected() const
{
   return (mSelIndex >= 0 && mSelIndex < (int)mLabels.size());
}

/// Export labels including label start and end-times.
void LabelTrack::Export(wxTextFile & f) const
{
   // PRL: to do: export other selection fields
   for (auto &labelStruct: mLabels)
      labelStruct.Export(f);
}

/// Import labels, handling files with or without end-times.
void LabelTrack::Import(wxTextFile & in)
{
   int lines = in.GetLineCount();

   mLabels.clear();
   mLabels.reserve(lines);

   //Currently, we expect a tag file to have two values and a label
   //on each line. If the second token is not a number, we treat
   //it as a single-value label.
   for (int index = 0; index < lines;) {
      try {
         // Let LabelStruct::Import advance index
         LabelStruct l { LabelStruct::Import(in, index) };
         mLabels.push_back(l);
      }
      catch(const LabelStruct::BadFormatException&) {}
   }
   SortLabels();
}

bool LabelTrack::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   if (!wxStrcmp(tag, wxT("label"))) {

      SelectedRegion selectedRegion;
      wxString title;

      // loop through attrs, which is a null-terminated list of
      // attribute-value pairs
      while(*attrs) {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;

         if (!value)
            break;

         const wxString strValue = value;
         if (!XMLValueChecker::IsGoodString(strValue))
         {
            return false;
         }

         if (selectedRegion.HandleXMLAttribute(attr, value, wxT("t"), wxT("t1")))
            ;
         else if (!wxStrcmp(attr, wxT("title")))
            title = strValue;

      } // while

      // Handle files created by Audacity 1.1.   Labels in Audacity 1.1
      // did not have separate start- and end-times.
      // PRL: this superfluous now, given class SelectedRegion's internal
      // consistency guarantees
      //if (selectedRegion.t1() < 0)
      //   selectedRegion.collapseToT0();

      LabelStruct l { selectedRegion, title };
      mLabels.push_back(l);

      return true;
   }
   else if (!wxStrcmp(tag, wxT("labeltrack"))) {
      long nValue = -1;
      while (*attrs) {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;

         if (!value)
            return true;

         const wxString strValue = value;
         if (!wxStrcmp(attr, wxT("name")) && XMLValueChecker::IsGoodString(strValue))
            mName = strValue;
         else if (!wxStrcmp(attr, wxT("numlabels")) &&
                     XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
         {
            if (nValue < 0)
            {
               wxLogWarning(wxT("Project shows negative number of labels: %d"), nValue);
               return false;
            }
            mLabels.clear();
            mLabels.reserve(nValue);
         }
         else if (!wxStrcmp(attr, wxT("height")) &&
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
            SetHeight(nValue);
         else if (!wxStrcmp(attr, wxT("minimized")) &&
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
            SetMinimized(nValue != 0);
         else if (!wxStrcmp(attr, wxT("isSelected")) &&
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
            this->SetSelected(nValue != 0);
      }

      return true;
   }

   return false;
}

XMLTagHandler *LabelTrack::HandleXMLChild(const wxChar *tag)
{
   if (!wxStrcmp(tag, wxT("label")))
      return this;
   else
      return NULL;
}

void LabelTrack::WriteXML(XMLWriter &xmlFile)
{
   int len = mLabels.size();

   xmlFile.StartTag(wxT("labeltrack"));
   xmlFile.WriteAttr(wxT("name"), mName);
   xmlFile.WriteAttr(wxT("numlabels"), len);
   xmlFile.WriteAttr(wxT("height"), this->GetActualHeight());
   xmlFile.WriteAttr(wxT("minimized"), this->GetMinimized());
   xmlFile.WriteAttr(wxT("isSelected"), this->GetSelected());

   for (auto &labelStruct: mLabels) {
      xmlFile.StartTag(wxT("label"));
      labelStruct.getSelectedRegion()
         .WriteXMLAttributes(xmlFile, wxT("t"), wxT("t1"));
      // PRL: to do: write other selection fields
      xmlFile.WriteAttr(wxT("title"), labelStruct.title);
      xmlFile.EndTag(wxT("label"));
   }

   xmlFile.EndTag(wxT("labeltrack"));
}

#if LEGACY_PROJECT_FILE_SUPPORT
bool LabelTrack::Load(wxTextFile * in, DirManager * dirManager)
{
   if (in->GetNextLine() != wxT("NumMLabels"))
      return false;

   unsigned long len;
   if (!(in->GetNextLine().ToULong(&len)))
      return false;

   mLabels.clear();
   mLabels.reserve(len);

   for (int i = 0; i < len; i++) {
      double t0;
      if (!Internat::CompatibleToDouble(in->GetNextLine(), &t0))
         return false;
      // Legacy file format does not include label end-times.
      // PRL: nothing NEW to do, legacy file support
      mLabels.push_back(LabelStruct {
         SelectedRegion{ t0, t0 }, in->GetNextLine()
      });
   }

   if (in->GetNextLine() != wxT("MLabelsEnd"))
      return false;
   SortLabels();
   return true;
}

bool LabelTrack::Save(wxTextFile * out, bool overwrite)
{
   out->AddLine(wxT("NumMLabels"));
   int len = mLabels.size();
   out->AddLine(wxString::Format(wxT("%d"), len));

   for (auto pLabel : mLabels) {
      const auto &labelStruct = *pLabel;
      out->AddLine(wxString::Format(wxT("%lf"), labelStruct.selectedRegion.mT0));
      out->AddLine(labelStruct.title);
   }
   out->AddLine(wxT("MLabelsEnd"));

   return true;
}
#endif

Track::Holder LabelTrack::Cut(double t0, double t1)
{
   auto tmp = Copy(t0, t1);
   if (!tmp)
      return{};
   if (!Clear(t0, t1))
      return{};

   return tmp;
}

#if 0
Track::Holder LabelTrack::SplitCut(double t0, double t1)
{
   // SplitCut() == Copy() + SplitDelete()

   Track::Holder tmp = Copy(t0, t1);
   if (!tmp)
      return {};
   if (!SplitDelete(t0, t1))
      return {};

   return tmp;
}
#endif

Track::Holder LabelTrack::Copy(double t0, double t1) const
{
   auto tmp = std::make_unique<LabelTrack>(GetDirManager());
   const auto lt = static_cast<LabelTrack*>(tmp.get());

   for (auto &labelStruct: mLabels) {
      LabelStruct::TimeRelations relation =
                        labelStruct.RegionRelation(t0, t1, this);
      if (relation == LabelStruct::SURROUNDS_LABEL) {
         LabelStruct l {
            labelStruct.selectedRegion,
            labelStruct.getT0() - t0,
            labelStruct.getT1() - t0,
            labelStruct.title
         };
         lt->mLabels.push_back(l);
      }
      else if (relation == LabelStruct::WITHIN_LABEL) {
         LabelStruct l {
            labelStruct.selectedRegion,
            0,
            t1-t0,
            labelStruct.title
         };
         lt->mLabels.push_back(l);
      }
      else if (relation == LabelStruct::BEGINS_IN_LABEL) {
         LabelStruct l {
            labelStruct.selectedRegion,
            0,
            labelStruct.getT1() - t0,
            labelStruct.title
         };
         lt->mLabels.push_back(l);
      }
      else if (relation == LabelStruct::ENDS_IN_LABEL) {
         LabelStruct l {
            labelStruct.selectedRegion,
            labelStruct.getT0() - t0,
            t1 - t0,
            labelStruct.title
         };
         lt->mLabels.push_back(l);
      }
   }
   lt->mClipLen = (t1 - t0);

   // This std::move is needed to "upcast" the pointer type
   return std::move(tmp);
}


bool LabelTrack::PasteOver(double t, const Track * src)
{
   if (src->GetKind() != Track::Label)
      return false;

   int len = mLabels.size();
   int pos = 0;

   while (pos < len && mLabels[pos].getT0() < t)
      pos++;

   auto sl = static_cast<const LabelTrack *>(src);
   for (auto &labelStruct: sl->mLabels) {
      LabelStruct l {
         labelStruct.selectedRegion,
         labelStruct.getT0() + t,
         labelStruct.getT1() + t,
         labelStruct.title
      };
      mLabels.insert(mLabels.begin() + pos++, l);
      len++;
   }

   return true;
}

bool LabelTrack::Paste(double t, const Track *src)
{
   if (src->GetKind() != Track::Label)
      return false;

   LabelTrack *lt = (LabelTrack *)src;

   double shiftAmt = lt->mClipLen > 0.0 ? lt->mClipLen : lt->GetEndTime();

   ShiftLabelsOnInsert(shiftAmt, t);
   return PasteOver(t, src);
}

// This repeats the labels in a time interval a specified number of times.
bool LabelTrack::Repeat(double t0, double t1, int n)
{
   // Sanity-check the arguments
   if (n < 0 || t1 < t0)
      return false;

   double tLen = t1 - t0;

   // Insert space for the repetitions
   ShiftLabelsOnInsert(tLen * n, t1);

   // mLabels may resize as we iterate, so use subscripting
   for (unsigned int i = 0; i < mLabels.size(); ++i)
   {
      LabelStruct::TimeRelations relation =
                        mLabels[i].RegionRelation(t0, t1, this);
      if (relation == LabelStruct::SURROUNDS_LABEL)
      {
         // Label is completely inside the selection; duplicate it in each
         // repeat interval
         unsigned int pos = i; // running label insertion position in mLabels

         for (int j = 1; j <= n; j++)
         {
            const LabelStruct &label = mLabels[i];
            LabelStruct l {
               label.selectedRegion,
               label.getT0() + j * tLen,
               label.getT1() + j * tLen,
               label.title
            };

            // Figure out where to insert
            while (pos < mLabels.size() &&
                   mLabels[pos].getT0() < l.getT0())
               pos++;
            mLabels.insert(mLabels.begin() + pos, l);
         }
      }
      else if (relation == LabelStruct::BEGINS_IN_LABEL)
      {
         // Label ends inside the selection; ShiftLabelsOnInsert() hasn't touched
         // it, and we need to extend it through to the last repeat interval
         mLabels[i].selectedRegion.moveT1(n * tLen);
      }

      // Other cases have already been handled by ShiftLabelsOnInsert()
   }

   return true;
}

bool LabelTrack::Silence(double t0, double t1)
{
   int len = mLabels.size();

   // mLabels may resize as we iterate, so use subscripting
   for (int i = 0; i < len; ++i) {
      LabelStruct::TimeRelations relation =
                        mLabels[i].RegionRelation(t0, t1, this);
      if (relation == LabelStruct::WITHIN_LABEL)
      {
         // Split label around the selection
         const LabelStruct &label = mLabels[i];
         LabelStruct l {
            label.selectedRegion,
            t1,
            label.getT1(),
            label.title
         };

         mLabels[i].selectedRegion.setT1(t0);

         // This might not be the right place to insert, but we sort at the end
         ++i;
         mLabels.insert(mLabels.begin() + i, l);
      }
      else if (relation == LabelStruct::ENDS_IN_LABEL)
      {
         // Beginning of label to selection end
         mLabels[i].selectedRegion.setT0(t1);
      }
      else if (relation == LabelStruct::BEGINS_IN_LABEL)
      {
         // End of label to selection beginning
         mLabels[i].selectedRegion.setT1(t0);
      }
      else if (relation == LabelStruct::SURROUNDS_LABEL)
      {
         DeleteLabel( i );
         len--;
         i--;
      }
   }

   SortLabels();

   return true;
}

bool LabelTrack::InsertSilence(double t, double len)
{
   for (auto &labelStruct: mLabels) {
      double t0 = labelStruct.getT0();
      double t1 = labelStruct.getT1();
      if (t0 >= t)
         t0 += len;

      if (t1 >= t)
         t1 += len;
      labelStruct.selectedRegion.setTimes(t0, t1);
   }

   return true;
}

int LabelTrack::GetNumLabels() const
{
   return mLabels.size();
}

const LabelStruct *LabelTrack::GetLabel(int index) const
{
   return &mLabels[index];
}

int LabelTrack::GetLabelIndex(double t, double t1)
{
   //We'd have liked to have times in terms of samples,
   //because then we're doing an intrger comparison.
   //Never mind.  Instead we look for near enough.
   //This level of (in)accuracy is only a problem if we
   //deal with sounds in the MHz range.
   const double delta = 1.0e-7;
   { int i = -1; for (auto &labelStruct : mLabels) { ++i;
      if( fabs( labelStruct.getT0() - t ) > delta )
         continue;
      if( fabs( labelStruct.getT1() - t1 ) > delta )
         continue;
      return i;
   }}

   return wxNOT_FOUND;
}

int LabelTrack::AddLabel(const SelectedRegion &selectedRegion,
                         const wxString &title, int restoreFocus)
{
   LabelStruct l { selectedRegion, title };
   mInitialCursorPos = mCurrentCursorPos = title.length();

   int len = mLabels.size();
   int pos = 0;

   while (pos < len && mLabels[pos].getT0() < selectedRegion.t0())
      pos++;

   mLabels.insert(mLabels.begin() + pos, l);

   mSelIndex = pos;

   // Make sure the caret is visible
   //
   // LLL: The cursor will not be drawn when the first label
   //      is added since mDrawCursor will be false.  Presumably,
   //      if the user adds a label, then a cursor should be drawn
   //      to indicate that typing is expected.
   //
   //      If the label is added during actions like import, then the
   //      mDrawCursor flag will be reset once the action is complete.
   mDrawCursor = true;

   mRestoreFocus = restoreFocus;

   return pos;
}

void LabelTrack::DeleteLabel(int index)
{
   wxASSERT((index < (int)mLabels.size()));
   mLabels.erase(mLabels.begin() + index);
   // IF we've deleted the selected label
   // THEN set no label selected.
   if( mSelIndex== index )
   {
      mSelIndex = -1;
      mCurrentCursorPos = 1;
   }
   // IF we removed a label before the selected label
   // THEN the NEW selected label number is one less.
   else if( index < mSelIndex )
   {
      mSelIndex--;
   }
}

wxBitmap & LabelTrack::GetGlyph( int i)
{
   return theTheme.Bitmap( i + bmpLabelGlyph0);
}

// This one XPM spec is used to generate a number of
// different wxIcons.
/* XPM */
static const char *const GlyphXpmRegionSpec[] = {
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
void LabelTrack::CreateCustomGlyphs()
{
   int iConfig;
   int iHighlight;
   int index;
   const int nSpecRows =
      sizeof( GlyphXpmRegionSpec )/sizeof( GlyphXpmRegionSpec[0]);
   const char *XmpBmp[nSpecRows];

   // The glyphs are declared static wxIcon; so we only need
   // to create them once, no matter how many LabelTracks.
   if( mbGlyphsReady )
      return;

   // We're about to tweak the basic color spec to get 12 variations.
   for( iConfig=0;iConfig<NUM_GLYPH_CONFIGS;iConfig++)
   {
      for( iHighlight=0;iHighlight<NUM_GLYPH_HIGHLIGHTS;iHighlight++)
      {
         index = iConfig + NUM_GLYPH_CONFIGS * iHighlight;
         // Copy the basic spec...
         memcpy( XmpBmp, GlyphXpmRegionSpec, sizeof( GlyphXpmRegionSpec ));
         // The higlighted region (if any) is white...
         if( iHighlight==1 ) XmpBmp[5]="5 c #FFFFFF";
         if( iHighlight==2 ) XmpBmp[6]="6 c #FFFFFF";
         if( iHighlight==3 ) XmpBmp[7]="7 c #FFFFFF";
         // For left or right arrow the other side of the glyph
         // is the transparent color.
         if( iConfig==0) { XmpBmp[3]="3 c none"; XmpBmp[5]="5 c none"; }
         if( iConfig==1) { XmpBmp[4]="4 c none"; XmpBmp[7]="7 c none"; }
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
   wxASSERT( (mIconWidth %2)==1);

   mbGlyphsReady=true;
}

/// Returns true for keys we capture to start a label.
bool LabelTrack::IsGoodLabelFirstKey(const wxKeyEvent & evt)
{
   int keyCode = evt.GetKeyCode();
   return (keyCode < WXK_START
                  && keyCode != WXK_SPACE && keyCode != WXK_DELETE && keyCode != WXK_RETURN) ||
          (keyCode >= WXK_NUMPAD0 && keyCode <= WXK_DIVIDE) ||
          (keyCode >= WXK_NUMPAD_EQUAL && keyCode <= WXK_NUMPAD_DIVIDE) ||
#if defined(__WXMAC__)
          (keyCode > WXK_RAW_CONTROL) ||
#endif
          (keyCode > WXK_WINDOWS_MENU);
}

/// This returns true for keys we capture for label editing.
bool LabelTrack::IsGoodLabelEditKey(const wxKeyEvent & evt)
{
   int keyCode = evt.GetKeyCode();

   // Accept everything outside of WXK_START through WXK_COMMAND, plus the keys
   // within that range that are usually printable, plus the ones we use for
   // keyboard navigation.
   return keyCode < WXK_START ||
          (keyCode >= WXK_END && keyCode < WXK_UP) ||
          (keyCode == WXK_RIGHT) ||
          (keyCode >= WXK_NUMPAD0 && keyCode <= WXK_DIVIDE) ||
          (keyCode >= WXK_NUMPAD_SPACE && keyCode <= WXK_NUMPAD_ENTER) ||
          (keyCode >= WXK_NUMPAD_HOME && keyCode <= WXK_NUMPAD_END) ||
          (keyCode >= WXK_NUMPAD_DELETE && keyCode <= WXK_NUMPAD_DIVIDE) ||
#if defined(__WXMAC__)
          (keyCode > WXK_RAW_CONTROL) ||
#endif
          (keyCode > WXK_WINDOWS_MENU);
}

/// Sorts the labels in order of their starting times.
/// This function is called often (whilst dragging a label)
/// We expect them to be very nearly in order, so insertion
/// sort (with a linear search) is a reasonable choice.
void LabelTrack::SortLabels()
{
   const auto begin = mLabels.begin();
   const auto nn = (int)mLabels.size();
   int i = 1;
   while (true)
   {
      // Find the next disorder
      while (i < nn && mLabels[i - 1].getT0() <= mLabels[i].getT0())
         ++i;
      if (i >= nn)
         break;

      // Where must element i sink to?  At most i - 1, maybe less
      int j = i - 2;
      while( (j >= 0) && (mLabels[j].getT0() > mLabels[i].getT0()) )
         --j;
      ++j;

      // Now fix the disorder
      std::rotate(
         begin + j,
         begin + i,
         begin + i + 1
      );

      // Various indices need to be updated with the moved items...
      auto update = [=](int &index) {
         if( index <= i ) {
            if( index == i )
               index = j;
            else if( index >= j)
               ++index;
         }
      };
      update(mMouseOverLabelLeft);
      update(mMouseOverLabelRight);
      update(mSelIndex);
   }
}

wxString LabelTrack::GetTextOfLabels(double t0, double t1) const
{
   bool firstLabel = true;
   wxString retVal;

   for (auto &labelStruct: mLabels) {
      if (labelStruct.getT0() >= t0 &&
          labelStruct.getT1() <= t1)
      {
         if (!firstLabel)
            retVal += '\t';
         firstLabel = false;
         retVal += labelStruct.title;
      }
   }

   return retVal;
}
