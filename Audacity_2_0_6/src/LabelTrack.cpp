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

#include <stdio.h>

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
#include <wx/utils.h>

#include "LabelTrack.h"
#include "DirManager.h"
#include "Internat.h"
#include "Prefs.h"
#include "Theme.h"
#include "AllThemeResources.h"
#include "AColor.h"
#include "Project.h"
#include "TrackArtist.h"
#include "commands/CommandManager.h"

#include "CaptureEvents.h"
#include "effects/TimeWarper.h"

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

LabelTrack *TrackFactory::NewLabelTrack()
{
   return new LabelTrack(mDirManager);
}

LabelTrack::LabelTrack(DirManager * projDirManager):
   Track(projDirManager),
   mbHitCenter(false),
   mOldEdge(-1),
   mSelIndex(-1),
   mMouseOverLabelLeft(-1),
   mMouseOverLabelRight(-1),
   mClipLen(0.0),
   mIsAdjustingLabel(false)
{
   SetDefaultName(_("Label Track"));
   SetName(GetDefaultName());

   // Label tracks are narrow
   // Default is to allow two rows so that new users get the
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
   int len = orig.mLabels.Count();

   for (int i = 0; i < len; i++) {
      LabelStruct *l = new LabelStruct();
      l->t = orig.mLabels[i]->t;
      l->t1 = orig.mLabels[i]->t1;
      l->title = orig.mLabels[i]->title;
      mLabels.Add(l);
   }
   mSelIndex = orig.mSelIndex;

   // reset flags
   ResetFlags();
}

LabelTrack::~LabelTrack()
{
   int len = mLabels.Count();

   for (int i = 0; i < len; i++)
      delete mLabels[i];
}

void LabelTrack::SetOffset(double dOffset)
{
   int len = mLabels.Count();
   for (int i = 0; i < len; i++)
   {
      mLabels[i]->t += dOffset;
      mLabels[i]->t1 += dOffset;
   }
}

bool LabelTrack::Clear(double b, double e)
{
   for (size_t i=0;i<mLabels.GetCount();i++){
      LabelStruct::TimeRelations relation =
                        mLabels[i]->RegionRelation(b, e, this);
      if (relation == LabelStruct::BEFORE_LABEL) {
         mLabels[i]->t  = mLabels[i]->t  - (e-b);
         mLabels[i]->t1 = mLabels[i]->t1 - (e-b);
      } else if (relation == LabelStruct::SURROUNDS_LABEL) {
         DeleteLabel( i );
         i--;
      } else if (relation == LabelStruct::ENDS_IN_LABEL) {
         mLabels[i]->t  = b;
         mLabels[i]->t1 = mLabels[i]->t1 - (e - mLabels[i]->t);
      } else if (relation == LabelStruct::BEGINS_IN_LABEL) {
         mLabels[i]->t1 = b;
      } else if (relation == LabelStruct::WITHIN_LABEL) {
         mLabels[i]->t1 = mLabels[i]->t1 - (e-b);
      }
   }

   return true;
}

//used when we want to use clear only on the labels
bool LabelTrack::SplitDelete(double b, double e)
{
   for (size_t i=0;i<mLabels.GetCount();i++) {
      LabelStruct::TimeRelations relation =
                        mLabels[i]->RegionRelation(b, e, this);
      if (relation == LabelStruct::SURROUNDS_LABEL) {
         DeleteLabel(i);
         i--;
      } else if (relation == LabelStruct::WITHIN_LABEL) {
         mLabels[i]->t1 = mLabels[i]->t1 - (e-b);
      } else if (relation == LabelStruct::ENDS_IN_LABEL) {
         mLabels[i]->t  = e;
      } else if (relation == LabelStruct::BEGINS_IN_LABEL) {
         mLabels[i]->t1 = b;
      }
   }

   return true;
}
void LabelTrack::ShiftLabelsOnInsert(double length, double pt)
{
   for (unsigned int i=0;i<mLabels.GetCount();i++) {
      LabelStruct::TimeRelations relation =
                        mLabels[i]->RegionRelation(pt, pt, this);

      if (relation == LabelStruct::BEFORE_LABEL) {
         mLabels[i]->t = mLabels[i]->t + length;
         mLabels[i]->t1 = mLabels[i]->t1 + length;
      }
      else if (relation == LabelStruct::WITHIN_LABEL) {
         mLabels[i]->t1 = mLabels[i]->t1 + length;
      }
   }
}

void LabelTrack::ChangeLabelsOnReverse(double b, double e)
{
   for (size_t i=0; i<mLabels.GetCount(); i++) {
      if (mLabels[i]->RegionRelation(b, e, this) ==
                                    LabelStruct::SURROUNDS_LABEL)
      {
         double aux     = b + (e - mLabels[i]->t1);
         mLabels[i]->t1 = e - (mLabels[i]->t - b);
         mLabels[i]->t  = aux;
      }
   }
   SortLabels();
}

void LabelTrack::ScaleLabels(double b, double e, double change)
{
   for (unsigned int i=0;i<mLabels.GetCount();i++){
      mLabels[i]->t = AdjustTimeStampOnScale(mLabels[i]->t, b, e, change);
      mLabels[i]->t1 = AdjustTimeStampOnScale(mLabels[i]->t1, b, e, change);
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
   for (int i = 0; i < (int)mLabels.GetCount(); ++i) {
      double &labelT0 = mLabels[i]->t; labelT0 = warper.Warp(labelT0);
      double &labelT1 = mLabels[i]->t1; labelT1 = warper.Warp(labelT1);
   }
}

void LabelTrack::ResetFlags()
{
   mMouseXPos = -1;
   mXPos1 = -1;
   mXPos2 = -1;
   mDragXPos = -1;
   mInitialCursorPos = 1;
   mCurrentCursorPos = 1;
   mResetCursorPos = false;
   mRightDragging = false;
   mDrawCursor = false;
}

void LabelTrack::ResetFont()
{
   mFontHeight = -1;
   wxString facename = gPrefs->Read(wxT("/GUI/LabelFontFacename"), wxT(""));
   int size = gPrefs->Read(wxT("/GUI/LabelFontSize"), 12);
   if (facename != wxT("")) {
      msFont = wxFont(size, wxFONTFAMILY_DEFAULT, wxFONTSTYLE_NORMAL,
                      wxFONTWEIGHT_NORMAL, FALSE, facename,
                      wxFONTENCODING_SYSTEM);
   }
   else {
      msFont = wxFont(size, wxFONTFAMILY_DEFAULT, wxFONTSTYLE_NORMAL,
                      wxFONTWEIGHT_NORMAL);
   }
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
void LabelTrack::ComputeTextPosition(const wxRect & r, int index)
{
   // xExtra is extra space
   // between the text and the endpoints.
   const int xExtra=mIconWidth;
   int x     = mLabels[index]->x;  // left endpoint
   int x1    = mLabels[index]->x1; // right endpoint.
   int width = mLabels[index]->width;

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

   mLabels[index]->xText = xText;
}

/// ComputeLayout determines which row each label
/// should be placed on, and reserves space for it.
/// Function assumes that the labels are sorted.
void LabelTrack::ComputeLayout(const wxRect & r, double h, double pps)
{
   int i;
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
   const int xStart = r.x -(int)(h*pps) -100;
   for(i=0;i<MAX_NUM_ROWS;i++)
      xUsed[i]=xStart;
   int nRowsUsed=0;

   for (i = 0; i < (int)mLabels.Count(); i++)
   {
      int x  = r.x + (int) ((mLabels[i]->t  - h) * pps);
      int x1 = r.x + (int) ((mLabels[i]->t1 - h) * pps);
      int y = r.y;

      mLabels[i]->x=x;
      mLabels[i]->x1=x1;
      mLabels[i]->y=-1;// -ve indicates nothing doing.
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
         mLabels[i]->y=y;
         // On this row we have used up to max of end marker and width.
         // Plus also allow space to show the start icon and
         // some space for the text frame.
         xUsed[iRow]=x+mLabels[i]->width+xExtra;
         if( xUsed[iRow] < x1 ) xUsed[iRow]=x1;
         ComputeTextPosition( r, i );
      }
   }
}

LabelStruct::LabelStruct()
{
   changeInitialMouseXPos = true;
   highlighted = false;
   updated = false;
   t = 0.0;
   t1 = 0.0;
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
void LabelStruct::DrawLines(wxDC & dc, const wxRect & r)
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
void LabelStruct::DrawGlyphs(wxDC & dc, const wxRect & r, int GlyphLeft, int GlyphRight)
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
void LabelStruct::DrawText(wxDC & dc, const wxRect & r)
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

void LabelStruct::DrawTextBox(wxDC & dc, const wxRect & r)
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
void LabelStruct::DrawHighlight( wxDC & dc, int xPos1, int xPos2, int charHeight)
{
   highlighted = true;
   changeInitialMouseXPos = false;

   wxPen curPen = dc.GetPen();
   curPen.SetColour(wxString(wxT("BLUE")));
   wxBrush curBrush = dc.GetBrush();
   curBrush.SetColour(wxString(wxT("BLUE")));
   if (xPos1 < xPos2)
      dc.DrawRectangle(xPos1-1, y-charHeight/2, xPos2-xPos1+1, charHeight);
   else
      dc.DrawRectangle(xPos2-1, y-charHeight/2, xPos1-xPos2+1, charHeight);
}

void LabelStruct::getXPos( wxDC & dc, int * xPos1, int cursorPos)
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

bool LabelTrack::CalcCursorX(wxWindow * parent, int * x)
{
   if (mSelIndex >= 0) {
      wxClientDC dc(parent);

      if (msFont.Ok()) {
         dc.SetFont(msFont);
      }

      mLabels[mSelIndex]->getXPos(dc, x, mCurrentCursorPos);
      *x += LabelTrack::mIconWidth / 2;
      return true;
   }

   return false;
}

/// Draw calls other functions to draw the LabelTrack.
///   @param  dc the device context
///   @param  r  the LabelTrack rectangle.
void LabelTrack::Draw(wxDC & dc, const wxRect & r, double h, double pps,
                      double sel0, double sel1)
{
   if(msFont.Ok())
      dc.SetFont(msFont);

   if (mFontHeight == -1)
      calculateFontHeight(dc);

   TrackArtist::DrawBackgroundWithSelection(&dc, r, this,
         AColor::labelSelectedBrush, AColor::labelUnselectedBrush,
         sel0, sel1, h, pps);

   int i;

#ifdef __WXMAC__
   long textWidth, textHeight;
#else
   int textWidth, textHeight;
#endif

   // Get the text widths.
   // TODO: Make more efficient by only re-computing when a
   // text label title changes.
   for (i = 0; i < (int)mLabels.Count(); i++)
   {
      dc.GetTextExtent(mLabels[i]->title, &textWidth, &textHeight);
      mLabels[i]->width = textWidth;
   }

   // TODO: And this only needs to be done once, but we
   // do need the dc to do it.
   // We need to set mTextHeight to something sensible,
   // guarding against the case where there are no
   // labels or all are empty strings, which for example
   // happens with a new label track.
   dc.GetTextExtent(wxT("Demo Text x^y"), &textWidth, &textHeight);
   mTextHeight = (int)textHeight;
   ComputeLayout( r, h , pps );
   dc.SetTextForeground(theTheme.Colour( clrLabelTrackText));
   dc.SetBackgroundMode(wxTRANSPARENT);
   dc.SetBrush(AColor::labelTextNormalBrush);
   dc.SetPen(AColor::labelSurroundPen);
   const int nLabels = (int)mLabels.Count();
   int GlyphLeft;
   int GlyphRight;
   // Now we draw the various items in this order,
   // so that the correct things overpaint each other.

   // Draw vertical lines that show where the end positions are.
   for (i = 0; i < nLabels; i++)
   {
      mLabels[i]->DrawLines( dc, r );
   }

   // Draw the end glyphs.
   for (i = 0; i < nLabels; i++)
   {
      GlyphLeft=0;
      GlyphRight=1;
      if( i==mMouseOverLabelLeft )
         GlyphLeft = mbHitCenter ? 6:9;
      if( i==mMouseOverLabelRight )
         GlyphRight = mbHitCenter ? 7:4;
      mLabels[i]->DrawGlyphs( dc, r, GlyphLeft, GlyphRight );
   }

   // Draw the label boxes.
   for (i = 0; i < nLabels; i++)
   {
      if( mSelIndex==i) dc.SetBrush(AColor::labelTextEditBrush);
      mLabels[i]->DrawTextBox( dc, r );
      if( mSelIndex==i) dc.SetBrush(AColor::labelTextNormalBrush);
   }

   // Draw highlights
   if ((mDragXPos != -1) && (mSelIndex >= 0 ))
   {
      // find the left X pos of highlighted area
      mLabels[mSelIndex]->getXPos(dc, &mXPos1, mInitialCursorPos);
      // for preventing dragging glygh from changing current cursor position
      if (mResetCursorPos) {
         // set end dragging position to current cursor position
         SetCurrentCursorPosition(dc, mDragXPos);
         mResetCursorPos = false;
      }
      // find the right X pos of highlighted area
      mLabels[mSelIndex]->getXPos(dc, &mXPos2, mCurrentCursorPos);
      mLabels[mSelIndex]->DrawHighlight(dc, mXPos1, mXPos2, mFontHeight);
   }

   // Draw the text and the label boxes.
   for (i = 0; i < nLabels; i++)
   {
      if( mSelIndex==i) dc.SetBrush(AColor::labelTextEditBrush);
      mLabels[i]->DrawText( dc, r );
      if( mSelIndex==i) dc.SetBrush(AColor::labelTextNormalBrush);
   }

   // Draw the cursor, if there is one.
   if( mSelIndex >=0 )
   {
      i = mSelIndex;
      int xPos = mLabels[i]->xText;

      // if mouse is clicked in text box
      if (mMouseXPos != -1)
      {
         // set current cursor position
         SetCurrentCursorPosition(dc, (int) mMouseXPos);
         // for preventing from resetting by shift+mouse left button
         // set initialCursorPos equal to currentCursorPos
         if (mLabels[mSelIndex]->changeInitialMouseXPos)
            mInitialCursorPos = mCurrentCursorPos;
         mDrawCursor = true;
         mMouseXPos = -1;
      }

      if( mCurrentCursorPos > 0)
      {
         // Calculate the width of the substring and add it to Xpos
         int partWidth;
         dc.GetTextExtent((mLabels[i]->title).Left(mCurrentCursorPos), &partWidth, NULL);
         xPos += partWidth;
      }

      // Draw the cursor
      wxPen currentPen = dc.GetPen();
      const int CursorWidth=2;
      if (mDrawCursor) {
         currentPen.SetWidth(CursorWidth);
         AColor::Line(dc,
                      xPos-1, mLabels[i]->y - mFontHeight/2 + 1,
                      xPos-1, mLabels[i]->y + mFontHeight/2 - 1);
         currentPen.SetWidth(1);
      }
   }
}

/// Set the cursor position according to x position of mouse
/// uses GetTextExtent to find the character position
/// corresponding to the x pixel position.
void LabelTrack::SetCurrentCursorPosition(wxDC & dc, int xPos)
{
   // A bool indicator to see if set the cursor position or not
   bool finished = false;
   int charIndex = 1;
   int partWidth;
   int oneWidth;
   double bound;
   wxString subString;
   while (!finished && (charIndex < (int)mLabels[mSelIndex]->title.length() + 1))
   {
      subString = (mLabels[mSelIndex]->title).Left(charIndex);
      // Get the width of substring
      dc.GetTextExtent(subString, &partWidth, NULL);
      if (charIndex > 1)
      {
         // Get the width of the last character
         dc.GetTextExtent(subString.Right(1), &oneWidth, NULL);
         bound = mLabels[mSelIndex]->xText + partWidth - oneWidth * 0.5;
      }
      else
      {
         bound = mLabels[mSelIndex]->xText + partWidth * 0.5;
      }

      if (xPos <= bound)
      {
         // Found
         mCurrentCursorPos = charIndex - 1;
         finished = true;
      }
      else
      {
         // Advance
         charIndex++;
      }
   }
   if (!finished)
   {
      // Cursor should be in the last position
      mCurrentCursorPos = mLabels[mSelIndex]->title.length();
   }
}

void LabelTrack::calculateFontHeight(wxDC & dc)
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
   if (!mLabels[mSelIndex]->highlighted)
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

   wxString left=wxT("");
   wxString right=wxT("");
   wxString text = mLabels[mSelIndex]->title;

   // swapping to make sure currentCursorPos > initialCursorPos always
   if (mInitialCursorPos > mCurrentCursorPos) {
      int temp = mCurrentCursorPos;
      mCurrentCursorPos = mInitialCursorPos;
      mInitialCursorPos = temp;
   }

   // data for cutting
   wxString data = text.Mid(mInitialCursorPos, mCurrentCursorPos-mInitialCursorPos);

   // get left-remaining text
   if (mInitialCursorPos > 0) {
      left = text.Mid(0, mInitialCursorPos);
   }

   // get right-remaining text
   if (mCurrentCursorPos < (int)text.Length()) {
      right = text.Mid(mCurrentCursorPos, text.Length()-mCurrentCursorPos);
   }

   // set title to the combination of the two remainders
   mLabels[mSelIndex]->title = left + right;

   // copy data onto clipboard
   if (wxTheClipboard->Open()) {
#if defined(__WXGTK__) && defined(HAVE_GTK)
      CaptureEvents capture;
#endif
      wxTheClipboard->SetData(new wxTextDataObject(data));
      wxTheClipboard->Close();
   }

   // set cursor positions
   mCurrentCursorPos = left.Length();
   mInitialCursorPos = mCurrentCursorPos;
   return true;
}

/// Copy the selected text in the text box
///  @return true if text is selected in text box, false otherwise
bool LabelTrack::CopySelectedText()
{
   if (mSelIndex == -1)
      return false;
   if (!mLabels[mSelIndex]->highlighted)
      return false;

   // swapping to make sure currentCursorPos > mInitialCursorPos always
   int init = mInitialCursorPos;
   int cur = mCurrentCursorPos;
   if (init > cur) {
      cur = mInitialCursorPos;
      init = mCurrentCursorPos;
   }

   // data for copying
   wxString data = mLabels[mSelIndex]->title.Mid(init, cur-init);

   // copy the data on clipboard
   if (wxTheClipboard->Open()) {
#if defined(__WXGTK__) && defined(HAVE_GTK)
      CaptureEvents capture;
#endif
      wxTheClipboard->SetData(new wxTextDataObject(data));
      wxTheClipboard->Close();
   }

   return true;
}

/// Paste the text on the clipboard to text box
///  @return true if mouse is clicked in text box, false otherwise
bool LabelTrack::PasteSelectedText(double sel0, double sel1)
{
   if (mSelIndex == -1) {
      AddLabel(sel0, sel1, wxT(""));
   }

   wxString text;
   wxString left=wxT("");
   wxString right=wxT("");

   // if text data is available
   if (IsTextClipSupported()) {
      if (wxTheClipboard->Open()) {
#if defined(__WXGTK__) && HAVE_GTK
         CaptureEvents capture;
#endif
         wxTextDataObject data;
         wxTheClipboard->GetData(data);
         wxTheClipboard->Close();
         text = data.GetText();
      }

      // Convert control characters to blanks
      int i;
      for (i = 0; i < (int)text.Length(); i++) {
         if (wxIscntrl(text[i])) {
            text[i] = wxT(' ');
         }
      }
   }

   // if there is some highlighted text
   if (mLabels[mSelIndex]->highlighted) {
      // swapping to make sure currentCursorPos > mInitialCursorPos always
      if (mInitialCursorPos > mCurrentCursorPos) {
         int temp = mCurrentCursorPos;
         mCurrentCursorPos = mInitialCursorPos;
         mInitialCursorPos = temp;
      }

      // same as cutting
      if (mInitialCursorPos > 0) {
         left = (mLabels[mSelIndex]->title).Mid(0, mInitialCursorPos);
      }
      if (mCurrentCursorPos < (int)(mLabels[mSelIndex]->title).Length()) {
         right = (mLabels[mSelIndex]->title).Mid(mCurrentCursorPos, (mLabels[mSelIndex]->title).Length()-mCurrentCursorPos);
      }
      mLabels[mSelIndex]->title = left + text + right;
      mCurrentCursorPos = left.Length() + text.Length();
   }
   else
   {
      // insert the data on the clipboard from the cursor position
      if (mCurrentCursorPos < (int)(mLabels[mSelIndex]->title).Length()) {
         right = (mLabels[mSelIndex]->title).Mid(mCurrentCursorPos);
      }
      mLabels[mSelIndex]->title = (mLabels[mSelIndex]->title).Left(mCurrentCursorPos);
      mLabels[mSelIndex]->title += text;
      mLabels[mSelIndex]->title += right;
      mCurrentCursorPos += text.Length();
   }
   // set mInitialCursorPos equal to currentCursorPos
   mInitialCursorPos = mCurrentCursorPos;
   return true;
}


/// @return true if the text data is available in the clipboard, false otherwise
bool LabelTrack::IsTextClipSupported()
{
#if defined(__WXGTK__) && defined(HAVE_GTK)
   CaptureEvents capture;
#endif

#if defined(__WXGTK__)
   // AWD: work-around for bug 154: do not call wxClipboard::IsSupported()
   // when handling a keyboard event
   if (!wxGetApp().inKbdHandler) {
#endif

      return wxTheClipboard->IsSupported(wxDF_TEXT);

#if defined (__WXGTK__)
   }

   // In keyboard handler; return false for now
   return false;
#endif
}


double LabelTrack::GetStartTime()
{
   int len = mLabels.Count();

   if (len == 0)
      return 0.0;
   else
      return mLabels[0]->t;
}

double LabelTrack::GetEndTime()
{
   //we need to scan through all the labels, because the last
   //label might not have the right-most end (if there is overlap).
   int len = mLabels.Count();
   if (len == 0)
      return 0.0;

   double end = 0.0;
   for(int i = 0; i < len; i++)
   {
      if(mLabels[i]->t1 > end)
         end = mLabels[i]->t1;
   }
   return end;
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
   //Determine the new selection.
   LabelStruct * pLabel;
   int result=0;
   const int d1=10; //distance in pixels, used for have we hit drag handle.
   const int d2=5;  //distance in pixels, used for have we hit drag handle center.

   //If not over a label, reset it
   mMouseOverLabelLeft  = -1;
   mMouseOverLabelRight = -1;
   mbHitCenter = false;
   for (int i = 0; i < (int)mLabels.Count(); i++)
   {
      pLabel = mLabels[i];

      //over left or right selection bound
      //Check right bound first, since it is drawn after left bound,
      //so give it precedence for matching/highlighting.
      if( abs(pLabel->y - (y - (LabelTrack::mTextHeight+3)/2)) < d1 &&
               abs(pLabel->x1 - d2 -x) < d1)
      {
         mMouseOverLabelRight = i;
         if(abs(pLabel->x1 - x) < d2 )
         {
            mbHitCenter = true;
            // If left and right co-incident at this resolution, then we drag both.
            // We could be a little less stringent about co-incidence here if we liked.
            if( abs(pLabel->x1-pLabel->x) < 1.0 )
            {
               result |=1;
               mMouseOverLabelLeft = i;
            }
         }
         result |= 2;
         mInBox = false;     // to disable the dragging for selecting the text in text box
      }
      // Use else-if here rather than else to avoid detecting left and right
      // of the same label.
      else if(   abs(pLabel->y - (y - (LabelTrack::mTextHeight+3)/2)) < d1 &&
            abs(pLabel->x + d2 - x) < d1 )
      {
         mMouseOverLabelLeft = i;
         if(abs(pLabel->x - x) < d2 )
            mbHitCenter = true;
         result |= 1;
         mInBox = false;     // to disable the dragging for selecting the text in text box
      }

      // give text box better priority for selecting
      if(OverTextBox(pLabel, x, y))
      {
         result = 0;
      }

   }
   return result;
}

// return true if the mouse is over text box, false otherwise
bool LabelTrack::OverTextBox(const LabelStruct *pLabel, int x, int y)
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
void LabelStruct::AdjustEdge( int iEdge, double fNewTime)
{
   if( iEdge < 0 )
      t = fNewTime;
   else
      t1 = fNewTime;
   updated = true;
}

// We're moving the label.  Adjust both left and right edge.
void LabelStruct::MoveLabel( int iEdge, double fNewTime)
{
   double fTimeSpan = getDuration();

   if( iEdge < 0 )
   {
      t  = fNewTime;
      t1 = fNewTime+fTimeSpan;
   }
   else
   {
      t  = fNewTime-fTimeSpan;
      t1 = fNewTime;
   }
   updated = true;
}

LabelStruct::TimeRelations LabelStruct::RegionRelation(
      double reg_t0, double reg_t1, LabelTrack * WXUNUSED(parent))
{
   bool retainLabels = false;

   wxASSERT(reg_t0 <= reg_t1);
   gPrefs->Read(wxT("/GUI/RetainLabels"), &retainLabels);

   if(retainLabels) {

      // Desired behavior for edge cases: The length of the selection is smaller
      // than the length of the label if the selection is within the label or
      // matching exactly a (region) label.

      if (reg_t0 < t && reg_t1 > t1)
         return SURROUNDS_LABEL;
      else if (reg_t1 < t)
         return BEFORE_LABEL;
      else if (reg_t0 > t1)
         return AFTER_LABEL;

      else if (reg_t0 >= t && reg_t0 <= t1 && reg_t1 >= t && reg_t1 <= t1)
         return WITHIN_LABEL;

      else if (reg_t0 >= t && reg_t0 <= t1)
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
      if (reg_t0 <= t && reg_t1 >= t1)
         return SURROUNDS_LABEL;
      else if (reg_t1 <= t)
         return BEFORE_LABEL;
      else if (reg_t0 >= t1)
         return AFTER_LABEL;

      // At this point, all point labels should have returned.

      else if (reg_t0 > t && reg_t0 < t1 && reg_t1 > t && reg_t1 < t1)
         return WITHIN_LABEL;

      // Knowing that none of the other relations match simplifies remaining
      // tests
      else if (reg_t0 > t && reg_t0 < t1)
         return BEGINS_IN_LABEL;
      else
         return ENDS_IN_LABEL;

   }
}

/// If the index is for a real label, adjust its left or right boundary.
/// @iLabel - index of label, -1 for none.
/// @iEdge - which edge is requested to move, -1 for left +1 for right.
/// @bAllowSwapping - if we can switch which edge is being dragged.
/// fNewTime - the new time for this edge of the label.
void LabelTrack::MayAdjustLabel( int iLabel, int iEdge, bool bAllowSwapping, double fNewTime)
{
   if( iLabel < 0 )
      return;
   LabelStruct * pLabel = mLabels[ iLabel ];

   // Adjust the requested edge.
   pLabel->AdjustEdge( iEdge, fNewTime );
   // If the label is not inverted, then we are done.
   if( pLabel->t <= pLabel->t1 )
      return;

   // If swapping's not allowed we must also move the edge
   // we didn't move.  Then we're done.
   if( !bAllowSwapping )
   {
      pLabel->AdjustEdge( -iEdge, fNewTime );
      return;
   }

   // Swapping's allowed and we moved the 'wrong' edge.
   // Swap the edges.
   double fTemp = pLabel->t;
   pLabel->t = pLabel->t1;
   pLabel->t1 = fTemp;

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
   mLabels[ iLabel ]->MoveLabel( iEdge, fNewTime );
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

/// HandleMouse gets called with every mouse move or click.
///
bool LabelTrack::HandleMouse(const wxMouseEvent & evt,
                             wxRect & r, double h, double pps,
                             double *newSel0, double *newSel1)
{
   if(evt.LeftUp())
   {
      bool lupd = false, rupd = false;
      if(mIsAdjustingLabel) {
         if(mMouseOverLabelLeft>=0) {
            lupd = mLabels[mMouseOverLabelLeft]->updated;
            mLabels[mMouseOverLabelLeft]->updated = false;
         }
         if(mMouseOverLabelRight>=0) {
            rupd = mLabels[mMouseOverLabelRight]->updated;
            mLabels[mMouseOverLabelRight]->updated = false;
         }
      }
#if 0
      // AWD: Due to wxWidgets bug #7491 (fix not ported to 2.8 branch) we
      // should never write the primary selection. We can enable this block
      // when we move to the 3.0 branch (or if a fixed 2.8 version is released
      // and we can do a runtime version check)
#if defined (__WXGTK__) && defined (HAVE_GTK)
      // On GTK, if we just dragged out a text selection, set the primary
      // selection
      else {
         if (mInitialCursorPos != mCurrentCursorPos) {
            wxTheClipboard->UsePrimarySelection(true);
            CopySelectedText();
            wxTheClipboard->UsePrimarySelection(false);
         }
      }
#endif
#endif

      mIsAdjustingLabel = false;
      mMouseOverLabelLeft  = -1;
      mMouseOverLabelRight = -1;
      return lupd || rupd;
   }

   if(evt.Dragging())
   {
      // if dragging happens in text box
      if (mInBox) {
         // end dragging x position in pixels
         // set flag to update current cursor position
         mDragXPos = evt.m_x;
         mResetCursorPos = true;

         // if it's an invalid dragging, disable displaying
         if (mRightDragging) {
            mDragXPos = -1;
            mRightDragging = false;
         }
      }

      //If we are currently adjusting a label,
      //just reset its value and redraw.
      if(mIsAdjustingLabel )  // This guard is necessary but hides another bug.  && mSelIndex != -1)
      {
         // LL:  Constrain to inside track rectangle for now.  Should be changed
         //      to allow scrolling while dragging labels
         int x = Constrain( evt.m_x + mxMouseDisplacement - r.x, 0, r.width - 2);

         // If exactly one edge is selected we allow swapping
         bool bAllowSwapping = (mMouseOverLabelLeft >=0 ) ^ ( mMouseOverLabelRight >= 0);
         // If we're on the 'dot' and nowe're moving,
         // Though shift-down inverts that.
         // and if both edges the same, then we're always moving the label.
         bool bLabelMoving = mbIsMoving;
         bLabelMoving ^= evt.ShiftDown();
         bLabelMoving |= mMouseOverLabelLeft==mMouseOverLabelRight;
         double fNewX = h + x / pps;
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
            //the new size of the label.
            *newSel0 = mLabels[mSelIndex]->t;
            *newSel1 = mLabels[mSelIndex]->t1;
         }
         SortLabels();
      }

      return false;
   }

   if( evt.ButtonDown())
   {
      //OverGlyph sets mMouseOverLabel to be the chosen label.
      int iGlyph = OverGlyph(evt.m_x, evt.m_y);
      mIsAdjustingLabel = iGlyph != 0;

      // reset mouseXPos if the mouse is pressed in the text box
      mMouseXPos = -1;
      mInBox = false;
      bool changeCursor = true;

      // reset the highlight indicator
      wxRect highlightedRect;
      if (mSelIndex != -1) {
         // the rectangle of highlighted area
         if (mXPos1 < mXPos2)
            highlightedRect = wxRect(mXPos1, mLabels[mSelIndex]->y - mFontHeight/2, (int) (mXPos2-mXPos1+0.5), mFontHeight);
         else
            highlightedRect = wxRect(mXPos2, mLabels[mSelIndex]->y - mFontHeight/2, (int) (mXPos1-mXPos2+0.5), mFontHeight);

         // reset when left button is down
         if (evt.LeftDown())
            mLabels[mSelIndex]->highlighted = false;
         // reset when right button is down outside text box
         if (evt.RightDown())
         {
            if (!highlightedRect.Contains(evt.m_x, evt.m_y))
            {
               mCurrentCursorPos=0;
               mInitialCursorPos=0;
               mLabels[mSelIndex]->highlighted = false;
            }
         }
         // set changeInitialMouseXPos flag
         mLabels[mSelIndex]->changeInitialMouseXPos = true;
      }

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
            t = (mLabels[mMouseOverLabelRight]->t1+mLabels[mMouseOverLabelLeft]->t)/2.0f;
            // If we're moving two edges, then it's a move (label size preserved)
            // if both edges are the same label, and it's an adjust (label sizes change)
            // if we're on a boundary between two different labels.
            mbIsMoving = (mMouseOverLabelLeft == mMouseOverLabelRight);
         }
         else if(mMouseOverLabelRight >=0)
         {
            t = mLabels[mMouseOverLabelRight]->t1;
         }
         else if(mMouseOverLabelLeft >=0)
         {
            t = mLabels[mMouseOverLabelLeft]->t;
         }
         mxMouseDisplacement = (int)((((t-h) * pps) + r.x )-evt.m_x);
         return false;
      }

      // disable displaying if left button is down
      if (evt.LeftDown())
         mDragXPos = -1;

      // disable displaying if right button is down outside text box
      if(mSelIndex != -1)
      {
         if (evt.RightDown())
         {
            if (!highlightedRect.Contains(evt.m_x, evt.m_y))
            {
               mDragXPos = -1;
            }
            else
               // if it's in text box, don't need to reset the current cursor position
               changeCursor = false;
         }
      }

      // Middle click on GTK: paste from primary selection
#if defined(__WXGTK__) && (HAVE_GTK)
      if (evt.MiddleDown()) {
         // Check for a click outside of the selected label's text box; in this
         // case PasteSelectedText() will start a new label at the click
         // location
         if (mSelIndex != -1) {
            if (!OverTextBox(mLabels[mSelIndex], evt.m_x, evt.m_y))
               mSelIndex = -1;
            double t = h + (evt.m_x - r.x) / pps;
            *newSel0 = t;
            *newSel1 = t;
         }

         wxTheClipboard->UsePrimarySelection(true);
         PasteSelectedText(*newSel0, *newSel1);
         wxTheClipboard->UsePrimarySelection(false);

         return false;
      }
#endif

      mSelIndex = -1;
      LabelStruct * pLabel;
      for (int i = 0; i < (int)mLabels.Count(); i++) {
         pLabel = mLabels[i];
         if(OverTextBox(pLabel, evt.m_x, evt.m_y))
         {
            mSelIndex = i;
            *newSel0 = mLabels[i]->t;
            *newSel1 = mLabels[i]->t1;
            // set mouseXPos to set current cursor position
            if (changeCursor)
               mMouseXPos = evt.m_x;
            // set mInBox flag
            mInBox = true;
            return false;
         }
      }
   }

   return false;
}

// Check for keys that we will process
bool LabelTrack::CaptureKey(wxKeyEvent & event)
{
   // Cache the keycode
   int keyCode = event.GetKeyCode();
   // Check for modifiers -- this does what wxKeyEvent::HasModifiers() should
   // do (it checks Control instead of CMD on Mac)
   bool hasMods = ((event.GetModifiers() & (wxMOD_CMD | wxMOD_ALT)) != 0);

   if (mSelIndex >= 0) {
      if (IsGoodLabelEditKey(keyCode) && !hasMods) {
         return true;
      }
   }
   else
   {
      if( IsGoodLabelFirstKey(keyCode) && !hasMods)
      {
         AudacityProject * pProj = GetActiveProject();

         // If we're playing, don't capture if the selection is the same as the
         // playback region (this helps prevent label track creation from
         // stealing unmodified kbd. shortcuts)
         if (pProj->GetAudioIOToken() > 0 &&
               gAudioIO->IsStreamActive(pProj->GetAudioIOToken()))
         {
            double t0, t1;
            pProj->GetPlayRegion(&t0, &t1);
            if (pProj->mViewInfo.sel0 == t0 && pProj->mViewInfo.sel1 == t1)
               return false;
         }

         // If there's a label there already don't capture
         if( GetLabelIndex( pProj->mViewInfo.sel0,  pProj->mViewInfo.sel1) != wxNOT_FOUND )
            return false;

         return true;
      }
   }

   return false;
}

/// KeyEvent is called for every keypress when over the label track.
bool LabelTrack::OnKeyDown(double & newSel0, double & newSel1, wxKeyEvent & event)
{
   // Only track true changes to the label
   bool updated = false;

   // Cache the keycode
   int keyCode = event.GetKeyCode();
   int mods = event.GetModifiers();

   // All editing keys are only active if we're currently editing a label
   if (mSelIndex >= 0) {
      switch (keyCode) {

      case WXK_BACK:
         {
            int len = mLabels[mSelIndex]->title.Length();

            //IF the label is not blank THEN get rid of a letter or letters according to cursor position
            if (len > 0)
            {
               // IF there are some highlighted letters, THEN delete them
               if (mLabels[mSelIndex]->highlighted) {
                  RemoveSelectedText();
               }
               else
               {
                  // delete one letter
                  if (mCurrentCursorPos > 0) {
                     mLabels[mSelIndex]->title.Remove(mCurrentCursorPos-1, 1);
                     mCurrentCursorPos--;
                  }
               }
            }
            else
            {
               // ELSE no text in text box, so delete whole label.
               DeleteLabel( mSelIndex );
            }
            mInitialCursorPos = mCurrentCursorPos;
            updated = true;
         }
         break;

      case WXK_DELETE:
      case WXK_NUMPAD_DELETE:
         {
            int len = mLabels[mSelIndex]->title.Length();

            //If the label is not blank get rid of a letter according to cursor position
            if (len > 0)
            {
               // if there are some highlighted letters, delete them
               if (mLabels[mSelIndex]->highlighted) {
                  RemoveSelectedText();
               }
               else
               {
                  // delete one letter
                  if (mCurrentCursorPos < len) {
                     mLabels[mSelIndex]->title.Remove(mCurrentCursorPos, 1);
                  }
               }
            }
            else
            {
               // delete whole label if no text in text box
               DeleteLabel( mSelIndex );
            }
            mInitialCursorPos = mCurrentCursorPos;
            updated = true;
         }
         break;

      case WXK_HOME:
      case WXK_NUMPAD_HOME:
         // Move cursor to beginning of label
         if (mods == wxMOD_SHIFT) {
            mCurrentCursorPos = 0;
            mDragXPos = 0;
         }
         else if (mods == wxMOD_NONE)
         {
            mCurrentCursorPos = 0;
            mDragXPos = -1;
            mInitialCursorPos = mCurrentCursorPos;
         }
         else {
            // Not handled
            event.Skip();
         }
         break;

      case WXK_END:
      case WXK_NUMPAD_END:
         // Move cursor to end of label
         if (mods == wxMOD_SHIFT) {
            mCurrentCursorPos = (int)mLabels[mSelIndex]->title.length();
            mDragXPos = 0;
         }
         else if (mods == wxMOD_NONE)
         {
            mCurrentCursorPos = (int)mLabels[mSelIndex]->title.length();
            mDragXPos = -1;
            mInitialCursorPos = mCurrentCursorPos;
         }
         else {
            // Not handled
            event.Skip();
         }
         break;

      case WXK_LEFT:
      case WXK_NUMPAD_LEFT:
         // Moving cursor left
         if (mods == wxMOD_SHIFT) {
            if (mCurrentCursorPos > 0) {
               mCurrentCursorPos--;
               mDragXPos = 0;
            }
         }
         else if (mods == wxMOD_NONE) {
            if (mCurrentCursorPos > 0) {
               mCurrentCursorPos--;
               mDragXPos = -1;
               mInitialCursorPos = mCurrentCursorPos;
            }
         }
         else {
            // Not handled
            event.Skip();
         }
         break;

      case WXK_RIGHT:
      case WXK_NUMPAD_RIGHT:
         // Moving cursor right
         if (mods == wxMOD_SHIFT) {
            if (mCurrentCursorPos < (int)mLabels[mSelIndex]->title.length()) {
               mCurrentCursorPos++;
               mDragXPos = 0;
            }
         }
         else if (mods == wxMOD_NONE)
         {
            if (mCurrentCursorPos < (int)mLabels[mSelIndex]->title.length()) {
               mCurrentCursorPos++;
               mDragXPos = -1;
               mInitialCursorPos = mCurrentCursorPos;
            }
         }
         else {
            // Not handled
            event.Skip();
         }
         break;

      case WXK_RETURN:
      case WXK_NUMPAD_ENTER:

      case WXK_ESCAPE:
         mSelIndex = -1;
         break;

      case WXK_TAB:
      case WXK_NUMPAD_TAB:
         if (event.ShiftDown()) {
               mSelIndex--;
         } else {
               mSelIndex++;
         }

         if (mSelIndex >= 0 && mSelIndex < (int)mLabels.Count()) {
            mCurrentCursorPos = mLabels[mSelIndex]->title.Length();
            //Set the selection region to be equal to the selection bounds of the tabbed-to label.
            newSel0 = mLabels[mSelIndex]->t;
            newSel1 = mLabels[mSelIndex]->t1;
         }
         else {
            mSelIndex = -1;
         }
         break;

      default:
         event.Skip();
         break;
      }
   }
   else
   {
      switch (keyCode) {

      case WXK_TAB:
      case WXK_NUMPAD_TAB:
         if (!mLabels.IsEmpty()) {
            int len = (int) mLabels.Count();
            if (event.ShiftDown()) {
               mSelIndex = len - 1;
               if (newSel0 > mLabels[0]->t) {
                  while (mSelIndex >= 0 && mLabels[mSelIndex]->t >= newSel0) {
                     mSelIndex--;
                  }
               }
            } else {
               mSelIndex = 0;
               if (newSel0 < mLabels[len - 1]->t) {
                  while (mSelIndex < len && mLabels[mSelIndex]->t <= newSel0) {
                     mSelIndex++;
                  }
               }
            }

            if (mSelIndex >= 0 && mSelIndex < len) {
               mCurrentCursorPos = mLabels[mSelIndex]->title.Length();
               //Set the selection region to be equal to the selection bounds of the tabbed-to label.
               newSel0 = mLabels[mSelIndex]->t;
               newSel1 = mLabels[mSelIndex]->t1;
            }
            else {
               mSelIndex = -1;
            }
         }
         break;

      default:
         event.Skip();
         break;
      }
   }

   // Make sure the caret is visible
   mDrawCursor = true;

   return updated;
}

/// OnChar is called for incoming characters -- that's any keypress not handled
/// by OnKeyDown.
bool LabelTrack::OnChar(double & WXUNUSED(newSel0), double & WXUNUSED(newSel1), wxKeyEvent & event)
{
   // Only track true changes to the label
   bool updated = false;

   // Cache the keycode
   int keyCode = event.GetKeyCode();
   wxChar charCode = keyCode;
#if wxUSE_UNICODE
   charCode = event.GetUnicodeKey();
#endif

   // We still have some filtering to do. Character events can be generated for,
   // i.e., the F keys, and if they aren't handled in OnKeyDown() or in the
   // command manager we get them here.

   // AWD: the following behavior is not really documented (I figured it out by
   // entering lots of Unicode characters on various OSes), and it's possible
   // that different versions of wxWidgets act differently. It's unfortunately
   // the only way I can find to allow input of full Unicode ranges without
   // breaking other stuff (Audacity's command manager, keyboard menu
   // navigation, Windows' Alt-+-xxxx arbitrary Unicode entry, etc.)
   bool bogusChar =
#if defined(__WXMSW__) && wxUSE_UNICODE
      // In Windows Unicode builds, these have keyCode not matching charCode
      (keyCode != (int)charCode) ||
#else
      // In Windows non-unicode, GTK+, and Mac builds the keyCode comes in the
      // WXK_* range
      (keyCode >= WXK_START && keyCode <= WXK_COMMAND) ||
#endif
      // Avoid modified characters, but allow Alt (option) on Mac because
      // several legit characters come in with it set.
      (event.CmdDown()) ||
#if !defined(__WXMAC__)
      (event.AltDown()) ||
#endif
      // Avoid control characters on all platforms; Casting to wxUChar to avoid
      // assertions in Windows non-Unicode builds...
      (wxIscntrl((wxUChar)charCode));

   if (bogusChar) {
      event.Skip();
      return false;
   }


   // If we've reached this point and aren't currently editing, add new label
   if (mSelIndex < 0) {
      // Don't create a new label for a space
      if (wxIsspace((wxUChar)charCode)) {
         event.Skip();
         return false;
      }
      SetSelected(true);
      AudacityProject *p = GetActiveProject();
      AddLabel(p->mViewInfo.sel0, p->mViewInfo.sel1);
      p->PushState(_("Added label"), _("Label"));
   }

   //
   // Now we are definitely in a label; append the incoming character
   //

   // Test if cursor is in the end of string or not
   if (mLabels[mSelIndex]->highlighted) {
      RemoveSelectedText();
   }

   if (mCurrentCursorPos < (int)mLabels[mSelIndex]->title.length()) {
      // Get substring on the righthand side of cursor
      wxString rightPart = mLabels[mSelIndex]->title.Mid(mCurrentCursorPos);
      // Set title to substring on the lefthand side of cursor
      mLabels[mSelIndex]->title = mLabels[mSelIndex]->title.Left(mCurrentCursorPos);
      //append charcode
      mLabels[mSelIndex]->title += charCode;
      //append the right part substring
      mLabels[mSelIndex]->title += rightPart;
   }
   else
   {
      //append charCode
      mLabels[mSelIndex]->title += charCode;
   }
   //moving cursor position forward
   mCurrentCursorPos++;
   mInitialCursorPos = mCurrentCursorPos;
   updated = true;

   // Make sure the caret is visible
   mDrawCursor = true;

   return updated;
}

void LabelTrack::RemoveSelectedText()
{
   wxString left = wxT("");
   wxString right = wxT("");

   if (mInitialCursorPos > mCurrentCursorPos) {
      int temp = mCurrentCursorPos;
      mCurrentCursorPos = mInitialCursorPos;
      mInitialCursorPos = temp;
   }

   if (mInitialCursorPos > 0) {
      left = (mLabels[mSelIndex]->title).Mid(0, mInitialCursorPos);
   }
   if (mCurrentCursorPos < (int)(mLabels[mSelIndex]->title).Length()) {
      right = (mLabels[mSelIndex]->title).Mid(mCurrentCursorPos, (mLabels[mSelIndex]->title).Length()-mCurrentCursorPos);
   }
   mLabels[mSelIndex]->title = left + right;
   mCurrentCursorPos = left.Length();
   mInitialCursorPos = mCurrentCursorPos;
   mLabels[mSelIndex]->highlighted = false;
   mDragXPos = -1;
}

void LabelTrack::Unselect()
{
   mSelIndex = -1;
}

bool LabelTrack::IsSelected() const
{
   return (mSelIndex >= 0 && mSelIndex < (int)mLabels.Count());
}

/// Export labels including label start and end-times.
void LabelTrack::Export(wxTextFile & f)
{
   for (int i = 0; i < (int)mLabels.Count(); i++) {
      f.AddLine(wxString::Format(wxT("%f\t%f\t%s"),
                                 (double)mLabels[i]->t,
                                 (double)mLabels[i]->t1,
                                 mLabels[i]->title.c_str()));
   }
}

/// Import labels, handling files with or without end-times.
void LabelTrack::Import(wxTextFile & in)
{
   wxString currentLine;
   int i, i2,len;
   int index, lines;
   wxString s,s1;
   wxString title;
   double t,t1;

   lines = in.GetLineCount();

   mLabels.Clear();
   mLabels.Alloc(lines);

   //Currently, we expect a tag file to have two values and a label
   //on each line. If the second token is not a number, we treat
   //it as a single-value label.
   for (index = 0; index < lines; index++) {
      currentLine = in.GetLine(index);

      len = currentLine.Length();
      if (len == 0)
         return;

      //get the timepoint of the left edge of the label.
      i = 0;
      while (i < len && currentLine.GetChar(i) != wxT(' ')
         && currentLine.GetChar(i) != wxT('\t'))
      {
         i++;
      }
      s = currentLine.Left(i);

      if (!Internat::CompatibleToDouble(s, &t))
         return;

      //Increment one letter.
      i++;

      //Now, go until we find the start of the get the next token
      while (i < len
         && (currentLine.GetChar(i) == wxT(' ')
         || currentLine.GetChar(i) == wxT('\t')))
      {
         i++;
      }
      //Keep track of the start of the second token
      i2=i;

      //Now, go to the end of the second token.
      while (i < len && currentLine.GetChar(i) != wxT(' ')
         && currentLine.GetChar(i) != wxT('\t'))
      {
         i++;
      }

      //We are at the end of the second token.
      s1 = currentLine.Mid(i2,i-i2+1).Strip(wxString::stripType(0x3));
      if (!Internat::CompatibleToDouble(s1, &t1))
      {
         //s1 is not a number.
         t1 = t;  //This is a one-sided label; t1 == t.

         //Because s1 is not a number, the label should be
         //The rest of the line, starting at i2;
         title = currentLine.Right(len - i2).Strip(wxString::stripType(0x3));  //0x3 indicates both
      }
      else
      {
         //s1 is a number, and it is stored correctly in t1.
         //The title should be the remainder of the line,
         //After we eat

         //Get rid of spaces at either end
         title = currentLine.Right(len - i).Strip(wxString::stripType(0x3)); //0x3 indicates both.

      }
      LabelStruct *l = new LabelStruct();
      l->t = t;
      l->t1 = t1;
      l->title = title;
      mLabels.Add(l);
   }
   SortLabels();
}

bool LabelTrack::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   if (!wxStrcmp(tag, wxT("label"))) {

      LabelStruct *l = new LabelStruct();

      // loop through attrs, which is a null-terminated list of
      // attribute-value pairs
      bool has_t1 = false;
      double dblValue;
      while(*attrs) {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;

         if (!value)
            break;

         const wxString strValue = value;
         if (!XMLValueChecker::IsGoodString(strValue))
         {
            delete l;
            return false;
         }

         if (!wxStrcmp(attr, wxT("t")) && Internat::CompatibleToDouble(strValue, &dblValue))
            l->t = dblValue;
         else if (!wxStrcmp(attr, wxT("t1")) && Internat::CompatibleToDouble(strValue, &dblValue))
         {
            has_t1 = true;
            l->t1 = dblValue;
         }
         else if (!wxStrcmp(attr, wxT("title")))
            l->title = strValue;

      } // while

      // Handle files created by Audacity 1.1.   Labels in Audacity 1.1
      // did not have separate start- and end-times.
      if (!has_t1)
         l->t1 = l->t;

      mLabels.Add(l);

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
            mLabels.Clear();
            mLabels.Alloc(nValue);
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
   int len = mLabels.Count();
   int i;

   xmlFile.StartTag(wxT("labeltrack"));
   xmlFile.WriteAttr(wxT("name"), mName);
   xmlFile.WriteAttr(wxT("numlabels"), len);
   xmlFile.WriteAttr(wxT("height"), this->GetActualHeight());
   xmlFile.WriteAttr(wxT("minimized"), this->GetMinimized());
   xmlFile.WriteAttr(wxT("isSelected"), this->GetSelected());

   for (i = 0; i < len; i++) {
      xmlFile.StartTag(wxT("label"));
      xmlFile.WriteAttr(wxT("t"), mLabels[i]->t, 8);
      xmlFile.WriteAttr(wxT("t1"), mLabels[i]->t1, 8);
      xmlFile.WriteAttr(wxT("title"), mLabels[i]->title);
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

   unsigned int i;
   for (i = 0; i < mLabels.Count(); i++)
      delete mLabels[i];
   mLabels.Clear();
   mLabels.Alloc(len);

   for (i = 0; i < len; i++) {
      LabelStruct *l = new LabelStruct();
      if (!Internat::CompatibleToDouble(in->GetNextLine(), &l->t))
         return false;
      // Legacy file format does not include label end-times.
      l->t1 = l->t;
      l->title = in->GetNextLine();
      mLabels.Add(l);
   }

   if (in->GetNextLine() != wxT("MLabelsEnd"))
      return false;
   SortLabels();
   return true;
}

bool LabelTrack::Save(wxTextFile * out, bool overwrite)
{
   out->AddLine(wxT("NumMLabels"));
   int len = mLabels.Count();
   out->AddLine(wxString::Format(wxT("%d"), len));

   for (int i = 0; i < len; i++) {
      out->AddLine(wxString::Format(wxT("%lf"), mLabels[i]->t));
      out->AddLine(mLabels[i]->title);
   }
   out->AddLine(wxT("MLabelsEnd"));

   return true;
}
#endif

bool LabelTrack::Cut(double t0, double t1, Track **dest)
{
   if (!Copy(t0, t1, dest))
      return false;
   if (!Clear(t0, t1))
      return false;

   return true;
}

bool LabelTrack::SplitCut(double t0, double t1, Track ** dest)
{
   // SplitCut() == Copy() + SplitDelete()

   if (!Copy(t0, t1, dest))
      return false;
   if (!SplitDelete(t0, t1))
      return false;

   return true;
}

bool LabelTrack::Copy(double t0, double t1, Track ** dest)
{
   *dest = new LabelTrack(GetDirManager());
   int len = mLabels.Count();

   for (int i = 0; i < len; i++) {
      LabelStruct::TimeRelations relation =
                        mLabels[i]->RegionRelation(t0, t1, this);
      if (relation == LabelStruct::SURROUNDS_LABEL) {
         LabelStruct *l = new LabelStruct();
         l->t = mLabels[i]->t - t0;
         l->t1 = mLabels[i]->t1 - t0;
         l->title = mLabels[i]->title;
         ((LabelTrack *) (*dest))->mLabels.Add(l);
      }
      else if (relation == LabelStruct::WITHIN_LABEL) {
         LabelStruct *l = new LabelStruct();
         l->t = 0;
         l->t1 = t1 - t0;
         l->title = mLabels[i]->title;
         ((LabelTrack *) (*dest))->mLabels.Add(l);
      }
      else if (relation == LabelStruct::BEGINS_IN_LABEL) {
         LabelStruct *l = new LabelStruct();
         l->t = 0;
         l->t1 = mLabels[i]->t1 - t0;
         l->title = mLabels[i]->title;
         ((LabelTrack *) (*dest))->mLabels.Add(l);
      }
      else if (relation == LabelStruct::ENDS_IN_LABEL) {
         LabelStruct *l = new LabelStruct();
         l->t = mLabels[i]->t - t0;
         l->t1 = t1 - t0;
         l->title = mLabels[i]->title;
         ((LabelTrack *) (*dest))->mLabels.Add(l);
      }
   }
   ((LabelTrack *) (*dest))->mClipLen = (t1 - t0);

   return true;
}


bool LabelTrack::PasteOver(double t, Track * src)
{
   if (src->GetKind() != Track::Label)
      return false;

   int len = mLabels.Count();
   int pos = 0;

   while (pos < len && mLabels[pos]->t < t)
      pos++;

   LabelTrack *sl = (LabelTrack *) src;
   for (unsigned int j = 0; j < sl->mLabels.Count(); j++) {
      LabelStruct *l = new LabelStruct();
      l->t = sl->mLabels[j]->t + t;
      l->t1 = sl->mLabels[j]->t1 + t;
      l->title = sl->mLabels[j]->title;
      mLabels.Insert(l, pos++);
      len++;
   }

   return true;
}

bool LabelTrack::Paste(double t, Track *src)
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
   if (n < 0 || t1 < t0) return false;

   double tLen = t1 - t0;

   // Insert space for the repetitions
   ShiftLabelsOnInsert(tLen * n, t1);

   for (unsigned int i = 0; i < mLabels.GetCount(); i++)
   {
      LabelStruct::TimeRelations relation =
                        mLabels[i]->RegionRelation(t0, t1, this);
      if (relation == LabelStruct::SURROUNDS_LABEL)
      {
         // Label is completely inside the selection; duplicate it in each
         // repeat interval
         unsigned int pos = i; // running label insertion position in mLabels

         for (int j = 1; j <= n; j++)
         {
            LabelStruct *l = new LabelStruct();
            l->t = mLabels[i]->t + j * tLen;
            l->t1 = mLabels[i]->t1 + j * tLen;
            l->title = mLabels[i]->title;

            // Figure out where to insert
            while (pos < mLabels.Count() && mLabels[pos]->t < l->t)
               pos++;
            mLabels.Insert(l, pos);
         }
      }
      else if (relation == LabelStruct::BEGINS_IN_LABEL)
      {
         // Label ends inside the selection; ShiftLabelsOnInsert() hasn't touched
         // it, and we need to extend it through to the last repeat interval
         mLabels[i]->t1 += n * tLen;
      }

      // Other cases have already been handled by ShiftLabelsOnInsert()
   }

   return true;
}

bool LabelTrack::Silence(double t0, double t1)
{
   int len = mLabels.Count();

   for (int i = 0; i < len; i++) {
      LabelStruct::TimeRelations relation =
                        mLabels[i]->RegionRelation(t0, t1, this);
      if (relation == LabelStruct::WITHIN_LABEL)
      {
         // Split label around the selection
         LabelStruct *l = new LabelStruct();
         l->t = t1;
         l->t1 = mLabels[i]->t1;
         l->title = mLabels[i]->title;

         mLabels[i]->t1 = t0;

         // This might not be the right place to insert, but we sort at the end
         ++i;
         mLabels.Insert(l, i);
      }
      else if (relation == LabelStruct::ENDS_IN_LABEL)
      {
         // Beginning of label to selection end
         mLabels[i]->t = t1;
      }
      else if (relation == LabelStruct::BEGINS_IN_LABEL)
      {
         // End of label to selection beginning
         mLabels[i]->t1 = t0;
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
   int numLabels = mLabels.Count();

   for (int i = 0; i < numLabels; i++) {
      if (mLabels[i]->t >= t)
         mLabels[i]->t += len;

      if (mLabels[i]->t1 >= t)
         mLabels[i]->t1 += len;
   }

   return true;
}

int LabelTrack::GetNumLabels() const
{
   return mLabels.Count();
}

const LabelStruct *LabelTrack::GetLabel(int index) const
{
   return mLabels[index];
}

int LabelTrack::GetLabelIndex(double t, double t1)
{
   LabelStruct *l;

   int len = mLabels.Count();
   int i;
   //We'd have liked to have times in terms of samples,
   //because then we're doing an intrger comparison.
   //Never mind.  Instead we look for near enough.
   //This level of (in)accuracy is only a problem if we
   //deal with sounds in the MHz range.
   const double delta = 1.0e-7;
   for( i=0;i<len;i++)
   {
      l = mLabels[i];
      if( fabs( l->t - t ) > delta )
         continue;
      if( fabs( l->t1 - t1 ) > delta )
         continue;
      return i;
   }

   return wxNOT_FOUND;
}

int LabelTrack::AddLabel(double t, double t1, const wxString &title)
{
   LabelStruct *l = new LabelStruct();
   l->t = t;
   l->t1 = t1;
   l->title = title;
   mCurrentCursorPos = title.length();
   mInitialCursorPos = mCurrentCursorPos;

   int len = mLabels.Count();
   int pos = 0;

   while (pos < len && mLabels[pos]->t < t)
      pos++;

   mLabels.Insert(l, pos);

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

   return pos;
}

void LabelTrack::DeleteLabel(int index)
{
   wxASSERT((index < (int)mLabels.GetCount()));
   delete mLabels[index];
   mLabels.RemoveAt(index);
   // IF we've deleted the selected label
   // THEN set no label selected.
   if( mSelIndex== index )
   {
      mSelIndex = -1;
      mCurrentCursorPos = 1;
   }
   // IF we removed a label before the selected label
   // THEN the new selected label number is one less.
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
         mBoundaryGlyphs[index].SetMask(new wxMask(mBoundaryGlyphs[index], wxColour(192, 192, 192)));
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
bool LabelTrack::IsGoodLabelFirstKey(int keyCode)
{
   // Allow everything before WXK_START except space, return and delete, the numpad keys
   // when numlock is on, and everything after WXK_COMMAND
   return (keyCode < WXK_START
                  && keyCode != WXK_SPACE && keyCode != WXK_DELETE && keyCode != WXK_RETURN) ||
          (keyCode >= WXK_NUMPAD0 && keyCode <= WXK_DIVIDE) ||
          (keyCode >= WXK_NUMPAD_EQUAL && keyCode <= WXK_NUMPAD_DIVIDE) ||
          (keyCode > WXK_COMMAND);
}

/// This returns true for keys we capture for label editing.
bool LabelTrack::IsGoodLabelEditKey(int keyCode)
{
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
          keyCode > WXK_COMMAND;
}

/// Sorts the labels in order of their starting times.
/// This function is called often (whilst dragging a label)
/// We expect them to be very nearly in order, so insertion
/// sort (with a linear search) is a reasonable choice.
void LabelTrack::SortLabels()
{
   int i,j;
   LabelStruct * pTemp;
   for (i = 1; i < (int)mLabels.Count(); i++)
   {
      j=i-1;
      while( (j>=0) && (mLabels[j]->t > mLabels[i]->t) )
      {
         j--;
      }
      j++;
      if( j<i)
      {
         // Remove at i and insert at j.
         // Don't use DeleteLabel() since just moving it.
         pTemp = mLabels[i];
         mLabels.RemoveAt( i );
         mLabels.Insert(pTemp, j);

         // Various indecese need to be updated with the moved items...
         if( mMouseOverLabelLeft <=i )
         {
            if( mMouseOverLabelLeft == i )
               mMouseOverLabelLeft=j;
            else if( mMouseOverLabelLeft >= j)
               mMouseOverLabelLeft++;
         }
         if( mMouseOverLabelRight <=i )
         {
            if( mMouseOverLabelRight == i )
               mMouseOverLabelRight=j;
            else if( mMouseOverLabelRight >= j)
               mMouseOverLabelRight++;
         }
         if( mSelIndex <=i )
         {
            if( mSelIndex == i )
               mSelIndex=j;
            else if( mSelIndex >= j)
               mSelIndex++;
         }
      }
   }
}

wxString LabelTrack::GetTextOfLabels(double t0, double t1)
{
   bool firstLabel = true;
   wxString retVal;

   for (unsigned int i=0; i < mLabels.GetCount(); ++i)
   {
      if (mLabels[i]->t >= t0 && mLabels[i]->t1 <= t1)
      {
         if (!firstLabel)
            retVal += '\t';
         firstLabel = false;
         retVal += mLabels[i]->title;
      }
   }

   return retVal;
}
