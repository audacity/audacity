/**********************************************************************

Audacity: A Digital Audio Editor

LabelTrackView.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/


#include "LabelTrackView.h"

#include "LabelTrackVRulerControls.h"
#include "LabelGlyphHandle.h"
#include "LabelTextHandle.h"

#include "../../ui/TextEditHelper.h"

#include "LabelLayout.h"

#include "AColor.h"
#include "../../../widgets/BasicMenu.h"
#include "AllThemeResources.h"
#include "../../../HitTestResult.h"
#include "Project.h"
#include "ProjectHistory.h"
#include "ProjectNumericFormats.h"
#include "ProjectRate.h"
#include "../../../ProjectWindow.h"
#include "../../../ProjectWindows.h"
#include "../../../RefreshCode.h"
#include "SyncLock.h"
#include "Theme.h"
#include "../../../TrackArt.h"
#include "../../../TrackArtist.h"
#include "../../../TrackPanelAx.h"
#include "../../../TrackPanel.h"
#include "../../../TrackPanelMouseEvent.h"
#include "UndoManager.h"
#include "ViewInfo.h"
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

LabelTrackView::Index &LabelTrackView::Index::operator =(int index)
{
   if (index != mIndex) {
      mModified = false;
   }
   mIndex = index;
   return *this;
}

LabelTrackView::Index &LabelTrackView::Index::operator ++()
{
   mModified = false;
   mIndex += 1;
   return *this;
}

LabelTrackView::Index &LabelTrackView::Index::operator --()
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

namespace {
   struct FontChangeNotifier : Observer::Publisher<>{
      using Observer::Publisher<>::Publish;
   } sFontChangeNotifier;
}

LabelTrackView::LabelTrackView( const std::shared_ptr<Track> &pTrack )
   : CommonChannelView{ pTrack, 0 }
{
   ResetFont();
   UpdateAllWidths();
   mFontChangeSubscription = sFontChangeNotifier.Subscribe([this](auto){
      UpdateAllWidths();
   });

   CreateCustomGlyphs();
   ResetFlags();

   // Events will be emitted by the track
   const auto pLabelTrack = FindLabelTrack();
   BindTo( pLabelTrack.get() );
}

LabelTrackView::~LabelTrackView()
{
}

void LabelTrackView::Reparent( const std::shared_ptr<Track> &parent )
{
   auto oldParent = FindLabelTrack();
   auto newParent = track_cast<LabelTrack*>(parent.get());
   if (oldParent.get() != newParent)
      BindTo( newParent );
   CommonChannelView::Reparent(parent);
}

void LabelTrackView::BindTo( LabelTrack *pParent )
{
   // Destroys any previous subscription to another track
   mTrackChangeSubscription =
   pParent->Subscribe([this](const LabelTrackEvent &e){
      switch (e.type) {
      case LabelTrackEvent::Addition:
         return OnLabelAdded(e);
      case LabelTrackEvent::Deletion:
         return OnLabelDeleted(e);
      case LabelTrackEvent::Permutation:
         return OnLabelPermuted(e);
      case LabelTrackEvent::Selection:
         return OnSelectionChange(e);
      case LabelTrackEvent::TitleChange:
         return OnTitleChange(e);
      default:
         return;
      }
   });
}

void LabelTrackView::CopyTo(Track &track) const
{
   ChannelView::CopyTo(track);
   auto &other = ChannelView::Get(*track.GetChannel(0));
   if (const auto pOther = dynamic_cast<const LabelTrackView*>(&other)) {
      pOther->mNavigationIndex = mNavigationIndex;
      pOther->mTextEditIndex = mTextEditIndex;
      pOther->mUndoLabel = mUndoLabel;
   }
}

std::shared_ptr<TextEditHelper> LabelTrackView::MakeTextEditHelper(const LabelStruct& label)
{
   auto lock = std::static_pointer_cast<LabelTrackView>(shared_from_this());
   auto textEdit = std::make_unique<TextEditHelper>(lock, label.title, msFont);
   textEdit->SetTextColor(theTheme.Colour(clrLabelTrackText));
   textEdit->SetTextSelectionColor(theTheme.Colour(clrLabelTrackTextSelection));
   return std::move(textEdit);
}

wxRect LabelTrackView::MakeTextEditHelperRect(const LabelStruct& label)
{
   const auto &layout = LabelLayout::Get(label);
   const int yFrameHeight = mTextHeight + TextFramePadding * 2;
   return {
      layout.xText,
      layout.y + TextFrameYOffset - (LabelBarHeight + yFrameHeight) / 2,
      layout.width + 1,
      yFrameHeight };
}

LabelTrackView &LabelTrackView::Get(LabelTrack &track)
{
   return static_cast<LabelTrackView&>(ChannelView::Get(track));
}

const LabelTrackView &LabelTrackView::Get(const LabelTrack &track)
{
   return static_cast<const LabelTrackView&>(ChannelView::Get(track));
}

std::shared_ptr<LabelTrack> LabelTrackView::FindLabelTrack()
{
   return std::static_pointer_cast<LabelTrack>( FindTrack() );
}

std::shared_ptr<const LabelTrack> LabelTrackView::FindLabelTrack() const
{
   return const_cast<LabelTrackView*>(this)->FindLabelTrack();
}

std::vector<UIHandlePtr> LabelTrackView::DetailedHitTest
(const TrackPanelMouseState &st,
 const AudacityProject *WXUNUSED(pProject), int, bool)
{
   UIHandlePtr result;
   std::vector<UIHandlePtr> results;
   const wxMouseState &state = st.state;

   const auto pTrack = FindLabelTrack();
   result = LabelGlyphHandle::HitTest(
      mGlyphHandle, state, pTrack, st.rect);
   if (result)
      results.push_back(result);

   if(!state.ControlDown())
   {
      auto index = LabelTrackView::OverATextBox(*pTrack.get(), state.GetX(), state.GetY());
      if (index != -1)
      {
         results.push_back(
            AssignUIHandlePtr(mTextHandle,
               std::make_shared<LabelTextHandle>(pTrack, index)));
      }
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
   mTextEditHelper.reset();
   mTextEditIndex = -1;
   mNavigationIndex = -1;
}

LabelTrackView::Flags LabelTrackView::SaveFlags() const
{
   auto selection 
      = mTextEditHelper ? mTextEditHelper->GetSelection() : std::pair<int, int>();
   return {
      selection.first, selection.second, mNavigationIndex,
      mTextEditIndex, mUndoLabel
   };
}

void LabelTrackView::RestoreFlags( const Flags& flags )
{
   mNavigationIndex = flags.mNavigationIndex;
   SetTextSelection(flags.mTextEditIndex, flags.mInitialCursorPos, flags.mCurrentCursorPos);
}

wxFont LabelTrackView::GetFont(const wxString &faceName, int size)
{
   wxFontEncoding encoding;
   if (faceName.empty())
      encoding = wxFONTENCODING_DEFAULT;
   else
      encoding = wxFONTENCODING_SYSTEM;
   
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
   auto font = GetFont(facename, size);
   if (msFont != font) {
      msFont = font;
      sFontChangeNotifier.Publish({});
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
void LabelTrackView::ComputeTextPosition(const wxRect & r, int index) const
{
   const auto pTrack = FindLabelTrack();
   const auto &mLabels = pTrack->GetLabels();

   const auto &labelStruct = mLabels[index];
   const auto &layout = LabelLayout::Get(labelStruct);

   // xExtra is extra space
   // between the text and the endpoints.
   const int xExtra=mIconWidth;
   int x     = layout.x;  // left endpoint
   int x1    = layout.x1; // right endpoint.
   int width = layout.width;

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

   // The only change in layout done by this function
   LabelLayout::Get(labelStruct).xText = xText;
}

/// ComputeLayout determines which row each label
/// should be placed on, and reserves space for it.
/// Function assumes that the labels are sorted.
void LabelTrackView::ComputeLayout(const wxRect & r, const ZoomInfo &zoomInfo) const
{
   int xUsed[MAX_NUM_ROWS];

   int iRow;
   // Rows are the 'same' height as icons or as the text,
   // whichever is taller.
   const int yRowHeight = std::max(mTextHeight, mIconHeight) + 3;// pixels.
   // Extra space at end of rows.
   // We allow space for one half icon at the start and two
   // half icon widths for extra x for the text frame.
   // [we don't allow half a width space for the end icon since it is
   // allowed to be obscured by the text].
   const int xExtra= (3 * mIconWidth)/2;

   bool bAvoidName = false;
   const int nRows = wxMin((r.height / yRowHeight) + 1, MAX_NUM_ROWS);
   if( nRows > 2 )
      bAvoidName = gPrefs->ReadBool(wxT("/GUI/ShowTrackNameInWaveform"), false);
   // Initially none of the rows have been used.
   // So set a value that is less than any valid value.
   {
      // Bug 502: With dragging left of zeros, labels can be in 
      // negative space.  So set least possible value as starting point.
      const int xStart = INT_MIN;
      for (auto &x : xUsed)
         x = xStart;
   }
   int nRowsUsed=0;

   const auto pTrack = FindLabelTrack();
   const auto &mLabels = pTrack->GetLabels();

   { int i = -1; for (const auto &labelStruct : mLabels) { ++i;
      auto &layout = LabelLayout::Get(labelStruct);
   
      const auto t0 = labelStruct.getT0();
      const auto t1 = labelStruct.getT1();
      const int x = zoomInfo.TimeToPosition(t0, r.x);
      const int x1 = zoomInfo.TimeToPosition(t1, r.x);
      int y = r.y;

      layout.x = x;
      layout.x1 = x1;

      // Bug 2388 - Point label and range label can appear identical
      // If the start and end times are not actually the same, but they
      // would appear so when drawn as lines at current zoom, be sure to draw
      // two lines - i.e. displace the second line slightly.
      if (x == x1 && t0 != t1)
         ++layout.x1;

      layout.y = -1;// -ve indicates nothing doing.
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
         // Logic to ameliorate case where first label is under the 
         // (on track) track name.  For later labels it does not matter
         // as we can scroll left or right and/or zoom.
         // A possible alternative idea would be to (instead) increase the 
         // translucency of the track name, when the mouse is inside it.
         if( (i==0 ) && (iRow==0) && bAvoidName ){
            // reserve some space in first row.
            // reserve max of 200px or t1, or text box right edge.
            const int x2 = zoomInfo.TimeToPosition(0.0, r.x) + 200;
            xUsed[iRow] = x + layout.width + xExtra;
            if( xUsed[iRow] < x1 ) xUsed[iRow]=x1;
            if( xUsed[iRow] < x2 ) xUsed[iRow]=x2;
            iRow=1;
         }

         // Possibly update the number of rows actually used.
         if( iRow >= nRowsUsed )
            nRowsUsed=iRow+1;
         // Record the position for this label
         y= r.y + iRow * yRowHeight +(yRowHeight/2)+1;
         layout.y = y;
         // On this row we have used up to max of end marker and width.
         // Plus also allow space to show the start icon and
         // some space for the text frame.
         xUsed[iRow] = x + layout.width + xExtra;
         if( xUsed[iRow] < x1 ) xUsed[iRow]=x1;
         ComputeTextPosition( r, i );
      }
   }}
}

/// Draw vertical lines that go exactly through the position
/// of the start or end of a label.
///   @param  dc the device context
///   @param  r  the LabelTrack rectangle.
void LabelTrackView::DrawLines(
   wxDC & dc, const LabelStruct &ls, const wxRect & r)
{
   const auto &layout = LabelLayout::Get(ls);
   auto &x = layout.x;
   auto &x1 = layout.x1;
   auto &y = layout.y;

   // How far out from the centre line should the vertical lines
   // start, i.e. what is the y position of the icon?
   // We adjust this so that the line encroaches on the icon
   // slightly (there is white space in the design).
   const int yIconStart = y - (mIconHeight /2)+1+(mTextHeight+3)/2;
   const int yIconEnd   = yIconStart + mIconHeight-2;

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
void LabelTrackView::DrawGlyphs(
   wxDC & dc, const LabelStruct &ls, const wxRect & r,
   int GlyphLeft, int GlyphRight)
{
   const auto &layout = LabelLayout::Get(ls);
   auto &y = layout.y;

   const int xHalfWidth=mIconWidth/2;
   const int yStart=y-mIconHeight/2+(mTextHeight+3)/2;

   // If y == -1, nothing to draw
   if( y == -1 )
      return;

   auto &x = layout.x;
   auto &x1 = layout.x1;

   if((x  >= r.x) && (x  <= (r.x+r.width)))
      dc.DrawBitmap(GetGlyph(GlyphLeft), x-xHalfWidth,yStart, true);
   // The extra test commented out here would suppress right hand markers
   // when they overlap the left hand marker (e.g. zoomed out) or to the left.
   if((x1 >= r.x) && (x1 <= (r.x+r.width)) /*&& (x1>x+mIconWidth)*/)
      dc.DrawBitmap(GetGlyph(GlyphRight), x1-xHalfWidth,yStart, true);
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
void LabelTrackView::DrawText(wxDC & dc, const LabelStruct &ls, const wxRect & r)
{
   const int yFrameHeight = mTextHeight + TextFramePadding * 2;
   //If y is positive then it is the center line for the
   //text we are about to draw.
   //if it isn't, nothing to draw.

   const auto &layout = LabelLayout::Get(ls);
   auto &y = layout.y;
   if( y == -1 )
      return;

   // Draw frame for the text...
   // We draw it half an icon width left of the text itself.
   {
      auto &xText = layout.xText;
      const int xStart = std::max(r.x, xText - mIconWidth / 2);
      const int xEnd = std::min(r.x + r.width,
         xText + layout.width + mIconWidth / 2);
      const int xWidth = xEnd-xStart;

      if( (xStart < (r.x+r.width)) && (xEnd > r.x) && (xWidth>0))
      {
         // Now draw the text itself.
         auto pos = y - LabelBarHeight - yFrameHeight + TextFrameYOffset +
            (yFrameHeight - mFontHeight) / 2 + dc.GetFontMetrics().ascent;
         dc.DrawText(ls.title, xText, pos);
      }
   }

}

void LabelTrackView::DrawTextBox(
   wxDC & dc, const LabelStruct &ls, const wxRect & r)
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
    const auto &layout = LabelLayout::Get(ls);
    auto& xText = layout.xText;
    const int xStart = std::max(r.x, xText - mIconWidth / 2);
    const int xEnd = std::min(r.x + r.width,
      xText + layout.width + mIconWidth / 2);
    const int xWidth = xEnd - xStart;

    if ((xStart < (r.x + r.width)) && (xEnd > r.x) && (xWidth > 0))
    {
       wxRect frame(
          xStart,
          layout.y - (textFrameHeight + LabelBarHeight) / 2 + TextFrameYOffset,
          xWidth, textFrameHeight);
       dc.DrawRectangle(frame);
    }
}

void LabelTrackView::DrawBar(wxDC& dc, const LabelStruct& ls, const wxRect& r)
{
   //If y is positive then it is the center line for the
   //text we are about to draw.
   const int xBarShorten = mIconWidth + 4;
   const auto &layout = LabelLayout::Get(ls);
   auto& y = layout.y;
   if (y == -1)
     return;

   auto& x = layout.x;
   auto& x1 = layout.x1;
   const int xStart = std::max(r.x, x + xBarShorten / 2);
   const int xEnd = wxMin(r.x + r.width, x1 - xBarShorten / 2);
   const int xWidth = xEnd - xStart;

   if ((xStart < (r.x + r.width)) && (xEnd > r.x) && (xWidth > 0))
   {
      wxRect bar(xStart, y - (LabelBarHeight - GetTextFrameHeight()) / 2,
         xWidth, LabelBarHeight);
      if (x1 > x + xBarShorten)
         dc.DrawRectangle(bar);
   }
}

#include "LabelGlyphHandle.h"
namespace {
   LabelTrackHit *findHit( TrackPanel *pPanel )
   {
      if (! pPanel )
         return nullptr;

      // Fetch the highlighting state
      auto target = pPanel->Target();
      if (target) {
         auto handle = dynamic_cast<LabelGlyphHandle*>( target.get() );
         if (handle)
            return &*handle->mpHit;
      }
      return nullptr;
   }
}

#include "../../../TrackPanelDrawingContext.h"
#include "LabelTextHandle.h"

/// Draw calls other functions to draw the LabelTrack.
///   @param  dc the device context
///   @param  r  the LabelTrack rectangle.
void LabelTrackView::Draw(TrackPanelDrawingContext &context, const wxRect & r)
   const
{
   auto &dc = context.dc;
   const auto artist = TrackArtist::Get( context );
   const auto &zoomInfo = *artist->pZoomInfo;

   auto pHit = findHit( artist->parent );

   if (msFont.Ok())
      dc.SetFont(msFont);

   if (mFontHeight == -1)
      calculateFontHeight(dc);

   const auto pTrack = std::static_pointer_cast< const LabelTrack >(
      FindTrack()->SubstitutePendingChangedTrack());
   const auto &mLabels = pTrack->GetLabels();

   TrackArt::DrawBackgroundWithSelection( context, r, pTrack.get(),
      AColor::labelSelectedBrush, AColor::labelUnselectedBrush,
      SyncLock::IsSelectedOrSyncLockSelected(pTrack.get()) );

   // TODO: And this only needs to be done once, but we
   // do need the dc to do it.
   // We need to set mTextHeight to something sensible,
   // guarding against the case where there are no
   // labels or all are empty strings, which for example
   // happens with a NEW label track.
   mTextHeight = dc.GetFontMetrics().ascent + dc.GetFontMetrics().descent;
   const int yFrameHeight = mTextHeight + TextFramePadding * 2;

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
   for (const auto &labelStruct : mLabels)
      DrawLines( dc, labelStruct, r );

   // Draw the end glyphs.
   { int i = -1; for (const auto &labelStruct : mLabels) { ++i;
      GlyphLeft=0;
      GlyphRight=1;
      if( pHit && i == pHit->mMouseOverLabelLeft )
         GlyphLeft = (pHit->mEdge & 4) ? 6:9;
      if( pHit && i == pHit->mMouseOverLabelRight )
         GlyphRight = (pHit->mEdge & 4) ? 7:4;
      DrawGlyphs( dc, labelStruct, r, GlyphLeft, GlyphRight );
   }}

   auto &project = *artist->parent->GetProject();

   // Draw the label boxes.
   {
#ifdef EXPERIMENTAL_TRACK_PANEL_HIGHLIGHTING
      bool highlightTrack = false;
      auto target = dynamic_cast<LabelTextHandle*>(context.target.get());
      highlightTrack = target && target->GetTrack().get() == this;
#endif
      int i = -1; for (const auto &labelStruct : mLabels) { ++i;
         bool highlight = false;
#ifdef EXPERIMENTAL_TRACK_PANEL_HIGHLIGHTING
         highlight = highlightTrack && target->GetLabelNum() == i;
#endif
         
         dc.SetBrush(mNavigationIndex == i || (pHit && pHit->mMouseOverLabel == i) 
            ? AColor::labelTextEditBrush : AColor::labelTextNormalBrush);
         DrawBar(dc, labelStruct, r);

         bool selected = mTextEditIndex == i;

         if (selected)
            dc.SetBrush(AColor::labelTextEditBrush);
         else if (highlight)
            dc.SetBrush(AColor::uglyBrush);
         else
            dc.SetBrush(AColor::labelTextNormalBrush);
         DrawTextBox(dc, labelStruct, r);

         dc.SetBrush(AColor::labelTextNormalBrush);
      }
   }

   // Draw the text and the label boxes.
   int i = 0;
   for (auto label : mLabels)
   {
      if ((i == mTextEditIndex) && mTextEditHelper)
      {
         mTextEditHelper->Draw(dc, LabelTrackView::MakeTextEditHelperRect(label));
      }
      else
      {
         dc.SetBrush(AColor::labelTextNormalBrush);
         DrawText(dc, label, r);
      }
      ++i;
   }
}

void LabelTrackView::Draw(
   TrackPanelDrawingContext &context,
   const wxRect &rect, unsigned iPass )
{
   if ( iPass == TrackArtist::PassTracks )
      Draw( context, rect );
   CommonChannelView::Draw(context, rect, iPass);
}

int LabelTrackView::GetTextEditIndex(AudacityProject& project) const
{
    if (IsValidIndex(mTextEditIndex, project))
        return mTextEditIndex;
    return -1;
}
void LabelTrackView::ResetTextSelection()
{
    mTextEditIndex = -1;
    mTextEditHelper.reset();
}
void LabelTrackView::SetNavigationIndex(int index)
{
    mNavigationIndex = index;
}
int LabelTrackView::GetNavigationIndex(AudacityProject& project) const
{
    if (IsValidIndex(mNavigationIndex, project))
        return mNavigationIndex;
    return -1;
}

void LabelTrackView::calculateFontHeight(wxDC & dc)
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

bool LabelTrackView::IsValidIndex(const Index& index, AudacityProject& project) const
{
    if (index == -1)
       return false;
    // may make delayed update of mutable mSelIndex after track selection change
    auto track = FindLabelTrack();
    if (track->GetSelected() || (TrackFocus::Get(project).Get() == track.get()))
       return index >= 0 && index < static_cast<int>(track->GetLabels().size());
    return false;
}

/// Cut the selected text in the text box
///  @return true if text is selected in text box, false otherwise
bool LabelTrackView::CutSelectedText( AudacityProject &project )
{
   if (!mTextEditHelper)
      return false;
   return mTextEditHelper->CutSelectedText(project);
}

/// Copy the selected text in the text box
///  @return true if text is selected in text box, false otherwise
bool LabelTrackView::CopySelectedText( AudacityProject &project )
{
   if (!mTextEditHelper)
      return false;
   return mTextEditHelper->CopySelectedText(project);
}

// PRL:  should this set other fields of the label selection?
/// Paste the text on the clipboard to text box
///  @return true if mouse is clicked in text box, false otherwise
bool LabelTrackView::PasteSelectedText(
   AudacityProject &project, double sel0, double sel1 )
{
   if (!mTextEditHelper)
      StartLabelTextEdit(AddLabel(SelectedRegion(sel0, sel1)), &project);
   if (mTextEditHelper->PasteSelectedText(project))
   {
      // Make sure caret is in view
      int x{ 0 };
      if (mTextEditHelper->GetCharPositionX(mTextEditHelper->GetSelection().second, &x))
         ProjectWindow::Get(project).ScrollIntoView(x);
      return true;
   }
   return false;
}

bool LabelTrackView::SelectAllText(AudacityProject& project)
{
   if(mTextEditHelper)
   {
      mTextEditHelper->SelectAll();
      return true;
   }
   return false;
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
   const LabelTrack &track, LabelTrackHit &hit, int x, int y)
{
   //Determine the NEW selection.
   int result=0;
   const int d1=10; //distance in pixels, used for have we hit drag handle.
   const int d2=5;  //distance in pixels, used for have we hit drag handle center.

   //If not over a label, reset it
   hit.mMouseOverLabelLeft  = -1;
   hit.mMouseOverLabelRight = -1;
   hit.mMouseOverLabel = -1;
   hit.mEdge = 0;

   const auto pTrack = &track;
   const auto &mLabels = pTrack->GetLabels();
   { int i = -1; for (const auto &labelStruct : mLabels) { ++i;
      const auto &layout = LabelLayout::Get(labelStruct);
      // give text box better priority for selecting
      // reset selection state
      if (OverTextBox(&labelStruct, x, y))
      {
         result = 0;
         hit.mMouseOverLabel = -1;
         hit.mMouseOverLabelLeft = -1;
         hit.mMouseOverLabelRight = -1;
         break;
      }
   
      //over left or right selection bound
      //Check right bound first, since it is drawn after left bound,
      //so give it precedence for matching/highlighting.
      if( abs(layout.y - (y - (mTextHeight+3)/2)) < d1 &&
               abs(layout.x1 - d2 -x) < d1)
      {
         hit.mMouseOverLabelRight = i;
         if(abs(layout.x1 - x) < d2 )
         {
            result |= 4;
            // If left and right co-incident at this resolution, then we drag both.
            // We were more stringent about co-incidence here in the past.
            if( abs(layout.x1 - layout.x) < 5.0 )
            {
               result |=1;
               hit.mMouseOverLabelLeft = i;
            }
         }
         result |= 2;
      }
      // Use else-if here rather than else to avoid detecting left and right
      // of the same label.
      else if(   abs(layout.y - (y - (mTextHeight+3)/2)) < d1 &&
            abs(layout.x + d2 - x) < d1 )
      {
         hit.mMouseOverLabelLeft = i;
         if(abs(layout.x - x) < d2 )
            result |= 4;
         result |= 1;
      }
      else if (x >= layout.x && x <= layout.x1 &&
         abs(y - (layout.y + mTextHeight / 2)) < d1)
      {
         hit.mMouseOverLabel = i;
         result = 3;
      }
   }}
   hit.mEdge = result;
}

int LabelTrackView::OverATextBox( const LabelTrack &track, int xx, int yy )
{
   const auto pTrack = &track;
   const auto &mLabels = pTrack->GetLabels();
   for (int nn = (int)mLabels.size(); nn--;) {
      const auto &labelStruct = mLabels[nn];
      if ( OverTextBox( &labelStruct, xx, yy ) )
         return nn;
   }

   return -1;
}

// return true if the mouse is over text box, false otherwise
bool LabelTrackView::OverTextBox(const LabelStruct *pLabel, int x, int y)
{
   const auto &layout = LabelLayout::Get(*pLabel);
   if ( (layout.xText - (mIconWidth / 2) < x) &&
            (x < layout.xText + layout.width + (mIconWidth / 2)) &&
            (abs(layout.y - y) < mIconHeight / 2))
   {
      return true;
   }
   return false;
}

/// Returns true for keys we capture to start a label.
static bool IsGoodLabelFirstKey(const wxKeyEvent & evt)
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

// Check for keys that we will process
bool LabelTrackView::DoCaptureKey(
   AudacityProject &project, wxKeyEvent & event )
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
   if ((code == WXK_TAB || code == WXK_NUMPAD_TAB) &&
       !mLabels.empty())
      return true;

   if (IsValidIndex(mTextEditIndex, project)) {
      if (TextEditHelper::IsGoodEditKeyCode(code)) {
         return true;
      }
   }
   else {
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
#endif

         // If there's a label there already don't capture
         auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
         if( GetLabelIndex(selectedRegion.t0(),
                           selectedRegion.t1()) != wxNOT_FOUND ) {
            return false;
         }

         return true;
      }
   }

   return false;
}

unsigned LabelTrackView::CaptureKey(
   wxKeyEvent & event, ViewInfo &, wxWindow *, AudacityProject *project )
{
   event.Skip(!DoCaptureKey( *project, event ));
   return RefreshCode::RefreshNone;
}

unsigned LabelTrackView::KeyDown(
   wxKeyEvent & event, ViewInfo &viewInfo, wxWindow *WXUNUSED(pParent),
   AudacityProject *project)
{
   double bkpSel0 = viewInfo.selectedRegion.t0(),
      bkpSel1 = viewInfo.selectedRegion.t1();

   // Pass keystroke to labeltrack's handler and add to history if any
   // updates were done
   DoKeyDown(*project, viewInfo.selectedRegion, event);

   // If selection modified, refresh
   // Otherwise, refresh track display if the keystroke was handled
   if (bkpSel0 != viewInfo.selectedRegion.t0() ||
      bkpSel1 != viewInfo.selectedRegion.t1())
      return RefreshCode::RefreshAll;
   else if (!event.GetSkipped())
      return  RefreshCode::RefreshCell;

   return RefreshCode::RefreshNone;
}

unsigned LabelTrackView::Char(
   wxKeyEvent & event, ViewInfo &viewInfo, wxWindow *, AudacityProject *project)
{
   double bkpSel0 = viewInfo.selectedRegion.t0(),
      bkpSel1 = viewInfo.selectedRegion.t1();

   DoChar( *project, viewInfo.selectedRegion, event );

   // If selection modified, refresh
   // Otherwise, refresh track display if the keystroke was handled
   if (bkpSel0 != viewInfo.selectedRegion.t0() ||
      bkpSel1 != viewInfo.selectedRegion.t1())
      return RefreshCode::RefreshAll;
   else if (!event.GetSkipped())
      return RefreshCode::RefreshCell;

   return RefreshCode::RefreshNone;
}

/// KeyEvent is called for every keypress when over the label track.
void LabelTrackView::DoKeyDown(
   AudacityProject &project, NotifyingSelectedRegion &newSel, wxKeyEvent & event)
{
   // Cache the keycode
   int keyCode = event.GetKeyCode();
   const int mods = event.GetModifiers();

   // Check for modifiers and only allow shift
   // except in the case of Ctrl + F2, so hardcoded Ctrl+F2 can
   // be used for renaming a label
   if ((keyCode != WXK_F2 && mods != wxMOD_NONE && mods != wxMOD_SHIFT)
      || (keyCode == WXK_F2 && mods != wxMOD_CONTROL)) {
      event.Skip();
      return;
   }

   // All editing keys are only active if we're currently editing a label
   const auto pTrack = FindLabelTrack();
   const auto &mLabels = pTrack->GetLabels();
   if (IsValidIndex(mTextEditIndex, project)) {
      // Do label text changes
       if (auto editHelper = mTextEditHelper)
       {
           // Make sure caret is in view
           int x{ 0 };
           if (editHelper->GetCharPositionX(editHelper->GetSelection().second, &x))
               ProjectWindow::Get(project).ScrollIntoView(x);

           if (!editHelper->OnKeyDown(keyCode, mods, &project))
           {
               switch (keyCode)
               {
               case WXK_BACK:
               case WXK_DELETE:
               case WXK_NUMPAD_DELETE:
                   pTrack->DeleteLabel(mTextEditIndex);
                   ResetTextSelection();
                   break;
               case '\x10':   // OSX
               case WXK_MENU:
               case WXK_WINDOWS_MENU:
               {
                   int x{ 0 };
                   if (editHelper->GetCharPositionX(editHelper->GetSelection().second, &x))
                       ShowContextMenu(project, { x, editHelper->GetBBox().GetBottom() });
               } break;
               default:
                   if (!TextEditHelper::IsGoodEditKeyCode(keyCode))
                       event.Skip();
               }
           }
       }
   }
   else
   {
      // Do navigation
      switch (keyCode) {

      case WXK_ESCAPE:
          mNavigationIndex = -1;
          break;
      case WXK_TAB:
      case WXK_NUMPAD_TAB:
         if (!mLabels.empty()) {
            int len = (int) mLabels.size();
            // The case where the start of selection is the same as the
            // start of a label is handled separately so that if some labels
            // have the same start time, all labels are navigated.
            if (IsValidIndex(mNavigationIndex, project)
               && mLabels[mNavigationIndex].getT0() == newSel.t0())
            {
                if (event.ShiftDown()) {
                    --mNavigationIndex;
                }
                else {
                    ++mNavigationIndex;
                }
                mNavigationIndex = (mNavigationIndex + (int)mLabels.size()) % (int)mLabels.size();    // wrap round if necessary
            }
            else
            {
                if (event.ShiftDown()) {
                    //search for the first label starting from the end (and before selection)
                    mNavigationIndex = len - 1;
                    if (newSel.t0() > mLabels[0].getT0()) {
                        while (mNavigationIndex >= 0 &&
                            mLabels[mNavigationIndex].getT0() > newSel.t0()) {
                            --mNavigationIndex;
                        }
                    }
                }
                else {
                    //search for the first label starting from the beginning (and after selection)
                    mNavigationIndex = 0;
                    if (newSel.t0() < mLabels[len - 1].getT0()) {
                        while (mNavigationIndex < len &&
                            mLabels[mNavigationIndex].getT0() < newSel.t0()) {
                            ++mNavigationIndex;
                        }
                    }
                }
            }

            if (mNavigationIndex >= 0 && mNavigationIndex < len) {
               const auto &labelStruct = mLabels[mNavigationIndex];
               //Set the selection region to be equal to the selection bounds of the tabbed-to label.
               newSel = labelStruct.selectedRegion;
               ProjectWindow::Get(project).ScrollIntoView(labelStruct.selectedRegion.t0());
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
            }
            else {
               mNavigationIndex = -1;
            }
         }
         break;
      case WXK_F2:     // Must be Ctrl + F2 to have reached here
         // Hardcoded Ctrl+F2 activates editing of the label
         // pointed to by mNavigationIndex (if valid)
         if (IsValidIndex(mNavigationIndex, project)) {
             StartLabelTextEdit(mNavigationIndex, &project);
         }
         break;
      default:
         if (!IsGoodLabelFirstKey(event)) {
            event.Skip();
         }
         break;
      }
   }
}

/// OnChar is called for incoming characters -- that's any keypress not handled
/// by OnKeyDown.
void LabelTrackView::DoChar(
   AudacityProject &project, NotifyingSelectedRegion &WXUNUSED(newSel),
   wxKeyEvent & event)
{
   // Check for modifiers and only allow shift.
   //
   // We still need to check this or we will eat the top level menu accelerators
   // on Windows if our capture or key down handlers skipped the event.
   const int mods = event.GetModifiers();
   if (mods != wxMOD_NONE && mods != wxMOD_SHIFT) {
      event.Skip();
      return;
   }

   // Only track true changes to the label
   //bool updated = false;

   // Cache the character
   wxChar charCode = event.GetUnicodeKey();

   // Skip if it's not a valid unicode character or a control character
   if (charCode == 0 || wxIscntrl(charCode)) {
      event.Skip();
      return;
   }
   
   // If we've reached this point and aren't currently editing, add NEW label
   const auto pTrack = FindLabelTrack();
   if (!IsValidIndex(mTextEditIndex, project)) {
      // Don't create a NEW label for a space
      if (wxIsspace(charCode)) {
         event.Skip();
         return;
      }
      bool useDialog;
      gPrefs->Read(wxT("/GUI/DialogForNameNewLabel"), &useDialog, false);
      auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
      if (useDialog) {
         wxString title;
         if (DialogForLabelName(
            project, selectedRegion, charCode, title) ==
             wxID_CANCEL) {
            return;
         }
         pTrack->SetSelected(true);
         pTrack->AddLabel(selectedRegion, title);
         ProjectHistory::Get( project )
            .PushState(XO("Added label"), XO("Label"));
         return;
      }
      else {
         pTrack->SetSelected(true);
         AddLabel( selectedRegion );
         ProjectHistory::Get( project )
            .PushState(XO("Added label"), XO("Label"));
      }
   }

   if (!IsValidIndex(mTextEditIndex, project) || !mTextEditHelper)
      return;

   mTextEditHelper->OnChar(charCode, &project);
}

enum
{
   OnCutSelectedTextID = 1,      // OSX doesn't like a 0 menu id
   OnCopySelectedTextID,
   OnPasteSelectedTextID,
   OnDeleteSelectedLabelID,
   OnEditSelectedLabelID,
};

void LabelTrackView::ShowContextMenu( AudacityProject &project, const wxPoint& position )
{
   wxWindow *parent = wxWindow::FindFocus();

   // Bug 2044.  parent can be nullptr after a context switch.
   if( !parent )
      parent = &GetProjectFrame( project );

   if( parent )
   {
      wxMenu menu;
      menu.Bind(wxEVT_MENU,
         [this, &project]( wxCommandEvent &event ){
            OnContextMenu( project, event ); }
      );

      menu.Append(OnCutSelectedTextID, _("Cu&t Label text"));
      menu.Append(OnCopySelectedTextID, _("&Copy Label text"));
      menu.Append(OnPasteSelectedTextID, _("&Paste"));
      menu.Append(OnDeleteSelectedLabelID, _("&Delete Label"));
      menu.Append(OnEditSelectedLabelID, _("&Edit Label..."));

      menu.Enable(OnCutSelectedTextID, mTextEditHelper && !mTextEditHelper->IsSelectionEmpty());
      menu.Enable(OnCopySelectedTextID, mTextEditHelper && !mTextEditHelper->IsSelectionEmpty());
      menu.Enable(OnPasteSelectedTextID, IsTextClipSupported());
      menu.Enable(OnDeleteSelectedLabelID, true);
      menu.Enable(OnEditSelectedLabelID, true);

      if(!IsValidIndex(mTextEditIndex, project)) {
         return;
      }

      const auto pTrack = FindLabelTrack();
      const LabelStruct *ls = pTrack->GetLabel(mTextEditIndex);

      wxClientDC dc(parent);

      if (msFont.Ok())
         dc.SetFont(msFont);

      //int x = 0;
      //bool success = CalcCursorX( project, &x );
      //wxASSERT(success);
      //static_cast<void>(success); // Suppress unused variable warning if debug mode is disabled

      // Bug #2571: Hackage alert! For some reason wxGTK does not like
      // displaying the LabelDialog from within the PopupMenu "context".
      // So, workaround it by editing the label AFTER the popup menu is
      // closed. It's really ugly, but it works.  :-(
      mEditIndex = -1;
      parent->PopupMenu(&menu, position.x, position.y);
      if (mEditIndex >= 0)
      {
         DoEditLabels( project, FindLabelTrack().get(), mEditIndex );
      }
   }
}

void LabelTrackView::OnContextMenu(
   AudacityProject &project, wxCommandEvent & evt )
{
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;

   switch (evt.GetId())
   {
   /// Cut selected text if cut menu item is selected
   case OnCutSelectedTextID:
      if (CutSelectedText( project ))
      {
         ProjectHistory::Get( project ).PushState(XO("Modified Label"),
                      XO("Label Edit"),
                      mTextEditIndex.IsModified() ? UndoPush::CONSOLIDATE : UndoPush::NONE);
      }
      break;

   /// Copy selected text if copy menu item is selected
   case OnCopySelectedTextID:
      CopySelectedText( project );
      break;

   /// paste selected text if paste menu item is selected
   case OnPasteSelectedTextID:
      if (PasteSelectedText(
         project, selectedRegion.t0(), selectedRegion.t1() ))
      {
         ProjectHistory::Get( project ).PushState(XO("Modified Label"),
                      XO("Label Edit"),
                      mTextEditIndex.IsModified() ? UndoPush::CONSOLIDATE : UndoPush::NONE);
      }
      break;

   /// DELETE selected label
   case OnDeleteSelectedLabelID: {
      if (IsValidIndex(mTextEditIndex, project))
      {
         const auto pTrack = FindLabelTrack();
         pTrack->DeleteLabel(mTextEditIndex);
         ProjectHistory::Get( project ).PushState(XO("Deleted Label"),
                      XO("Label Edit"),
                      UndoPush::CONSOLIDATE);
      }
   }
      break;

   case OnEditSelectedLabelID: {
      // Bug #2571: See above
      if (IsValidIndex(mTextEditIndex, project))
         mEditIndex = mTextEditIndex;
   }
      break;
   }
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
   const auto &mLabels = pTrack->GetLabels();
   { int i = -1; for (const auto &labelStruct : mLabels) { ++i;
      if( fabs( labelStruct.getT0() - t ) > delta )
         continue;
      if( fabs( labelStruct.getT1() - t1 ) > delta )
         continue;
      return i;
   }}

   return wxNOT_FOUND;
}


// restoreFocus of -1 is the default, and sets the focus to this label.
// restoreFocus of -2 or other value leaves the focus unchanged.
// restoreFocus >= 0 will later cause focus to move to that track (counting
// tracks, not channels)
int LabelTrackView::AddLabel(const SelectedRegion &selectedRegion,
                         const wxString &title, int restoreFocus)
{
   const auto pTrack = FindLabelTrack();
   mRestoreFocus = restoreFocus;
   auto pos = pTrack->AddLabel( selectedRegion, title );
   return pos;
}

void LabelTrackView::OnTextEditFinished(AudacityProject* project, const wxString& text)
{
    if (mRestoreFocus >= 0) {
        auto& trackList = TrackList::Get(*project);
        auto track = *trackList.Any()
            .begin().advance(mRestoreFocus);
        if (track)
            TrackFocus::Get(*project).Set(track);
        mRestoreFocus = -2;
    }
    SetNavigationIndex(mTextEditIndex);
    ResetTextSelection();
}

void LabelTrackView::OnTextEditCancelled(AudacityProject* project)
{
    if (IsValidIndex(mTextEditIndex, *project)) 
    {
        mTextEditIndex.SetModified(true);
        auto track = FindLabelTrack();
        auto& labels = track->GetLabels();
        auto label = labels[mTextEditIndex];
        label.title = mUndoLabel;

        track->SetLabel(mTextEditIndex, label);

        ProjectHistory::Get(*project).PushState(XO("Modified Label"),
            XO("Label Edit"),
            mTextEditIndex.IsModified() ? UndoPush::CONSOLIDATE : UndoPush::NONE);
        ResetTextSelection();
    }
}

void LabelTrackView::OnTextModified(AudacityProject* project, const wxString& text)
{
    if (IsValidIndex(mTextEditIndex, *project))
    {
        auto track = FindLabelTrack();
        auto& labels = track->GetLabels();
        auto label = labels[mTextEditIndex];
        label.title = text;

        track->SetLabel(mTextEditIndex, label);

        ProjectHistory::Get(*project).PushState(XO("Modified Label"),
            XO("Label Edit"),
            mTextEditIndex.IsModified() ? UndoPush::CONSOLIDATE : UndoPush::NONE);

        mTextEditIndex.SetModified(true);
    }
}

void LabelTrackView::OnTextContextMenu(AudacityProject* project, const wxPoint& position)
{
    ShowContextMenu(*project, position);
}

std::shared_ptr<TextEditHelper> LabelTrackView::StartLabelTextEdit(int labelIndex, AudacityProject* project)
{
   if (mTextEditIndex == labelIndex && mTextEditHelper)
      return mTextEditHelper;

   auto labelTrack = std::static_pointer_cast<LabelTrack>(FindTrack());
   if (labelIndex < labelTrack->GetNumLabels())
   {
      auto label = labelTrack->GetLabel(labelIndex);
      
      mUndoLabel = label->title;
      mTextEditIndex = labelIndex;
      mTextEditHelper = MakeTextEditHelper(*label);
      return mTextEditHelper;
   }
   return { };
}

void LabelTrackView::OnLabelAdded( const LabelTrackEvent &e )
{
   if ( e.mpTrack.lock() != FindTrack() )
      return;

   const auto &title = e.mTitle;
   const auto pos = e.mPresentPosition;

   // restoreFocus is -2 e.g. from Nyquist label creation, when we should not
   // even lose the focus and open the label to edit in the first place.
   // -1 means we don't need to restore it to anywhere.
   // 0 or above is the track to restore to after editing the label is complete.
   if (mRestoreFocus >= -1)
      SetTextSelection(pos, title.Length(), title.Length());
   if( mRestoreFocus < 0 )
      mRestoreFocus = -2;

   OnTitleChange(e);
}

void LabelTrackView::OnLabelDeleted( const LabelTrackEvent &e )
{
   if ( e.mpTrack.lock() != FindTrack() )
      return;

   auto index = e.mFormerPosition;

   // IF we've deleted the selected label
   // THEN set no label selected.
   if (mTextEditIndex == index)
      ResetTextSelection();
   
   // IF we removed a label before the selected label
   // THEN the NEW selected label number is one less.
   else if( index < mTextEditIndex)
      --mTextEditIndex;//NB: Keep cursor selection region
}

void LabelTrackView::OnLabelPermuted( const LabelTrackEvent &e )
{
   if ( e.mpTrack.lock() != FindTrack() )
      return;

   auto former = e.mFormerPosition;
   auto present = e.mPresentPosition;

   auto fix = [&](Index& index) {
       if (index == former)
           index = present;
       else if (former < index && index <= present)
           --index;
       else if (former > index && index >= present)
           ++index;
   };
   fix(mNavigationIndex);
   fix(mTextEditIndex);
}

void LabelTrackView::OnSelectionChange( const LabelTrackEvent &e )
{
   if ( e.mpTrack.lock() != FindTrack() )
      return;

   if (!FindTrack()->GetSelected())
   {
       SetNavigationIndex(-1);
       ResetTextSelection();
   }
}

void LabelTrackView::OnTitleChange(const LabelTrackEvent &e)
{
   wxMemoryDC dc;
   if (msFont.Ok())
      dc.SetFont(msFont);
   const auto pTrack = std::static_pointer_cast<LabelTrack>(
      FindTrack()->SubstitutePendingChangedTrack());
   auto &labelStruct = pTrack->GetLabels()[e.mPresentPosition];
   UpdateOneWidth(dc, labelStruct);
}

void LabelTrackView::UpdateAllWidths()
{
   wxMemoryDC dc;
   if (msFont.Ok())
      dc.SetFont(msFont);
   const auto pTrack = std::static_pointer_cast<LabelTrack>(
      FindTrack()->SubstitutePendingChangedTrack());
   for (auto &labelStruct : pTrack->GetLabels())
      UpdateOneWidth(dc, labelStruct);
}

void LabelTrackView::UpdateOneWidth(wxDC &dc, const LabelStruct &labelStruct)
{
   wxCoord textWidth, textHeight;
   dc.GetTextExtent(labelStruct.title, &textWidth, &textHeight);
   LabelLayout::Get(labelStruct).width = textWidth;
}

wxBitmap & LabelTrackView::GetGlyph( int i)
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
void LabelTrackView::CreateCustomGlyphs()
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
         // The highlighted region (if any) is white...
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

#include "../../../LabelDialog.h"

void LabelTrackView::DoEditLabels
(AudacityProject &project, LabelTrack *lt, int index)
{
   const auto &formats = ProjectNumericFormats::Get( project );
   auto format = formats.GetSelectionFormat(),
      freqFormat = formats.GetFrequencySelectionFormatName();
   auto &tracks = TrackList::Get( project );
   auto &viewInfo = ViewInfo::Get( project );
   auto &window = ProjectWindow::Get( project );

   LabelDialog dlg(&window, project, &tracks,
                   lt, index,
                   viewInfo,
                   format, freqFormat);
#ifdef __WXGTK__
   dlg.Raise();
#endif

   if (dlg.ShowModal() == wxID_OK) {
      ProjectHistory::Get( project )
         .PushState(XO("Edited labels"), XO("Label"));
   }
}

int LabelTrackView::DialogForLabelName(
   AudacityProject &project,
   const SelectedRegion& region, const wxString& initialValue, wxString& value)
{
   auto &trackFocus = TrackFocus::Get( project );
   auto &trackPanel = TrackPanel::Get( project );
   auto &viewInfo = ViewInfo::Get( project );

   wxPoint position =
      trackPanel.FindTrackRect( trackFocus.Get() ).GetBottomLeft();
   // The start of the text in the text box will be roughly in line with the label's position
   // if it's a point label, or the start of its region if it's a region label.
   position.x += viewInfo.GetLeftOffset()
      + std::max(0, static_cast<int>(viewInfo.TimeToPosition(region.t0())))
      - 39;
   position.y += 2;  // just below the bottom of the track
   position = trackPanel.ClientToScreen(position);
   auto &window = GetProjectFrame( project );
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

   dialog.SetInsertionPointEnd();      // because, by default, initial text is selected
   int status = dialog.ShowModal();
   if (status != wxID_CANCEL) {
      value = dialog.GetValue();
      value.Trim(true).Trim(false);
   }

   return status;
}

bool LabelTrackView::IsTextSelected(AudacityProject& project) const
{
   if (IsValidIndex(mTextEditIndex, project) && mTextEditHelper)
      return !mTextEditHelper->IsSelectionEmpty();
   return false;
}


void LabelTrackView::SetTextSelection(int labelIndex, const std::pair<int, int>& selection)
{
    SetTextSelection(labelIndex, selection.first, selection.second);
}

void LabelTrackView::SetTextSelection(int labelIndex, int start, int end)
{
    if (labelIndex < 0)
        return;

    if ((mTextEditIndex == labelIndex) && mTextEditHelper)
        mTextEditHelper->SetSelection(start, end);
    else
    {
        auto labelTrack = std::static_pointer_cast<LabelTrack>(FindTrack());
        if (labelIndex >= labelTrack->GetNumLabels())
            return;

        auto label = labelTrack->GetLabel(labelIndex);

        mUndoLabel = label->title;

        mTextEditIndex = labelIndex;
        mTextEditHelper = MakeTextEditHelper(*label);
    }
}

using DoGetLabelTrackView = DoGetView::Override<LabelTrack>;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(DoGetLabelTrackView) {
   return [](LabelTrack &track, size_t) {
      return std::make_shared<LabelTrackView>( track.SharedPointer() );
   };
}

std::shared_ptr<ChannelVRulerControls> LabelTrackView::DoGetVRulerControls()
{
   return
      std::make_shared<LabelTrackVRulerControls>( shared_from_this() );
}

using GetLabelTrackSyncLockPolicy =
   GetSyncLockPolicy::Override< const LabelTrack >;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(GetLabelTrackSyncLockPolicy) {
   return [](auto &) { return SyncLockPolicy::EndSeparator; };
}
