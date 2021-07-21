/**********************************************************************

Audacity: A Digital Audio Editor

TrackInfo.cpp

Paul Licameli split from TrackPanel.cpp

********************************************************************//*!

\namespace TrackInfo
\brief
  Functions for drawing the track control panel, which is shown to the side
  of a track
  It has the menus, pan and gain controls displayed in it.
  So "Info" is somewhat a misnomer. Should possibly be "TrackControls".

  It maintains global slider widget instances that are reparented and
  repositioned as needed for drawing and interaction with the user,
  interoperating with the custom panel subdivision implemented in CellularPanel
  and avoiding wxWidgets sizers

  If we'd instead coded it as a wxWindow, we would have an instance
  of this class for each track displayed.

**********************************************************************/


#include "TrackInfo.h"

#include <wx/app.h>
#include <wx/dc.h>
#include <wx/frame.h>

#include "AColor.h"
#include "AllThemeResources.h"
#include "Prefs.h"
#include "Project.h"
#include "Theme.h"
#include "Track.h"
#include "TrackPanelDrawingContext.h"
#include "ViewInfo.h"
#include "prefs/TracksBehaviorsPrefs.h"
#include "tracks/ui/TrackView.h"

// Subscribe to preference changes to update static variables
struct Settings : PrefsListener {
   wxString gSoloPref;
   wxFont gFont;

   bool mInitialized{ false };

   void UpdatePrefs() override
   {
      gSoloPref = TracksBehaviorsSolo.Read();

      // Calculation of best font size depends on language, so it should be redone in case
      // the language preference changed.

      // wxWidgets seems to need a window to do this portably.
      if ( !wxTheApp )
         return;
      auto window = wxTheApp->GetTopWindow();
      if ( !window )
         return;

      int fontSize = 10;
      gFont.Create(fontSize, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL);

      int allowableWidth =
         // PRL:  was it correct to include the margin?
         ( kTrackInfoWidth + kLeftMargin )
            - 2; // 2 to allow for left/right borders
      int textWidth;
      do {
         gFont.SetPointSize(fontSize);
         window->GetTextExtent(_("Stereo, 999999Hz"),
            &textWidth, nullptr, nullptr, nullptr, &gFont);
         fontSize--;
      } while (textWidth >= allowableWidth);

      mInitialized = true;
   }
};

static Settings &settings()
{
   static Settings theSettings;
   if ( !theSettings.mInitialized )
      theSettings.UpdatePrefs();
   return theSettings;
}

bool TrackInfo::HasSoloButton()
{
   return settings().gSoloPref != wxT("None");
}

#define RANGE(array) (array), (array) + sizeof(array)/sizeof(*(array))
using TCPLine = TrackInfo::TCPLine;
using TCPLines = TrackInfo::TCPLines;

static const TCPLines &commonTrackTCPLines()
{
   static const TCPLines theLines{
#ifdef EXPERIMENTAL_DA

      { TCPLine::kItemBarButtons, kTrackInfoBtnSize, 4,
        &TrackInfo::CloseTitleDrawFunction },

#else

      { TCPLine::kItemBarButtons, kTrackInfoBtnSize, 0,
        &TrackInfo::CloseTitleDrawFunction },

#endif
   };
   return theLines;
}

#include "tracks/ui/CommonTrackControls.h"
const TCPLines &CommonTrackControls::StaticTCPLines()
{
   return commonTrackTCPLines();
}

namespace {

int totalTCPLines( const TCPLines &lines, bool omitLastExtra )
{
   int total = 0;
   int lastExtra = 0;
   for ( const auto line : lines ) {
      lastExtra = line.extraSpace;
      total += line.height + lastExtra;
   }
   if (omitLastExtra)
      total -= lastExtra;
   return total;
}
}

// return y value and height
std::pair< int, int >
TrackInfo::CalcItemY( const TCPLines &lines, unsigned iItem )
{
   int y = 0;
   auto pLines = lines.begin();
   while ( pLines != lines.end() &&
           0 == (pLines->items & iItem) ) {
      y += pLines->height + pLines->extraSpace;
      ++pLines;
   }
   int height = 0;
   if ( pLines != lines.end() )
      height = pLines->height;
   return { y, height };
}

namespace {

// Items for the bottom of the panel, listed bottom-upwards
// As also with the top items, the extra space is below the item
const TrackInfo::TCPLine defaultCommonTrackTCPBottomLines[] = {
   // The '0' avoids impinging on bottom line of TCP
   // Use -1 if you do want to do so.
   { TCPLine::kItemSyncLock | TCPLine::kItemMinimize, kTrackInfoBtnSize, 0,
     &TrackInfo::MinimizeSyncLockDrawFunction },
};
TCPLines commonTrackTCPBottomLines{ RANGE(defaultCommonTrackTCPBottomLines) };

// return y value and height
std::pair< int, int > CalcBottomItemY
   ( const TCPLines &lines, unsigned iItem, int height )
{
   int y = height;
   auto pLines = lines.begin();
   while ( pLines != lines.end() &&
           0 == (pLines->items & iItem) ) {
      y -= pLines->height + pLines->extraSpace;
      ++pLines;
   }
   if (pLines != lines.end())
      y -= (pLines->height + pLines->extraSpace );
   return { y, pLines->height };
}

}

const TCPLines &CommonTrackControls::GetTCPLines() const
{
   return commonTrackTCPLines();
}

unsigned TrackInfo::MinimumTrackHeight()
{
   unsigned height = 0;
   if (!commonTrackTCPLines().empty())
      height += commonTrackTCPLines().front().height;
   if (!commonTrackTCPBottomLines.empty())
      height += commonTrackTCPBottomLines.front().height;
   // + 1 prevents the top item from disappearing for want of enough space,
   // according to the rules in HideTopItem.
   return height + kTopMargin + kBottomMargin + 1;
}

bool TrackInfo::HideTopItem( const wxRect &rect, const wxRect &subRect,
                 int allowance ) {
   auto limit = CalcBottomItemY
   ( commonTrackTCPBottomLines, TCPLine::kHighestBottomItem, rect.height).first;
   // Return true if the rectangle is even touching the limit
   // without an overlap.  That was the behavior as of 2.1.3.
   return subRect.y + subRect.height - allowance >= rect.y + limit;
}

void TrackInfo::DrawItems
( TrackPanelDrawingContext &context,
  const wxRect &rect, const Track &track  )
{
   auto &trackControl = static_cast<const CommonTrackControls&>(
      TrackControls::Get( track ) );
   const auto &topLines = trackControl.GetTCPLines();
   const auto &bottomLines = commonTrackTCPBottomLines;
   DrawItems
      ( context, rect, &track, topLines, bottomLines );
}

void TrackInfo::DrawItems
( TrackPanelDrawingContext &context,
  const wxRect &rect, const Track *pTrack,
  const std::vector<TCPLine> &topLines, const std::vector<TCPLine> &bottomLines )
{
   auto dc = &context.dc;
   TrackInfo::SetTrackInfoFont(dc);
   dc->SetTextForeground(theTheme.Colour(clrTrackPanelText));

   {
      int yy = 0;
      for ( const auto &line : topLines ) {
         wxRect itemRect{
            rect.x, rect.y + yy,
            rect.width, line.height
         };
         if ( !TrackInfo::HideTopItem( rect, itemRect ) &&
              line.drawFunction )
            line.drawFunction( context, itemRect, pTrack );
         yy += line.height + line.extraSpace;
      }
   }
   {
      int yy = rect.height;
      for ( const auto &line : bottomLines ) {
         yy -= line.height + line.extraSpace;
         if ( line.drawFunction ) {
            wxRect itemRect{
               rect.x, rect.y + yy,
               rect.width, line.height
            };
            line.drawFunction( context, itemRect, pTrack );
         }
      }
   }
}

#include "tracks/ui/TrackButtonHandles.h"
void TrackInfo::DrawCloseButton(
   TrackPanelDrawingContext &context, const wxRect &bev,
   const Track *pTrack, ButtonHandle *target )
{
   auto dc = &context.dc;
   bool selected = pTrack ? pTrack->GetSelected() : true;
   bool hit = target && target->GetTrack().get() == pTrack;
   bool captured = hit && target->IsClicked();
   bool down = captured && bev.Contains( context.lastState.GetPosition());
   AColor::Bevel2(*dc, !down, bev, selected, hit );

#ifdef EXPERIMENTAL_THEMING
   wxPen pen( theTheme.Colour( clrTrackPanelText ));
   dc->SetPen( pen );
#else
   dc->SetPen(*wxBLACK_PEN);
#endif
   bev.Inflate( -1, -1 );
   // Draw the "X"
   const int s = 6;

   int ls = bev.x + ((bev.width - s) / 2);
   int ts = bev.y + ((bev.height - s) / 2);
   int rs = ls + s;
   int bs = ts + s;

   AColor::Line(*dc, ls,     ts, rs,     bs);
   AColor::Line(*dc, ls + 1, ts, rs + 1, bs);
   AColor::Line(*dc, rs,     ts, ls,     bs);
   AColor::Line(*dc, rs + 1, ts, ls + 1, bs);
   //   bev.Inflate(-1, -1);
}

void TrackInfo::CloseTitleDrawFunction
( TrackPanelDrawingContext &context,
  const wxRect &rect, const Track *pTrack )
{
   auto dc = &context.dc;
   bool selected = pTrack ? pTrack->GetSelected() : true;
   {
      wxRect bev = rect;
      GetCloseBoxHorizontalBounds( rect, bev );
      auto target = dynamic_cast<CloseButtonHandle*>( context.target.get() );
      DrawCloseButton( context, bev, pTrack, target );
   }

   {
      wxRect bev = rect;
      GetTitleBarHorizontalBounds( rect, bev );
      auto target = dynamic_cast<MenuButtonHandle*>( context.target.get() );
      bool hit = target && target->GetTrack().get() == pTrack;
      bool captured = hit && target->IsClicked();
      bool down = captured && bev.Contains( context.lastState.GetPosition());
      wxString titleStr =
         pTrack ? pTrack->GetName() : _("Name");

      //bev.Inflate(-1, -1);
      AColor::Bevel2(*dc, !down, bev, selected, hit);

      // Draw title text
      SetTrackInfoFont(dc);

      // Bug 1660 The 'k' of 'Audio Track' was being truncated.
      // Constant of 32 found by counting pixels on a windows machine.
      // I believe it's the size of the X close button + the size of the 
      // drop down arrow.
      int allowableWidth = rect.width - 32;

      wxCoord textWidth, textHeight;
      dc->GetTextExtent(titleStr, &textWidth, &textHeight);
      while (textWidth > allowableWidth) {
         titleStr = titleStr.Left(titleStr.length() - 1);
         dc->GetTextExtent(titleStr, &textWidth, &textHeight);
      }

      // Pop-up triangle
   #ifdef EXPERIMENTAL_THEMING
      wxColour c = theTheme.Colour( clrTrackPanelText );
   #else
      wxColour c = *wxBLACK;
   #endif

      // wxGTK leaves little scraps (antialiasing?) of the
      // characters if they are repeatedly drawn.  This
      // happens when holding down mouse button and moving
      // in and out of the title bar.  So clear it first.
   //   AColor::MediumTrackInfo(dc, t->GetSelected());
   //   dc->DrawRectangle(bev);

      dc->SetTextForeground( c );
      dc->SetTextBackground( wxTRANSPARENT );
      dc->DrawText(titleStr, bev.x + 2, bev.y + (bev.height - textHeight) / 2);



      dc->SetPen(c);
      dc->SetBrush(c);

      int s = 10; // Width of dropdown arrow...height is half of width
      AColor::Arrow(*dc,
                    bev.GetRight() - s - 3, // 3 to offset from right border
                    bev.y + ((bev.height - (s / 2)) / 2),
                    s);

   }
}

void TrackInfo::MinimizeSyncLockDrawFunction
( TrackPanelDrawingContext &context,
  const wxRect &rect, const Track *pTrack )
{
   auto dc = &context.dc;
   bool selected = pTrack ? pTrack->GetSelected() : true;
   bool syncLockSelected = pTrack ? pTrack->IsSyncLockSelected() : true;
   bool minimized =
      pTrack ? TrackView::Get( *pTrack ).GetMinimized() : false;
   {
      wxRect bev = rect;
      GetMinimizeHorizontalBounds(rect, bev);
      auto target = dynamic_cast<MinimizeButtonHandle*>( context.target.get() );
      bool hit = target && target->GetTrack().get() == pTrack;
      bool captured = hit && target->IsClicked();
      bool down = captured && bev.Contains( context.lastState.GetPosition());

      // Clear background to get rid of previous arrow
      //AColor::MediumTrackInfo(dc, t->GetSelected());
      //dc->DrawRectangle(bev);

      AColor::Bevel2(*dc, !down, bev, selected, hit);

#ifdef EXPERIMENTAL_THEMING
      wxColour c = theTheme.Colour(clrTrackPanelText);
      dc->SetBrush(c);
      dc->SetPen(c);
#else
      AColor::Dark(dc, selected);
#endif

      AColor::Arrow(*dc,
                    bev.x - 5 + bev.width / 2,
                    bev.y - 2 + bev.height / 2,
                    10,
                    minimized);
   }

   {
      wxRect bev = rect;
      GetSelectButtonHorizontalBounds(rect, bev);
      auto target = dynamic_cast<SelectButtonHandle*>( context.target.get() );
      bool hit = target && target->GetTrack().get() == pTrack;
      bool captured = hit && target->IsClicked();
      bool down = captured && bev.Contains( context.lastState.GetPosition());

      AColor::Bevel2(*dc, !down, bev, selected, hit);

#ifdef EXPERIMENTAL_THEMING
      wxColour c = theTheme.Colour(clrTrackPanelText);
      dc->SetBrush(c);
      dc->SetPen(c);
#else
      AColor::Dark(dc, selected);
#endif

      wxString str = _("Select");
      wxCoord textWidth;
      wxCoord textHeight;
      SetTrackInfoFont(dc);
      dc->GetTextExtent(str, &textWidth, &textHeight);

      dc->SetTextForeground( c );
      dc->SetTextBackground( wxTRANSPARENT );
      dc->DrawText(str, bev.x + 2 + (bev.width-textWidth)/2, bev.y + (bev.height - textHeight) / 2);
   }


   // Draw the sync-lock indicator if this track is in a sync-lock selected group.
   if (syncLockSelected)
   {
      wxRect syncLockIconRect = rect;
	
      GetSyncLockHorizontalBounds( rect, syncLockIconRect );
      wxBitmap syncLockBitmap(theTheme.Image(bmpSyncLockIcon));
      // Icon is 12x12 and syncLockIconRect is 16x16.
      dc->DrawBitmap(syncLockBitmap,
                     syncLockIconRect.x + 3,
                     syncLockIconRect.y + 2,
                     true);
   }
}

void TrackInfo::GetCloseBoxHorizontalBounds( const wxRect & rect, wxRect &dest )
{
   dest.x = rect.x;
   dest.width = kTrackInfoBtnSize;
}

void TrackInfo::GetCloseBoxRect(const wxRect & rect, wxRect & dest)
{
   GetCloseBoxHorizontalBounds( rect, dest );
   auto results = CalcItemY( commonTrackTCPLines(), TCPLine::kItemBarButtons );
   dest.y = rect.y + results.first;
   dest.height = results.second;
}

void TrackInfo::GetTitleBarHorizontalBounds( const wxRect & rect, wxRect &dest )
{
   // to right of CloseBoxRect, plus a little more
   wxRect closeRect;
   GetCloseBoxHorizontalBounds( rect, closeRect );
   dest.x = rect.x + closeRect.width + 1;
   dest.width = rect.x + rect.width - dest.x + TitleSoloBorderOverlap;
}

void TrackInfo::GetTitleBarRect(const wxRect & rect, wxRect & dest)
{
   GetTitleBarHorizontalBounds( rect, dest );
   auto results = CalcItemY( commonTrackTCPLines(), TCPLine::kItemBarButtons );
   dest.y = rect.y + results.first;
   dest.height = results.second;
}

void TrackInfo::GetSliderHorizontalBounds( const wxPoint &topleft, wxRect &dest )
{
   dest.x = topleft.x + 6;
   dest.width = kTrackInfoSliderWidth;
}

void TrackInfo::GetMinimizeHorizontalBounds( const wxRect &rect, wxRect &dest )
{
   const int space = 0;// was 3.
   dest.x = rect.x + space;

   wxRect syncLockRect;
   GetSyncLockHorizontalBounds( rect, syncLockRect );

   // Width is rect.width less space on left for track select
   // and on right for sync-lock icon.
   dest.width = kTrackInfoBtnSize;
// rect.width - (space + syncLockRect.width);
}

void TrackInfo::GetMinimizeRect(const wxRect & rect, wxRect &dest)
{
   GetMinimizeHorizontalBounds( rect, dest );
   auto results = CalcBottomItemY
      ( commonTrackTCPBottomLines, TCPLine::kItemMinimize, rect.height);
   dest.y = rect.y + results.first;
   dest.height = results.second;
}

void TrackInfo::GetSelectButtonHorizontalBounds( const wxRect &rect, wxRect &dest )
{
   const int space = 0;// was 3.
   dest.x = rect.x + space;

   wxRect syncLockRect;
   GetSyncLockHorizontalBounds( rect, syncLockRect );
   wxRect minimizeRect;
   GetMinimizeHorizontalBounds( rect, minimizeRect );

   dest.x = dest.x + space + minimizeRect.width;
   // Width is rect.width less space on left for track select
   // and on right for sync-lock icon.
   dest.width = rect.width - (space + syncLockRect.width) - (space + minimizeRect.width);
}


void TrackInfo::GetSelectButtonRect(const wxRect & rect, wxRect &dest)
{
   GetSelectButtonHorizontalBounds( rect, dest );
   auto results = CalcBottomItemY
      ( commonTrackTCPBottomLines, TCPLine::kItemMinimize, rect.height);
   dest.y = rect.y + results.first;
   dest.height = results.second;
}

void TrackInfo::GetSyncLockHorizontalBounds( const wxRect &rect, wxRect &dest )
{
   dest.width = kTrackInfoBtnSize;
   dest.x = rect.x + rect.width - dest.width;
}

void TrackInfo::GetSyncLockIconRect(const wxRect & rect, wxRect &dest)
{
   GetSyncLockHorizontalBounds( rect, dest );
   auto results = CalcBottomItemY
      ( commonTrackTCPBottomLines, TCPLine::kItemSyncLock, rect.height);
   dest.y = rect.y + results.first;
   dest.height = results.second;
}

/// \todo Probably should move to 'Utils.cpp'.
void TrackInfo::SetTrackInfoFont(wxDC * dc)
{
   dc->SetFont(settings().gFont);
}

//#define USE_BEVELS

unsigned TrackInfo::DefaultTrackHeight( const TCPLines &topLines )
{
   int needed =
      kTopMargin + kBottomMargin +
      totalTCPLines( topLines, true ) +
      totalTCPLines( commonTrackTCPBottomLines, false ) + 1;
   return (unsigned) std::max( needed, (int) TrackView::DefaultHeight );
}
