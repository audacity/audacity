/**********************************************************************

Audacity: A Digital Audio Editor

CommonTrackInfo.cpp

Paul Licameli split from TrackInfo.cpp

********************************************************************//*!

\namespace CommonTrackInfo
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
#include "CommonTrackInfo.h"

#include <wx/dc.h>
#include "AColor.h"
#include "AllThemeResources.h"
#include "SyncLock.h"
#include "Theme.h"
#include "Track.h"
#include "TrackControls.h"
#include "TrackPanelDrawingContext.h"
#include "UIHandle.h"
#include "ViewInfo.h"
#include "tracks/ui/ChannelView.h"

#define RANGE(array) std::begin(array), std::end(array)
using TCPLine = TrackInfo::TCPLine;
using TCPLines = TrackInfo::TCPLines;

static const TCPLines &commonTrackTCPLines()
{
   static const TCPLines theLines{
      { TCPLine::kItemBarButtons, kTrackInfoBtnSize, 0,
        &CommonTrackInfo::CloseTitleDrawFunction },
   };
   return theLines;
}

const TCPLines &CommonTrackInfo::StaticTCPLines()
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

namespace {

// Items for the bottom of the panel, listed bottom-upwards
// As also with the top items, the extra space is below the item
const TrackInfo::TCPLine defaultCommonTrackTCPBottomLines[] = {
   // The '0' avoids impinging on bottom line of TCP
   // Use -1 if you do want to do so.
   { TCPLine::kItemSyncLock | TCPLine::kItemMinimize, kTrackInfoBtnSize, 0,
     &CommonTrackInfo::MinimizeSyncLockDrawFunction },
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

unsigned CommonTrackInfo::MinimumTrackHeight()
{
   unsigned height = 0;
   if (!commonTrackTCPLines().empty())
      height += commonTrackTCPLines().front().height;
   if (!commonTrackTCPBottomLines.empty())
      height += commonTrackTCPBottomLines.front().height;
   // + 1 prevents the top item from disappearing for want of enough space,
   // according to the rules in HideTopItem.
   return height + kVerticalPadding + 1;
}

bool CommonTrackInfo::HideTopItem( const wxRect &rect, const wxRect &subRect,
                 int allowance ) {
   auto limit = CalcBottomItemY
   ( commonTrackTCPBottomLines, TCPLine::kHighestBottomItem, rect.height).first;
   // Return true if the rectangle is even touching the limit
   // without an overlap.  That was the behavior as of 2.1.3.
   return subRect.y + subRect.height - allowance >= rect.y + limit;
}

void CommonTrackInfo::DrawItems
( TrackPanelDrawingContext &context,
  const wxRect &rect, const Track &track  )
{
   auto &trackControl = TrackControls::Get(track);
   const auto &topLines = trackControl.GetTCPLines();
   const auto &bottomLines = commonTrackTCPBottomLines;
   DrawItems
      ( context, rect, &track, topLines, bottomLines );
}

void CommonTrackInfo::DrawItems
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
         if ( !CommonTrackInfo::HideTopItem( rect, itemRect ) &&
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

void CommonTrackInfo::DrawCloseButton(
   TrackPanelDrawingContext &context, const wxRect &bev,
   const Channel *pChannel, UIHandle *target)
{
   auto dc = &context.dc;
   auto pTrack = pChannel
      ? dynamic_cast<const Track*>(&pChannel->GetChannelGroup())
      : nullptr;
   bool selected = pTrack ? pTrack->GetSelected() : true;
   bool hit = target && target->FindTrack().get() == pTrack;
   bool captured = hit && target->IsDragging();
   bool down = captured && bev.Contains( context.lastState.GetPosition());
   AColor::Bevel2(*dc, !down, bev, selected, hit );

   wxPen pen( theTheme.Colour( clrTrackPanelText ));
   dc->SetPen( pen );
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

void CommonTrackInfo::CloseTitleDrawFunction
( TrackPanelDrawingContext &context,
  const wxRect &rect, const Track *pTrack )
{
   auto dc = &context.dc;
   bool selected = pTrack ? pTrack->GetSelected() : true;
   {
      wxRect bev = rect;
      GetCloseBoxHorizontalBounds( rect, bev );
      auto target = context.target.get();
      DrawCloseButton(context, bev,
         (*pTrack->Channels().begin()).get(), target);
   }

   {
      wxRect bev = rect;
      GetTitleBarHorizontalBounds( rect, bev );
      auto target = context.target.get();
      bool hit = target &&
         target->FindTrack().get() == pTrack;
      bool captured = hit && target->IsDragging();
      bool down = captured && bev.Contains( context.lastState.GetPosition());
      wxString titleStr =
         pTrack ? pTrack->GetName() : _("Name");

      //bev.Inflate(-1, -1);
      AColor::Bevel2(*dc, !down, bev, selected, hit);

      // Draw title text
      TrackInfo::SetTrackInfoFont(dc);

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
      wxColour c = theTheme.Colour( clrTrackPanelText );

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

void CommonTrackInfo::MinimizeSyncLockDrawFunction
( TrackPanelDrawingContext &context,
  const wxRect &rect, const Track *pTrack )
{
   auto dc = &context.dc;
   bool selected = pTrack ? pTrack->GetSelected() : true;
   bool syncLockSelected =
      pTrack ? SyncLock::IsSyncLockSelected(*pTrack) : true;
   bool minimized =
      pTrack ? ChannelView::Get(*pTrack->GetChannel(0)).GetMinimized() : false;
   {
      wxRect bev = rect;
      GetMinimizeHorizontalBounds(rect, bev);
      auto target = context.target.get();
      bool hit = target && target->FindTrack().get() == pTrack;
      bool captured = hit && target->IsDragging();
      bool down = captured && bev.Contains( context.lastState.GetPosition());

      // Clear background to get rid of previous arrow
      //AColor::MediumTrackInfo(dc, t->GetSelected());
      //dc->DrawRectangle(bev);

      AColor::Bevel2(*dc, !down, bev, selected, hit);

      wxColour c = theTheme.Colour(clrTrackPanelText);
      dc->SetBrush(c);
      dc->SetPen(c);

      AColor::Arrow(*dc,
                    bev.x - 5 + bev.width / 2,
                    bev.y - 2 + bev.height / 2,
                    10,
                    minimized);
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

void CommonTrackInfo::GetCloseBoxHorizontalBounds( const wxRect & rect, wxRect &dest )
{
   dest.x = rect.x;
   dest.width = kTrackInfoBtnSize;
}

void CommonTrackInfo::GetCloseBoxRect(const wxRect & rect, wxRect & dest)
{
   GetCloseBoxHorizontalBounds( rect, dest );
   auto results = CalcItemY( commonTrackTCPLines(), TCPLine::kItemBarButtons );
   dest.y = rect.y + results.first;
   dest.height = results.second;
}

void CommonTrackInfo::GetTitleBarHorizontalBounds( const wxRect & rect, wxRect &dest )
{
   // to right of CloseBoxRect, plus a little more
   wxRect closeRect;
   GetCloseBoxHorizontalBounds( rect, closeRect );
   dest.x = rect.x + closeRect.width + 2;
   dest.width = rect.x + rect.width - dest.x;
}

void CommonTrackInfo::GetTitleBarRect(const wxRect & rect, wxRect & dest)
{
   GetTitleBarHorizontalBounds( rect, dest );
   auto results = CalcItemY( commonTrackTCPLines(), TCPLine::kItemBarButtons );
   dest.y = rect.y + results.first;
   dest.height = results.second;
}

void CommonTrackInfo::GetSliderHorizontalBounds( const wxPoint &topleft, wxRect &dest )
{
   dest.x = topleft.x + 6;
   dest.width = kTrackInfoSliderWidth;
}

void CommonTrackInfo::GetMinimizeHorizontalBounds( const wxRect &rect, wxRect &dest )
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

void CommonTrackInfo::GetMinimizeRect(const wxRect & rect, wxRect &dest)
{
   GetMinimizeHorizontalBounds( rect, dest );
   auto results = CalcBottomItemY
      ( commonTrackTCPBottomLines, TCPLine::kItemMinimize, rect.height);
   dest.y = rect.y + results.first;
   dest.height = results.second;
}

void CommonTrackInfo::GetSyncLockHorizontalBounds( const wxRect &rect, wxRect &dest )
{
   dest.width = kTrackInfoBtnSize;
   dest.x = rect.x + rect.width - dest.width;
}

void CommonTrackInfo::GetSyncLockIconRect(const wxRect & rect, wxRect &dest)
{
   GetSyncLockHorizontalBounds( rect, dest );
   auto results = CalcBottomItemY
      ( commonTrackTCPBottomLines, TCPLine::kItemSyncLock, rect.height);
   dest.y = rect.y + results.first;
   dest.height = results.second;
}

unsigned CommonTrackInfo::DefaultTrackHeight( const TCPLines &topLines )
{
   int needed =
      kVerticalPadding +
      totalTCPLines( topLines, true ) +
      totalTCPLines( commonTrackTCPBottomLines, false ) + 1;
   return (unsigned) std::max(needed, (int) ChannelView::DefaultHeight);
}
