/**********************************************************************

Audacity: A Digital Audio Editor

TrackInfo.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_TRACK_INFO__
#define __AUDACITY_TRACK_INFO__

#include "Audacity.h"
#include "Experimental.h"

#include <vector>

class wxDC;
class wxPoint;
class wxRect;
class wxWindow;

class ButtonHandle;
class LWSlider;
class Track;
struct TrackPanelDrawingContext;

static const int TitleSoloBorderOverlap = 1;

namespace TrackInfo
{
   unsigned MinimumTrackHeight();

   struct TCPLine {
      enum : unsigned {
         // The sequence is not significant, just keep bits distinct
         kItemBarButtons       = 1 << 0,
         kItemStatusInfo1      = 1 << 1,
         kItemMute             = 1 << 2,
         kItemSolo             = 1 << 3,
         kItemGain             = 1 << 4,
         kItemPan              = 1 << 5,
         kItemVelocity         = 1 << 6,
         kItemMidiControlsRect = 1 << 7,
         kItemMinimize         = 1 << 8,
         kItemSyncLock         = 1 << 9,
         kItemStatusInfo2      = 1 << 10,

         kHighestBottomItem = kItemMinimize,
      };

      using DrawFunction = void (*)(
         TrackPanelDrawingContext &context,
         const wxRect &rect,
         const Track *maybeNULL
      );

      unsigned items; // a bitwise OR of values of the enum above
      int height;
      int extraSpace;
      DrawFunction drawFunction;
   };

   using TCPLines = std::vector< TCPLine >;

   // return y value and height
   std::pair< int, int > CalcItemY( const TCPLines &lines, unsigned iItem );

   unsigned DefaultTrackHeight( const TCPLines &topLines );

   void DrawItems
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track &track );

   void DrawItems
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track *pTrack,
        const std::vector<TCPLine> &topLines,
        const std::vector<TCPLine> &bottomLines );

   void DrawCloseButton(
      TrackPanelDrawingContext &context, const wxRect &bev,
      const Track *pTrack, ButtonHandle *target );

   void CloseTitleDrawFunction
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track *pTrack );

   void MinimizeSyncLockDrawFunction
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track *pTrack );

   void SetTrackInfoFont(wxDC *dc);


   void GetCloseBoxHorizontalBounds( const wxRect & rect, wxRect &dest );
   void GetCloseBoxRect(const wxRect & rect, wxRect &dest);

   void GetTitleBarHorizontalBounds( const wxRect & rect, wxRect &dest );
   void GetTitleBarRect(const wxRect & rect, wxRect &dest);

   void GetSliderHorizontalBounds( const wxPoint &topleft, wxRect &dest );

   void GetMinimizeHorizontalBounds( const wxRect &rect, wxRect &dest );
   void GetMinimizeRect(const wxRect & rect, wxRect &dest);

   void GetSelectButtonHorizontalBounds( const wxRect &rect, wxRect &dest );
   void GetSelectButtonRect(const wxRect & rect, wxRect &dest);

   void GetSyncLockHorizontalBounds( const wxRect &rect, wxRect &dest );
   void GetSyncLockIconRect(const wxRect & rect, wxRect &dest);

   bool HideTopItem( const wxRect &rect, const wxRect &subRect,
                               int allowance = 0 );

   // Non-member, namespace function relying on TrackPanel to invoke it
   // when it handles preference update events
   void UpdatePrefs( wxWindow *pParent );

   bool HasSoloButton();
};

#endif
