/**********************************************************************

Audacity: A Digital Audio Editor

TrackInfo.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_TRACK_INFO__
#define __AUDACITY_TRACK_INFO__




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
   AUDACITY_DLL_API
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
   AUDACITY_DLL_API
   std::pair< int, int > CalcItemY( const TCPLines &lines, unsigned iItem );

   AUDACITY_DLL_API
   unsigned DefaultTrackHeight( const TCPLines &topLines );

   AUDACITY_DLL_API
   void DrawItems
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track &track );

   AUDACITY_DLL_API
   void DrawItems
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track *pTrack,
        const std::vector<TCPLine> &topLines,
        const std::vector<TCPLine> &bottomLines );

   AUDACITY_DLL_API
   void DrawCloseButton(
      TrackPanelDrawingContext &context, const wxRect &bev,
      const Track *pTrack, ButtonHandle *target );

   AUDACITY_DLL_API
   void CloseTitleDrawFunction
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track *pTrack );

   AUDACITY_DLL_API
   void MinimizeSyncLockDrawFunction
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track *pTrack );

   AUDACITY_DLL_API
   void SetTrackInfoFont(wxDC *dc);


   AUDACITY_DLL_API
   void GetCloseBoxHorizontalBounds( const wxRect & rect, wxRect &dest );
   AUDACITY_DLL_API
   void GetCloseBoxRect(const wxRect & rect, wxRect &dest);

   AUDACITY_DLL_API
   void GetTitleBarHorizontalBounds( const wxRect & rect, wxRect &dest );
   AUDACITY_DLL_API
   void GetTitleBarRect(const wxRect & rect, wxRect &dest);

   AUDACITY_DLL_API
   void GetSliderHorizontalBounds( const wxPoint &topleft, wxRect &dest );

   AUDACITY_DLL_API
   void GetMinimizeHorizontalBounds( const wxRect &rect, wxRect &dest );
   AUDACITY_DLL_API
   void GetMinimizeRect(const wxRect & rect, wxRect &dest);

   AUDACITY_DLL_API
   void GetSelectButtonHorizontalBounds( const wxRect &rect, wxRect &dest );
   AUDACITY_DLL_API
   void GetSelectButtonRect(const wxRect & rect, wxRect &dest);

   AUDACITY_DLL_API
   void GetSyncLockHorizontalBounds( const wxRect &rect, wxRect &dest );
   AUDACITY_DLL_API
   void GetSyncLockIconRect(const wxRect & rect, wxRect &dest);

   AUDACITY_DLL_API
   bool HideTopItem( const wxRect &rect, const wxRect &subRect,
                               int allowance = 0 );

   // Non-member, namespace function relying on TrackPanel to invoke it
   // when it handles preference update events
   AUDACITY_DLL_API
   void UpdatePrefs( wxWindow *pParent );

   AUDACITY_DLL_API
   bool HasSoloButton();
};

#endif
