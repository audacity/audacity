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
class wxString;
class wxWindow;

class LWSlider;
class NoteTrack;
class Track;
struct TrackPanelDrawingContext;
class WaveTrack;

namespace TrackInfo
{
   void ReCreateSliders();

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

   void CloseTitleDrawFunction
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track *pTrack );

   void MinimizeSyncLockDrawFunction
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track *pTrack );

   void MuteOrSoloDrawFunction
      ( wxDC *dc, const wxRect &rect, const Track *pTrack, bool down,
        bool captured, bool solo, bool hit );

   void WideMuteDrawFunction
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track *pTrack );

   void WideSoloDrawFunction
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track *pTrack );

   void MuteAndSoloDrawFunction
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track *pTrack );

   void SetTrackInfoFont(wxDC *dc);


   void DrawBackground(
      wxDC * dc, const wxRect & rect, bool bSelected, const int vrul );
   // void DrawBordersWithin(
   //   wxDC * dc, const wxRect & rect, const Track &track ) const;

   void GetCloseBoxHorizontalBounds( const wxRect & rect, wxRect &dest );
   void GetCloseBoxRect(const wxRect & rect, wxRect &dest);

   void GetTitleBarHorizontalBounds( const wxRect & rect, wxRect &dest );
   void GetTitleBarRect(const wxRect & rect, wxRect &dest);

   void GetNarrowMuteHorizontalBounds
      ( const wxRect & rect, wxRect &dest );
   void GetNarrowSoloHorizontalBounds
      ( const wxRect & rect, wxRect &dest );
   void GetWideMuteSoloHorizontalBounds
      ( const wxRect & rect, wxRect &dest );
   void GetMuteSoloRect
      (const wxRect & rect, wxRect &dest, bool solo, bool bHasSoloButton,
       const Track *pTrack);

   void GetSliderHorizontalBounds( const wxPoint &topleft, wxRect &dest );

   void GetMinimizeHorizontalBounds( const wxRect &rect, wxRect &dest );
   void GetMinimizeRect(const wxRect & rect, wxRect &dest);

   void GetSelectButtonHorizontalBounds( const wxRect &rect, wxRect &dest );
   void GetSelectButtonRect(const wxRect & rect, wxRect &dest);

   void GetSyncLockHorizontalBounds( const wxRect &rect, wxRect &dest );
   void GetSyncLockIconRect(const wxRect & rect, wxRect &dest);

   bool HideTopItem( const wxRect &rect, const wxRect &subRect,
                               int allowance = 0 );

   LWSlider * GainSlider
      (const wxRect &sliderRect, const WaveTrack *t, bool captured,
       wxWindow *pParent);
   LWSlider * PanSlider
      (const wxRect &sliderRect, const WaveTrack *t, bool captured,
       wxWindow *pParent);

#ifdef EXPERIMENTAL_MIDI_OUT
   LWSlider * VelocitySlider
      (const wxRect &sliderRect, const NoteTrack *t, bool captured,
       wxWindow *pParent);
#endif

   // Non-member, namespace function relying on TrackPanel to invoke it
   // when it handles preference update events
   void UpdatePrefs( wxWindow *pParent );
};

#endif
