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

   struct TCPLine;

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

   void MidiControlsDrawFunction
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track *pTrack );

   template<typename TrackClass>
   void SliderDrawFunction
      ( LWSlider *(*Selector)
           (const wxRect &sliderRect, const TrackClass *t, bool captured,
            wxWindow*),
        wxDC *dc, const wxRect &rect, const Track *pTrack,
        bool captured, bool highlight );

   void PanSliderDrawFunction
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track *pTrack );

   void GainSliderDrawFunction
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track *pTrack );

#ifdef EXPERIMENTAL_MIDI_OUT
   void VelocitySliderDrawFunction
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track *pTrack );
#endif

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

   void StatusDrawFunction
      ( const wxString &string, wxDC *dc, const wxRect &rect );

   void Status1DrawFunction
      ( TrackPanelDrawingContext &context,
        const wxRect &rect, const Track *pTrack );

   void Status2DrawFunction
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

   void GetGainRect(const wxPoint & topLeft, wxRect &dest);

   void GetPanRect(const wxPoint & topLeft, wxRect &dest);

#ifdef EXPERIMENTAL_MIDI_OUT
   void GetVelocityRect(const wxPoint & topLeft, wxRect &dest);
#endif

   void GetMinimizeHorizontalBounds( const wxRect &rect, wxRect &dest );
   void GetMinimizeRect(const wxRect & rect, wxRect &dest);

   void GetSelectButtonHorizontalBounds( const wxRect &rect, wxRect &dest );
   void GetSelectButtonRect(const wxRect & rect, wxRect &dest);

   void GetSyncLockHorizontalBounds( const wxRect &rect, wxRect &dest );
   void GetSyncLockIconRect(const wxRect & rect, wxRect &dest);

#ifdef USE_MIDI
   void GetMidiControlsHorizontalBounds
      ( const wxRect &rect, wxRect &dest );
   void GetMidiControlsRect(const wxRect & rect, wxRect &dest);
#endif

   bool HideTopItem( const wxRect &rect, const wxRect &subRect,
                               int allowance = 0 );

   unsigned DefaultNoteTrackHeight();
   unsigned DefaultWaveTrackHeight();

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
