/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackPanel2.h

  James Crook

**********************************************************************/

#ifndef __AUDACITY_TRACK_PANEL2__
#define __AUDACITY_TRACK_PANEL2__

#include "TrackPanel.h"

class TrackPanel2 : public TrackPanel
{
public:
   TrackPanel2( 
      wxWindow * parent, wxWindowID id,
      const wxPoint & pos,
      const wxSize & size,
      TrackList * tracks,
      ViewInfo * viewInfo,
      TrackPanelListener * listener,
      AdornedRulerPanel * ruler);

   // Upgrades an existing TrackPanel to a TrackPanel2
   static void Upgrade( TrackPanel ** ppTrackPanel );

   virtual void OnPaint(wxPaintEvent & event);
};

// Factory function.
TrackPanel * TrackPanel2Factory(wxWindow * parent,
   wxWindowID id,
   const wxPoint & pos,
   const wxSize & size,
   TrackList * tracks,
   ViewInfo * viewInfo,
   TrackPanelListener * listener,
   AdornedRulerPanel * ruler);

#endif
