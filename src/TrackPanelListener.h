/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackPanelListener.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_TRACK_PANEL_LISTENER__
#define __AUDACITY_TRACK_PANEL_LISTENER__

#include "Audacity.h"

enum class UndoPush : unsigned char;

/*
\brief A now badly named abstract class which was a failed attempt to let
TrackPanel code pretend it doesn't completely know what an AudacityProject is
and use only a limited number of its services.
*/
class AUDACITY_DLL_API TrackPanelListener /* not final */ {

 public:
   TrackPanelListener(){};
   virtual ~TrackPanelListener(){};

   virtual void TP_RedrawScrollbars() = 0;
   virtual void TP_ScrollLeft() = 0;
   virtual void TP_ScrollRight() = 0;
   virtual void TP_ScrollWindow(double scrollto) = 0;
   virtual bool TP_ScrollUpDown(int delta) = 0;
   virtual void TP_HandleResize() = 0;
};

#endif
