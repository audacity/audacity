/**********************************************************************

 Audacity: A Digital Audio Editor

 TrackPanelDrawingContext.h

 Paul Licameli

 **********************************************************************/

#ifndef __AUDACITY_TRACK_PANEL_DRAWING_CONTEXT__
#define __AUDACITY_TRACK_PANEL_DRAWING_CONTEXT__

#include "MemoryX.h"

class UIHandle;
using UIHandlePtr = std::shared_ptr<UIHandle>;
class wxDC;

#include <wx/mousestate.h>

// MSVC 2013 says this can't be instantiated - but in fact it can
// using {} syntax.
struct TrackPanelDrawingContext {
   wxDC &dc;
   UIHandlePtr target;
   wxMouseState lastState;

   // MSVC 2013 has a bug and reports
   // warning C4610: struct 'TrackPanelDrawingContext' 
   // can never be instantiated 
#ifdef _MSC_VER
   // Add a default initialiser here to workaround that?
   //TrackPanelDrawingContext();

#endif

};

#endif
