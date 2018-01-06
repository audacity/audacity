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
// As it's a bogus warning caused by a bug in MSVC2013, it's Ok to disable it.
#pragma warning( push )
#pragma warning( disable : 4510)
#pragma warning( disable : 4610)

struct TrackPanelDrawingContext {
   wxDC &dc;
   UIHandlePtr target;
   wxMouseState lastState;
};

#pragma warning( pop ) 

#endif
