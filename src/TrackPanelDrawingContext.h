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

struct TrackPanelDrawingContext {
   wxDC &dc;
   UIHandlePtr target;
   wxMouseState lastState;
};

#endif
