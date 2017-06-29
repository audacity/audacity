/**********************************************************************

Audacity: A Digital Audio Editor

TrackPanelCell.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_TRACK_PANEL_CELL__
#define __AUDACITY_TRACK_PANEL_CELL__

#include "MemoryX.h"

class AudacityProject;
struct TrackPanelMouseEvent;
struct TrackPanelMouseState;
class ViewInfo;
class wxKeyEvent;
class wxPoint;
class wxRect;
class wxWindow;

class UIHandle;
using UIHandlePtr = std::shared_ptr<UIHandle>;

#include <vector>

// Abstract base class defining TrackPanel's access to specialist classes that
// implement drawing and user interactions
class AUDACITY_DLL_API TrackPanelCell /* not final */
{
public:
   virtual ~TrackPanelCell () = 0;

   // Return pointers to objects that can be queried for a status
   // bar message and cursor appropriate to the point, and that dispatch
   // mouse button events.
   // The button-down state passed to the function is as it will be at click
   // time -- not necessarily as it is now.
   virtual std::vector<UIHandlePtr> HitTest
      (const TrackPanelMouseState &state,
       const AudacityProject *pProject) = 0;

   // Return value is a bitwise OR of RefreshCode values
   // Include Cancelled in the flags to indicate that the event is not handled.
   // Default does only that.
   virtual unsigned HandleWheelRotation
      (const TrackPanelMouseEvent &event,
       AudacityProject *pProject);

   // The pPosition parameter indicates mouse position but may be NULL
   // Return value is a bitwise OR of RefreshCode values
   // Default implementation does nothing
   virtual unsigned DoContextMenu
      (const wxRect &rect,
       wxWindow *pParent, wxPoint *pPosition);

   // Return value is a bitwise OR of RefreshCode values
   // Default skips the event and does nothing
   virtual unsigned CaptureKey
      (wxKeyEvent &event, ViewInfo &viewInfo, wxWindow *pParent);

   // Return value is a bitwise OR of RefreshCode values
   // Default skips the event and does nothing
   virtual unsigned KeyDown
      (wxKeyEvent & event, ViewInfo &viewInfo, wxWindow *pParent);

   // Return value is a bitwise OR of RefreshCode values
   // Default skips the event and does nothing
   virtual unsigned KeyUp
      (wxKeyEvent & event, ViewInfo &viewInfo, wxWindow *pParent);

   // Return value is a bitwise OR of RefreshCode values
   // Default skips the event and does nothing
   virtual unsigned Char
      (wxKeyEvent & event, ViewInfo &viewInfo, wxWindow *pParent);
};

#endif
