/**********************************************************************

  Audacity: A Digital Audio Editor

  Grabber.cpp

  Leland Lucius

*******************************************************************//**

\file Grabber.cpp

  Implements Grabber

*//*******************************************************************//**

\class Grabber
\brief The widget to the left of a ToolBar that allows it to be dragged
around to new positions.

*//**********************************************************************/

#ifndef __AUDACITY_WIDGETS_GRABBER__
#define __AUDACITY_WIDGETS_GRABBER__

#include "../Audacity.h"

#include "wx/defs.h"
#include "wx/dc.h"
#include "wx/event.h"
#include "wx/gdicmn.h"
#include "wx/window.h"

////////////////////////////////////////////////////////////
/// Grabber Class
////////////////////////////////////////////////////////////

// Custom events

DECLARE_EXPORTED_EVENT_TYPE(AUDACITY_DLL_API, EVT_GRABBER_CLICKED, -1)

class GrabberEvent:public wxCommandEvent
{
 public:

   GrabberEvent(wxEventType type = wxEVT_NULL,
                wxWindowID winid = 0,
                const wxPoint& pt = wxDefaultPosition)
   : wxCommandEvent(type, winid)
   {
      mPos = pt;
   }

   GrabberEvent(const GrabberEvent & event)
   : wxCommandEvent(event)
   {
      mPos = event.mPos;
   }

   // Position of event (in screen coordinates)
   const wxPoint & GetPosition() const
   {
      return mPos;
   }

   void SetPosition(const wxPoint & pos)
   {
      mPos = pos;
   }

   virtual wxEvent *Clone() const
   {
      return new GrabberEvent(*this);
   }

 protected:

   wxPoint mPos;
};

typedef void (wxEvtHandler::*GrabberEventFunction)(GrabberEvent &);

#define GrabberEventHandler(func) \
    (wxObjectEventFunction)(wxEventFunction)wxStaticCastEvent(GrabberEventFunction, &func)

#define EVT_GRABBER(id, fn) \
    DECLARE_EVENT_TABLE_ENTRY(EVT_GRABBER_CLICKED, id, wxID_ANY, \
    (wxObjectEventFunction) (wxEventFunction) (wxCommandEventFunction) \
    wxStaticCastEvent(GrabberEventFunction, & fn), (wxObject *) NULL),

// Specifies how wide the grabber will be

#define grabberWidth 10

class Grabber:public wxWindow
{

 public:

   Grabber(wxWindow *parent, wxWindowID id);
   virtual ~Grabber();

   // We don't need or want to accept focus since there's really
   // not a need to dock/float a toolbar from the keyboard.  If this
   // changes, remove this and add the necessary keyboard movement
   // handling.
   bool AcceptsFocus() const {return false;}

   void PushButton(bool state);

 protected:

   void OnLeftDown(wxMouseEvent & event);
   void OnEnter(wxMouseEvent & event);
   void OnLeave(wxMouseEvent & event);
   void OnPaint(wxPaintEvent & event);

 private:

   void DrawGrabber(wxDC & dc);
   void SendEvent(wxEventType type, const wxPoint & pos);

   bool mOver;
   bool mPressed;

 public:

   DECLARE_EVENT_TABLE();
};

#endif
