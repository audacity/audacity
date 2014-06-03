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

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/dcclient.h>
#include <wx/event.h>
#include <wx/intl.h>
#include <wx/window.h>

#include "Grabber.h"
#include "../Experimental.h"

#include "../AColor.h"

////////////////////////////////////////////////////////////
/// Methods for Grabber
////////////////////////////////////////////////////////////

DEFINE_EVENT_TYPE(EVT_GRABBER_CLICKED)

BEGIN_EVENT_TABLE(Grabber, wxWindow)
   EVT_ENTER_WINDOW(Grabber::OnEnter)
   EVT_LEAVE_WINDOW(Grabber::OnLeave)
   EVT_LEFT_DOWN(Grabber::OnLeftDown)
   EVT_PAINT(Grabber::OnPaint)
END_EVENT_TABLE()

//
// Constructor
//
Grabber::Grabber(wxWindow * parent, wxWindowID id)
: wxWindow(parent,
           id,
           wxDefaultPosition,
           wxSize(grabberWidth, 27),
           wxFULL_REPAINT_ON_RESIZE)
{
   mOver = false;
   mPressed = false;

   /* i18n-hint: A 'Grabber' is a region you can click and drag on
   It's used to drag a track around (when in multi-tool mode) rather
   than requiring that you use the drag tool.  It's shown as a series
   of horizontal bumps */

   SetLabel(_("Grabber"));
   SetName(_("Grabber"));
}

//
// Destructor
//
Grabber::~Grabber()
{
}

//
// Queue a drag event
//
void Grabber::SendEvent(wxEventType type, const wxPoint & pos)
{
   wxWindow *parent = GetParent();

   // Initialize event and convert mouse coordinates to screen space
   GrabberEvent e(type, GetId(), parent->ClientToScreen(pos));

   // Set the object of our desire
   e.SetEventObject(parent);

   // Queue the event
   parent->AddPendingEvent(e);
}

//
// Draw the grabber
//
void Grabber::DrawGrabber( wxDC & dc )
{
   wxRect r = GetRect();
   int y, left, right, top, bottom;

#ifndef EXPERIMENTAL_THEMING
   AColor::Medium(&dc, mOver );
   dc.DrawRectangle(r);
#else
   // Paint the background
   if( mOver )
   {
   AColor::Medium(&dc, mOver );
   dc.DrawRectangle(r);
   }
   else
   {
      // Get colour from parent...
      // when parent colour changes, child colour might not!
      wxBrush brush( GetParent()->GetBackgroundColour() );
      dc.SetBrush( brush );
      dc.DrawRectangle(r);
   }
#endif

#ifndef __WXMAC__

   // Add a box
   r.width -= 1;
   r.height -= 1;
   AColor::Bevel(dc, !mPressed, r);
   r.width += 1;
   r.height += 1;

#endif

   // Calculate the bump rectangle
   r.Deflate(3, 3);
   if ((r.GetHeight() % 4) < 2) {
      r.Offset(0, 1);
   }

   // Cache
   left = r.GetLeft();
   right = r.GetRight();
   top = r.GetTop();
   bottom = r.GetBottom();

   // Draw the raised bumps
   if (mPressed) {
      AColor::Dark(&dc, false);
   }
   else {
      AColor::Light(&dc, false);
   }

   for (y = top; y < bottom; y += 4) {
      AColor::Line(dc, left, y, right, y);
   }

   // Draw the pushed bumps
   if (mPressed) {
      AColor::Light(&dc, false);
   }
   else {
      AColor::Dark(&dc, false);
   }

   for (y = top + 1; y <= bottom; y += 4) {
      AColor::Line(dc, left, y, right, y);
   }
}

//
// Change the button state
//
void Grabber::PushButton(bool state )
{
   wxRect r = GetRect();
   mOver = r.Contains(ScreenToClient(wxGetMousePosition()));

   // Redraw button
   mPressed = state;
   Refresh(false);
}

//
// Handle left button down events
//
void Grabber::OnLeftDown(wxMouseEvent & event)
{
   // Button should be drawn pushed
   PushButton(true);

   // Notify parent
   SendEvent(EVT_GRABBER_CLICKED, event.GetPosition());

   event.Skip();
}

//
// Handle mouse enter events
//
void Grabber::OnEnter(wxMouseEvent & WXUNUSED(event))
{
   // Redraw highlighted
   mOver = true;
   Refresh(false);
}

//
// Handle mouse leave events
//
void Grabber::OnLeave(wxMouseEvent & WXUNUSED(event))
{
   if (!GetCapture()) {
      // Redraw plain
      mOver = false;
      Refresh(false);
   }
}

//
// Handle the paint events
//
void Grabber::OnPaint(wxPaintEvent & WXUNUSED(event))
{
   wxPaintDC dc(this);

   // Redraw the grabber
   DrawGrabber(dc);
}
