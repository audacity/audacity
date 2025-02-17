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
around to NEW positions.

*//**********************************************************************/

#include "Grabber.h"

#include <wx/defs.h>
#include <wx/dcclient.h>
#include <wx/window.h>

#include "AColor.h"
#include "AllThemeResources.h"
#include "Internat.h"
#include "Theme.h"

////////////////////////////////////////////////////////////
/// Methods for Grabber
////////////////////////////////////////////////////////////

DEFINE_EVENT_TYPE(EVT_GRABBER_CLICKED)

BEGIN_EVENT_TABLE(Grabber, wxWindow)
EVT_ENTER_WINDOW(Grabber::OnEnter)
EVT_LEAVE_WINDOW(Grabber::OnLeave)
EVT_LEFT_DOWN(Grabber::OnLeftDown)
EVT_LEFT_UP(Grabber::OnLeftUp)
EVT_ERASE_BACKGROUND(Grabber::OnErase)
EVT_PAINT(Grabber::OnPaint)
EVT_KEY_DOWN(Grabber::OnKeyDown)
END_EVENT_TABLE()

//
// Constructor
//
Grabber::Grabber(wxWindow* parent, Identifier id)
    : wxWindow(parent,
               wxID_ANY,
               wxDefaultPosition,
               wxSize(grabberWidth, 27),
               wxFULL_REPAINT_ON_RESIZE)
    , mIdentifier{id}
{
    mOver = false;
    mPressed = false;
    mAsSpacer = false;
    SetBackgroundColour(theTheme.Colour(clrMedium));

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
void Grabber::SendEvent(wxEventType type, const wxPoint& pos, bool escaping)
{
    wxWindow* parent = GetParent();

    // Initialize event and convert mouse coordinates to screen space
    GrabberEvent e(type, mIdentifier, parent->ClientToScreen(pos), escaping);

    // Set the object of our desire
    e.SetEventObject(parent);

    // Queue the event
    parent->GetEventHandler()->AddPendingEvent(e);
}

void Grabber::SetAsSpacer(bool bIsSpacer)
{
    if (mAsSpacer != bIsSpacer) {
        // HACK: Use a wider rectangle to also cover one pixel of space just to the right.
        wxSize siz = GetSize();
        siz.IncBy(bIsSpacer ? 1 : -1, 0);
        SetSize(siz);
    }
    mAsSpacer = bIsSpacer;
}

void Grabber::SetToolTip(const TranslatableString& toolTip)
{
    wxWindow::SetToolTip(toolTip.Stripped().Translation());
}

//
// Draw the grabber
//
void Grabber::DrawGrabber(wxDC& dc)
{
    wxRect r = GetRect();
    // PaintDC positions are relative to the grabber, not the parent window.
    // So use 0,0 as origin for draw, so that the grabber draws right if
    // positioned in its parent at some non zero position.
    r.SetPosition(wxPoint(0, 0));
    int y, left, right, top, bottom;

    AColor::Medium(&dc, mOver);
    dc.DrawRectangle(r);

    // HACK: We used a wider rectangle to also cover one pixel of space just to the right.
    if (mAsSpacer) {
        r.width -= 1;
    }

    // No bumps in a spacer grabber.
    if (mAsSpacer) {
        return;
    }
    // Calculate the bump rectangle
    r.Deflate(2, 2);
    if ((r.GetHeight() % 4) < 2) {
        r.Offset(0, 1);
    }

    // 2-bar toolbars and larger get padding
    int padding = r.GetHeight() > 32 ? 22 : 6;

    // Cache
    left = r.GetLeft();
    right = r.GetRight();
    top = r.GetTop();
    bottom = r.GetBottom();

    // Draw the bumps
    if (mPressed) {
        AColor::Light(&dc, false);
    } else {
        dc.SetPen(wxPen(theTheme.Colour(clrGrabber), 1, wxPENSTYLE_SOLID));
    }

    for (y = top + padding; y < bottom - padding; y += 5) {
        dc.DrawRectangle(left, y, 2, 2);
        dc.DrawRectangle(right, y, 2, 2);
    }
}

//
// Change the button state
//
void Grabber::PushButton(bool state)
{
    if (mAsSpacer) {
        return;
    }
    if (!state) {
        mPressed = state;
    }
    wxRect r = GetRect();
    mOver = r.Contains(ScreenToClient(wxGetMousePosition()));

    // Redraw button
    mPressed = state;
    Refresh(false);
}

//
// Handle left button down events
//
void Grabber::OnLeftDown(wxMouseEvent& event)
{
    // Button should be drawn pushed
    PushButton(true);

    // Notify parent
    SendEvent(EVT_GRABBER_CLICKED, event.GetPosition(), false);

    event.Skip();
}

//
// Handle left button up events
//
void Grabber::OnLeftUp(wxMouseEvent& event)
{
    // Normally, "left up" events are handled by the ToolManager::OnMouse() method
    // but, if the user double clicks a grabber, the "left up" event will come here
    // instead, so just "unpush" the button.
    PushButton(false);

    event.Skip();
}

//
// Handle mouse enter events
//
void Grabber::OnEnter(wxMouseEvent& WXUNUSED(event))
{
#if defined(__WXMAC__)
    // Bug 2416:  On Mac, we can get Enter events from grabbers other
    // than the one being dragged. So, ignore Enter events if another
    // window has captured the mouse.
    if (wxWindow::GetCapture() != nullptr) {
        return;
    }
#endif

    // Bug 1201:  On Mac, unsetting and re-setting the tooltip may be needed
    // to make it pop up when we want it.
    const auto text = GetToolTipText();
    UnsetToolTip();
    wxWindow::SetToolTip(text);

    if (mAsSpacer) {
        return;
    }

    // Redraw highlighted
    mOver = true;
    Refresh(false);
}

//
// Handle mouse leave events
//
void Grabber::OnLeave(wxMouseEvent& WXUNUSED(event))
{
#if defined(__WXMAC__)
    // Bug 2416:  On Mac, we can get Leave events from grabbers other
    // than the one being dragged. So, ignore Leave events if another
    // window has captured the mouse.
    if (wxWindow::GetCapture() != nullptr) {
        return;
    }
#endif

    if (!GetCapture()) {
        // Redraw plain
        mOver = false;
        Refresh(false);
    }
}

void Grabber::OnErase(wxEraseEvent& WXUNUSED(event))
{
    // Ignore it to prevent flashing
}

//
// Handle the paint events
//
void Grabber::OnPaint(wxPaintEvent& WXUNUSED(event))
{
    wxPaintDC dc(this);

    // Redraw the grabber
    DrawGrabber(dc);
}

void Grabber::OnKeyDown(wxKeyEvent& event)
{
    event.Skip();

    if (event.GetKeyCode() == WXK_ESCAPE) {
        // We must not only skip this key event, but propagate it up the window
        // hierarchy, so that ToolFrame detects it too.
        event.ResumePropagation(wxEVENT_PROPAGATE_MAX);
        SendEvent(EVT_GRABBER_CLICKED, wxPoint { -1, -1 }, true);
    }
}

// Piggy back in same source file as Grabber.
// Audacity Flicker-free StaticBitmap.
BEGIN_EVENT_TABLE(AStaticBitmap, wxStaticBitmap)
EVT_ERASE_BACKGROUND(AStaticBitmap::OnErase)
END_EVENT_TABLE()
