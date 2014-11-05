/**********************************************************************

  Audacity: A Digital Audio Editor

  AButton.cpp

  Dominic Mazzoni

*******************************************************************//**

\class AButton
\brief A wxButton with mouse-over behaviour.

  AButton is a custom button class for Audacity.  The main feature it
  supports that a wxButton does not is mouseovers.

  It uses an image for all of its states: up, over, down, and
  disabled, allowing any sort of customization you want.  Currently
  it does not support transparency effects, so the image musts be
  rectangular and opaque.

*//*******************************************************************/

#include "../Audacity.h"

#include "AButton.h"
#include "../AColor.h"

#include <wx/dcclient.h>
#include <wx/dcmemory.h>
#include <wx/dcbuffer.h>
#include <wx/image.h>

//This is needed for tooltips
#include "../Project.h"
#include <wx/tooltip.h>

BEGIN_EVENT_TABLE(AButton, wxWindow)
   EVT_MOUSE_EVENTS(AButton::OnMouseEvent)
   EVT_MOUSE_CAPTURE_LOST(AButton::OnCaptureLost)
   EVT_KEY_DOWN(AButton::OnKeyDown)
   EVT_SET_FOCUS(AButton::OnSetFocus)
   EVT_KILL_FOCUS(AButton::OnKillFocus)
   EVT_PAINT(AButton::OnPaint)
   EVT_SIZE(AButton::OnSize)
   EVT_ERASE_BACKGROUND(AButton::OnErase)
END_EVENT_TABLE()

AButton::AButton(wxWindow * parent,
                 wxWindowID id,
                 const wxPoint & pos,
                 const wxSize & size,
                 wxImage up,
                 wxImage over,
                 wxImage down,
                 wxImage dis,
                 bool toggle):
   wxWindow()
{
   Init(parent, id, pos, size,
        ImageRoll(up), ImageRoll(over),
        ImageRoll(down), ImageRoll(dis),
        toggle);
}

AButton::AButton(wxWindow * parent,
                 wxWindowID id,
                 const wxPoint & pos,
                 const wxSize & size,
                 ImageRoll up,
                 ImageRoll over,
                 ImageRoll down,
                 ImageRoll dis,
                 bool toggle):
   wxWindow()
{
   Init(parent, id, pos, size,
        up, over, down, dis,
        toggle);
}

AButton::~AButton()
{
}

void AButton::Init(wxWindow * parent,
                   wxWindowID id,
                   const wxPoint & pos,
                   const wxSize & size,
                   ImageRoll up,
                   ImageRoll over,
                   ImageRoll down,
                   ImageRoll dis,
                   bool toggle)
{
   // Bug in wxWidgets 2.8.12: by default pressing Enter on an AButton is interpreted as
   // a navigation event - move to next control. As a workaround, the style wxWANTS_CHARS
   // results in all characters being available in the OnKeyDown function below. Note
   // that OnKeyDown now has to handle navigation.
   Create(parent, id, pos, size, wxWANTS_CHARS);

   mWasShiftDown = false;
   mWasControlDown = false;
   mButtonIsDown = false;
   mIsClicking = false;
   mEnabled = true;
   mCursorIsInWindow = false;
   mToggle = toggle;
   mUseDisabledAsDownHiliteImage = false;

   mImage[0] = up;
   mImage[1] = over;
   mImage[2] = down;
   mImage[3] = dis;

   mAlternate = false;

   mButtonIsFocused = false;
   mFocusRect = GetRect().Deflate( 3, 3 );

   SetSizeHints(mImage[0].GetMinSize(),
                mImage[0].GetMaxSize());

#if wxUSE_ACCESSIBILITY
   SetName( wxT("") );
   SetAccessible(new AButtonAx(this));
#endif
}

void AButton::UseDisabledAsDownHiliteImage(bool flag)
{
   mUseDisabledAsDownHiliteImage = flag;
}

void AButton::SetAlternateImages(wxImage up,
                                 wxImage over,
                                 wxImage down,
                                 wxImage dis)
{
   mAltImage[0] = ImageRoll(up);
   mAltImage[1] = ImageRoll(over);
   mAltImage[2] = ImageRoll(down);
   mAltImage[3] = ImageRoll(dis);
}

void AButton::SetAlternateImages(ImageRoll up,
                                 ImageRoll over,
                                 ImageRoll down,
                                 ImageRoll dis)
{
   mAltImage[0] = up;
   mAltImage[1] = over;
   mAltImage[2] = down;
   mAltImage[3] = dis;
}

void AButton::SetAlternate(bool useAlternateImages)
{
   // If alternate-image-state is already correct then
   // nothing to do (saves repainting button).
   if( mAlternate == useAlternateImages )
      return;
   mAlternate = useAlternateImages;
   Refresh(false);
}

void AButton::SetFocusRect(wxRect & r)
{
   mFocusRect = r;
}

AButton::AButtonState AButton::GetState()
{
   AButtonState state;

   if (!mEnabled && (!mToggle || !mButtonIsDown))
      return AButtonDis;

   if (mCursorIsInWindow) {
      if (mToggle) {
         if (mIsClicking) {
            state = mButtonIsDown ? AButtonUp : AButtonDown;
            if (mUseDisabledAsDownHiliteImage) {
               state = mButtonIsDown ? AButtonOver : AButtonDis;
            }
         }
         else {
            state = mButtonIsDown ? AButtonDown : AButtonOver;
            if (mUseDisabledAsDownHiliteImage) {
               state = mButtonIsDown ? AButtonDis : AButtonOver;
            }
         }
      }
      else {
         if (mIsClicking) {
            state = mButtonIsDown ? AButtonOver : AButtonDown;
         }
         else {
            state = mButtonIsDown ? AButtonDown : AButtonOver;
         }
      }
   }
   else {
      if (mToggle) {
         state = mButtonIsDown ? AButtonDown : AButtonUp;
      }
      else {
         state = mButtonIsDown ? AButtonDown : AButtonUp;
      }
   }

   return state;
}

void AButton::OnPaint(wxPaintEvent & WXUNUSED(event))
{
   wxBufferedPaintDC dc(this);

   AButtonState buttonState = GetState();

   if (mAlternate)
      mAltImage[buttonState].Draw(dc, GetClientRect());
   else
      mImage[buttonState].Draw(dc, GetClientRect());

#if defined(__WXMSW__)
   if( mButtonIsFocused )
   {
      AColor::DrawFocus( dc, mFocusRect );
   }
#endif
}

void AButton::OnErase(wxEraseEvent & WXUNUSED(event))
{
   // Ignore it to prevent flashing
}

void AButton::OnSize(wxSizeEvent & WXUNUSED(event))
{
   mFocusRect = GetRect().Deflate( 3, 3 );
   Refresh(false);
}

bool AButton::HasAlternateImages()
{
   return (mAltImage[0].Ok() &&
           mAltImage[1].Ok() &&
           mAltImage[2].Ok() &&
           mAltImage[3].Ok());
}

void AButton::OnMouseEvent(wxMouseEvent & event)
{
   wxSize clientSize = GetClientSize();
   AButtonState prevState = GetState();

   if (event.Entering())
      mCursorIsInWindow = true;
   else if (event.Leaving())
      mCursorIsInWindow = false;
   else
      mCursorIsInWindow =
         (event.m_x >= 0 && event.m_y >= 0 &&
          event.m_x < clientSize.x && event.m_y < clientSize.y);

   if (HasAlternateImages() && !mButtonIsDown)
      mAlternate = event.ShiftDown();

   if (mEnabled && event.IsButton()) {
      if (event.ButtonIsDown(wxMOUSE_BTN_ANY)) {
         mIsClicking = true;
         CaptureMouse();
      }
      else if (mIsClicking) {
         mIsClicking = false;

         if (HasCapture())
            ReleaseMouse();

         if (mCursorIsInWindow && (mToggle || !mButtonIsDown)) {
            if (mToggle)
               mButtonIsDown = !mButtonIsDown;
            else
               mButtonIsDown = true;

            mWasShiftDown = event.ShiftDown();
            mWasControlDown = event.ControlDown();

            Click();
         }
      }
   }

   // Only redraw and change tooltips if the state has changed.
   AButtonState newState = GetState();

   if (newState != prevState) {
      Refresh(false);

      if (mCursorIsInWindow) {
       #if wxUSE_TOOLTIPS // Not available in wxX11
         // Display the tooltip in the status bar
         wxToolTip * pTip = this->GetToolTip();
         if( pTip ) {
            wxString tipText = pTip->GetTip();
            if (!mEnabled)
               tipText += _(" (disabled)");
            GetActiveProject()->TP_DisplayStatusMessage(tipText);
         }
       #endif
      }
      else {
         GetActiveProject()->TP_DisplayStatusMessage(wxT(""));
      }
   }
}

void AButton::OnCaptureLost(wxMouseCaptureLostEvent & WXUNUSED(event))
{
   wxMouseEvent e(wxEVT_LEFT_UP);
   e.m_x = -1;
   e.m_y = -1;
   OnMouseEvent(e);
}

// Note that OnKeyDown has to handle navigation because wxWANTS_CHARS
// flag was set - see above.
void AButton::OnKeyDown(wxKeyEvent & event)
{
   switch( event.GetKeyCode() )
   {
   case WXK_RIGHT:
      Navigate();
      break;
   case WXK_LEFT:
      Navigate(wxNavigationKeyEvent::IsBackward);
      break;
   case WXK_TAB:
      if (event.ShiftDown())
         Navigate(wxNavigationKeyEvent::IsBackward);
      else
         Navigate();
      break;
   case WXK_RETURN:
   case WXK_NUMPAD_ENTER:
      mWasShiftDown = event.ShiftDown();
      mWasControlDown = event.ControlDown();
      Click();
      break;
   default:
      event.Skip();
   }
}

void AButton::OnSetFocus(wxFocusEvent & WXUNUSED(event))
{
   mButtonIsFocused = true;
   Refresh( false );
}

void AButton::OnKillFocus(wxFocusEvent & WXUNUSED(event))
{
   mButtonIsFocused = false;
   Refresh( false );
}

bool AButton::WasShiftDown()
{
   return mWasShiftDown;
}

bool AButton::WasControlDown()
{
   return mWasControlDown;
}

void AButton::Enable()
{
   mEnabled = true;
   this->Refresh(false);
}

void AButton::Disable()
{
   mEnabled = false;
   if (GetCapture()==this)
      ReleaseMouse();
   this->Refresh(false);
}

void AButton::PushDown()
{
   mButtonIsDown = true;
   this->Refresh(false);
}

void AButton::PopUp()
{
   mButtonIsDown = false;

   if (GetCapture()==this)
      ReleaseMouse();

   this->Refresh(false);
}

void AButton::Click()
{
   wxCommandEvent event(wxEVT_COMMAND_BUTTON_CLICKED, GetId());
   event.SetEventObject(this);
   GetEventHandler()->ProcessEvent(event);
}

void AButton::SetShift(bool shift)
{
   mWasShiftDown = shift;
}

#if wxUSE_ACCESSIBILITY

AButtonAx::AButtonAx( wxWindow *window ):
   wxWindowAccessible( window )
{
}

AButtonAx::~AButtonAx()
{
}

// Performs the default action. childId is 0 (the action for this object)
// or > 0 (the action for a child).
// Return wxACC_NOT_SUPPORTED if there is no default action for this
// window (e.g. an edit control).
wxAccStatus AButtonAx::DoDefaultAction(int WXUNUSED(childId))
{
   AButton *ab = wxDynamicCast( GetWindow(), AButton );

   if(ab && ab->IsEnabled()) {
      ab->mWasShiftDown = false;
      ab->mWasControlDown = false;
      ab->Click();
   }

   return wxACC_OK;
}

// Retrieves the address of an IDispatch interface for the specified child.
// All objects must support this property.
wxAccStatus AButtonAx::GetChild( int childId, wxAccessible** child )
{
   if( childId == wxACC_SELF )
   {
      *child = this;
   }
   else
   {
      *child = NULL;
   }

   return wxACC_OK;
}

// Gets the number of children.
wxAccStatus AButtonAx::GetChildCount(int* childCount)
{
   *childCount = 0;

   return wxACC_OK;
}

// Gets the default action for this object (0) or > 0 (the action for
// a child).  Return wxACC_OK even if there is no action. actionName
// is the action, or the empty string if there is no action.  The
// retrieved string describes the action that is performed on an
// object, not what the object does as a result. For example, a
// toolbar button that prints a document has a default action of
// "Press" rather than "Prints the current document."
wxAccStatus AButtonAx::GetDefaultAction(int WXUNUSED(childId), wxString* actionName)
{
   *actionName = _( "Press" );

   return wxACC_OK;
}

// Returns the description for this object or a child.
wxAccStatus AButtonAx::GetDescription( int WXUNUSED(childId), wxString *description )
{
   description->Clear();

   return wxACC_OK;
}

// Gets the window with the keyboard focus.
// If childId is 0 and child is NULL, no object in
// this subhierarchy has the focus.
// If this object has the focus, child should be 'this'.
wxAccStatus AButtonAx::GetFocus(int* childId, wxAccessible** child)
{
   *childId = 0;
   *child = this;

   return wxACC_OK;
}

// Returns help text for this object or a child, similar to tooltip text.
wxAccStatus AButtonAx::GetHelpText( int WXUNUSED(childId), wxString *helpText )
{
#if wxUSE_TOOLTIPS // Not available in wxX11
   AButton *ab = wxDynamicCast( GetWindow(), AButton );

   wxToolTip *pTip = ab->GetToolTip();
   if( pTip )
   {
      *helpText = pTip->GetTip();
   }

   return wxACC_OK;
#else
   helpText->Clear();

   return wxACC_NOT_SUPPORTED;
#endif
}

// Returns the keyboard shortcut for this object or child.
// Return e.g. ALT+K
wxAccStatus AButtonAx::GetKeyboardShortcut( int WXUNUSED(childId), wxString *shortcut )
{
   shortcut->Clear();

   return wxACC_OK;
}

// Returns the rectangle for this object (id = 0) or a child element (id > 0).
// rect is in screen coordinates.
wxAccStatus AButtonAx::GetLocation( wxRect& rect, int WXUNUSED(elementId) )
{
   AButton *ab = wxDynamicCast( GetWindow(), AButton );

   rect = ab->GetRect();
   rect.SetPosition( ab->GetParent()->ClientToScreen( rect.GetPosition() ) );

   return wxACC_OK;
}

// Gets the name of the specified object.
wxAccStatus AButtonAx::GetName(int WXUNUSED(childId), wxString* name)
{
   AButton *ab = wxDynamicCast( GetWindow(), AButton );

   *name = ab->GetName();
   if( name->IsEmpty() )
   {
      *name = ab->GetLabel();
   }

   if( name->IsEmpty() )
   {
      *name = _("Button");
   }

   return wxACC_OK;
}

// Returns a role constant.
wxAccStatus AButtonAx::GetRole(int WXUNUSED(childId), wxAccRole* role)
{
   *role = wxROLE_SYSTEM_PUSHBUTTON;

   return wxACC_OK;
}

// Gets a variant representing the selected children
// of this object.
// Acceptable values:
// - a null variant (IsNull() returns TRUE)
// - a list variant (GetType() == wxT("list"))
// - an integer representing the selected child element,
//   or 0 if this object is selected (GetType() == wxT("long"))
// - a "void*" pointer to a wxAccessible child object
wxAccStatus AButtonAx::GetSelections( wxVariant * WXUNUSED(selections) )
{
   return wxACC_NOT_IMPLEMENTED;
}

// Returns a state constant.
wxAccStatus AButtonAx::GetState(int WXUNUSED(childId), long* state)
{
   AButton *ab = wxDynamicCast( GetWindow(), AButton );

   switch( ab->GetState() )
   {
      case AButton::AButtonDown:
         *state = wxACC_STATE_SYSTEM_PRESSED | wxACC_STATE_SYSTEM_FOCUSABLE;
      break;

      case AButton::AButtonOver:
         *state = wxACC_STATE_SYSTEM_HOTTRACKED | wxACC_STATE_SYSTEM_FOCUSABLE;
      break;

      case AButton::AButtonDis:
         *state = wxACC_STATE_SYSTEM_UNAVAILABLE;
      break;

      default:
         *state = wxACC_STATE_SYSTEM_FOCUSABLE;
      break;
   }

   // Do not use mButtonIsFocused is not set until after this method
   // is called.
   *state |= ( ab == wxWindow::FindFocus() ? wxACC_STATE_SYSTEM_FOCUSED : 0 );

   return wxACC_OK;
}

// Returns a localized string representing the value for the object
// or child.
wxAccStatus AButtonAx::GetValue(int WXUNUSED(childId), wxString* WXUNUSED(strValue))
{
   return wxACC_NOT_SUPPORTED;
}

#endif
