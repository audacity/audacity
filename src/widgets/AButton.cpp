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


#include "AButton.h"

#include "../AColor.h"

#include <wx/setup.h> // for wxUSE_* macros

#include <wx/app.h>
#include <wx/dcbuffer.h>
#include <wx/eventfilter.h>
#include <wx/image.h>
#include <wx/timer.h>

//This is needed for tooltips
#include "Project.h"
#include "ProjectStatus.h"
#include "../ProjectWindowBase.h"
#include <wx/tooltip.h>

#if wxUSE_ACCESSIBILITY
#include "WindowAccessible.h"

class AButtonAx final : public WindowAccessible
{
public:
   AButtonAx(wxWindow * window);

   virtual ~ AButtonAx();

   // Performs the default action. childId is 0 (the action for this object)
   // or > 0 (the action for a child).
   // Return wxACC_NOT_SUPPORTED if there is no default action for this
   // window (e.g. an edit control).
   wxAccStatus DoDefaultAction(int childId) override;

   // Retrieves the address of an IDispatch interface for the specified child.
   // All objects must support this property.
   wxAccStatus GetChild(int childId, wxAccessible** child) override;

   // Gets the number of children.
   wxAccStatus GetChildCount(int* childCount) override;

   // Gets the default action for this object (0) or > 0 (the action for a child).
   // Return wxACC_OK even if there is no action. actionName is the action, or the empty
   // string if there is no action.
   // The retrieved string describes the action that is performed on an object,
   // not what the object does as a result. For example, a toolbar button that prints
   // a document has a default action of "Press" rather than "Prints the current document."
   wxAccStatus GetDefaultAction(int childId, wxString *actionName) override;

   // Returns the description for this object or a child.
   wxAccStatus GetDescription(int childId, wxString *description) override;

   // Gets the window with the keyboard focus.
   // If childId is 0 and child is NULL, no object in
   // this subhierarchy has the focus.
   // If this object has the focus, child should be 'this'.
   wxAccStatus GetFocus(int *childId, wxAccessible **child) override;

   // Returns help text for this object or a child, similar to tooltip text.
   wxAccStatus GetHelpText(int childId, wxString *helpText) override;

   // Returns the keyboard shortcut for this object or child.
   // Return e.g. ALT+K
   wxAccStatus GetKeyboardShortcut(int childId, wxString *shortcut) override;

   // Returns the rectangle for this object (id = 0) or a child element (id > 0).
   // rect is in screen coordinates.
   wxAccStatus GetLocation(wxRect& rect, int elementId) override;

   // Gets the name of the specified object.
   wxAccStatus GetName(int childId, wxString *name) override;

   // Returns a role constant.
   wxAccStatus GetRole(int childId, wxAccRole *role) override;

   // Gets a variant representing the selected children
   // of this object.
   // Acceptable values:
   // - a null variant (IsNull() returns TRUE)
   // - a list variant (GetType() == wxT("list"))
   // - an integer representing the selected child element,
   //   or 0 if this object is selected (GetType() == wxT("long"))
   // - a "void*" pointer to a wxAccessible child object
   wxAccStatus GetSelections(wxVariant *selections) override;

   // Returns a state constant.
   wxAccStatus GetState(int childId, long* state) override;

   // Returns a localized string representing the value for the object
   // or child.
   wxAccStatus GetValue(int childId, wxString* strValue) override;

};

#endif // wxUSE_ACCESSIBILITY

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

// LL:  An alternative to this might be to just use the wxEVT_KILL_FOCUS
//      or wxEVT_ACTIVATE events.
class AButton::Listener final
   : public wxEventFilter
{
public:
   Listener (AButton *button);
   ~Listener();

   int FilterEvent(wxEvent &event) override;

   void OnEvent();

private:
   AButton *mButton;
};

AButton::Listener::Listener (AButton *button)
: mButton(button)
{
   wxEvtHandler::AddFilter(this);
}

AButton::Listener::~Listener ()
{
   wxEvtHandler::RemoveFilter(this);
}

void AButton::Listener::OnEvent()
{
   if (!mButton->IsDown())
   {
      int idx = 0;
      // Ignore the event, consult key states.  One modifier key might
      // have gone up but another remained down.
      // Note that CMD (or CTRL) takes precedence over Shift if both are down
      // and alternates are defined for both
      // see also AButton::OnMouseEvent()
      if (wxGetKeyState(WXK_CONTROL) && mButton->HasAlternateImages(2))
         idx = 2;
      else if (wxGetKeyState(WXK_SHIFT) && mButton->HasAlternateImages(1))
         idx = 1;

      // Turn e.g. the "Play" button into a "Loop" button
      // or "Cut Preview" button
      mButton->SetAlternateIdx(idx);
   }
}

int AButton::Listener::FilterEvent(wxEvent &event)
{
   if (event.GetEventType() == wxEVT_KEY_DOWN ||
       event.GetEventType() == wxEVT_KEY_UP)
      OnEvent();
   else if (event.GetEventType() == wxEVT_SET_FOCUS)
      // A modal dialog might have eaten the modifier key-up with its own
      // filter before we saw it; this is adequate to fix the button image
      // when the dialog disappears.
      OnEvent();
   return Event_Skip;
}

AButton::AButton(wxWindow * parent,
                 wxWindowID id,
                 const wxPoint & pos,
                 const wxSize & size,
                 ImageRoll up,
                 ImageRoll over,
                 ImageRoll down,
                 ImageRoll overDown,
                 ImageRoll dis,
                 bool toggle):
   wxWindow()
{
   Init(parent, id, pos, size,
        up, over, down, overDown, dis,
        toggle);
}

AButton::~AButton()
{
   if(HasCapture())
      ReleaseMouse();
}

void AButton::Init(wxWindow * parent,
                   wxWindowID id,
                   const wxPoint & pos,
                   const wxSize & size,
                   ImageRoll up,
                   ImageRoll over,
                   ImageRoll down,
                   ImageRoll overDown,
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

   mImages.resize(1);
   mImages[0].mArr[0] = up;
   mImages[0].mArr[1] = over;
   mImages[0].mArr[2] = down;
   mImages[0].mArr[3] = overDown;
   mImages[0].mArr[4] = dis;

   mAlternateIdx = 0;

   mFocusRect = GetClientRect().Deflate( 3, 3 );
   mForceFocusRect = false;

   SetMinSize(mImages[0].mArr[0].GetMinSize());
   SetMaxSize(mImages[0].mArr[0].GetMaxSize());

#if wxUSE_ACCESSIBILITY
   SetName( wxT("") );
   SetAccessible(safenew AButtonAx(this));
#endif
}

void AButton::UseDisabledAsDownHiliteImage(bool flag)
{
   mUseDisabledAsDownHiliteImage = flag;
}

void AButton::SetToolTip( const TranslatableString &toolTip )
{
   wxWindow::SetToolTip( toolTip.Stripped().Translation() );
}

void AButton::SetLabel( const TranslatableString &toolTip )
{
   wxWindow::SetLabel( toolTip.Stripped().Translation() );
}

// This compensates for a but in wxWidgets 3.0.2 for mac:
// Couldn't set focus from keyboard when AcceptsFocus returns false;
// this bypasses that limitation
void AButton::SetFocusFromKbd()
{
   auto temp = TemporarilyAllowFocus();
   SetFocus();
}

void AButton::SetAlternateImages(unsigned idx,
                                 wxImage up,
                                 wxImage over,
                                 wxImage down,
                                 wxImage overDown,
                                 wxImage dis)
{
   if (1 + idx > mImages.size())
      mImages.resize(1 + idx);
   mImages[idx].mArr[0] = ImageRoll(up);
   mImages[idx].mArr[1] = ImageRoll(over);
   mImages[idx].mArr[2] = ImageRoll(down);
   mImages[idx].mArr[3] = ImageRoll(overDown);
   mImages[idx].mArr[4] = ImageRoll(dis);
}

void AButton::SetAlternateImages(unsigned idx,
                                 ImageRoll up,
                                 ImageRoll over,
                                 ImageRoll down,
                                 ImageRoll overDown,
                                 ImageRoll dis)
{
   if (1 + idx > mImages.size())
      mImages.resize(1 + idx);
   mImages[idx].mArr[0] = up;
   mImages[idx].mArr[1] = over;
   mImages[idx].mArr[2] = down;
   mImages[idx].mArr[3] = overDown;
   mImages[idx].mArr[4] = dis;
}

void AButton::SetAlternateIdx(unsigned idx)
{
   // If alternate-image-state is already correct then
   // nothing to do (saves repainting button).
   if( mAlternateIdx == idx )
      return;
   mAlternateIdx = idx;
   Refresh(false);
}

void AButton::FollowModifierKeys()
{
   if(!mListener)
      mListener = std::make_unique<Listener>(this);
}

void AButton::SetFocusRect(wxRect & r)
{
   mFocusRect = r;
   mForceFocusRect = true;
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
               state = mButtonIsDown ? AButtonOverDown : AButtonDis;
            }
         }
         else {
            state = mButtonIsDown ? AButtonOverDown : AButtonOver;
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
            state = mButtonIsDown ? AButtonOverDown : AButtonOver;
         }
      }
   }
   else {
      //if (mToggle) {
         state = mButtonIsDown ? AButtonDown : AButtonUp;
      //}
      //else {
         //state = mButtonIsDown ? AButtonDown : AButtonUp;
      //}
   }

   return state;
}

void AButton::OnPaint(wxPaintEvent & WXUNUSED(event))
{
   wxBufferedPaintDC dc(this);

   AButtonState buttonState = GetState();

   mImages[mAlternateIdx].mArr[buttonState].Draw(dc, GetClientRect());

   if( this == wxWindow::FindFocus() )
   {
      AColor::DrawFocus( dc, mFocusRect );
   }
}

void AButton::OnErase(wxEraseEvent & WXUNUSED(event))
{
   // Ignore it to prevent flashing
}

void AButton::OnSize(wxSizeEvent & WXUNUSED(event))
{
   if (!mForceFocusRect)
   {
      mFocusRect = GetClientRect().Deflate( 3, 3 );
   }
   Refresh(false);
}

bool AButton::s_AcceptsFocus{ false };

bool AButton::HasAlternateImages(unsigned idx)
{
   if (mImages.size() <= idx)
      return false;

   const ImageArr &images = mImages[idx];
   const ImageRoll (&arr)[5] = images.mArr;
   return (arr[0].Ok() &&
           arr[1].Ok() &&
           arr[2].Ok() &&
           arr[3].Ok() &&
           arr[4].Ok());
}

void AButton::OnMouseEvent(wxMouseEvent & event)
{
   wxSize clientSize = GetClientSize();
   AButtonState prevState = GetState();

   if (event.Entering()) {
      // Bug 1201:  On Mac, unsetting and re-setting the tooltip may be needed
      // to make it pop up when we want it.
      auto text = GetToolTipText();
      UnsetToolTip();
      wxWindow::SetToolTip(text);
      mCursorIsInWindow = true;
   }
   else if (event.Leaving())
      mCursorIsInWindow = false;
   else
      mCursorIsInWindow =
         (event.m_x >= 0 && event.m_y >= 0 &&
          event.m_x < clientSize.x && event.m_y < clientSize.y);

   if (mEnabled && event.IsButton()) {
      if (event.ButtonIsDown(wxMOUSE_BTN_LEFT)) {
         mIsClicking = true;
         if (event.ButtonDClick())
            mIsDoubleClicked = true;
         if( !HasCapture() )
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

      if (mCursorIsInWindow)
         UpdateStatus();
      else {
         auto pProject = FindProjectFromWindow( this );
         if (pProject)
            ProjectStatus::Get( *pProject ).Set({});
      }
   }
   else
      event.Skip();
}

void AButton::UpdateStatus()
{
   if (mCursorIsInWindow) {
#if wxUSE_TOOLTIPS // Not available in wxX11
      // Display the tooltip in the status bar
      wxToolTip * pTip = this->GetToolTip();
      if( pTip ) {
         auto tipText = Verbatim( pTip->GetTip() );
         if (!mEnabled)
            tipText.Join( XO("(disabled)"), " " );
         auto pProject = FindProjectFromWindow( this );
         if (pProject)
            ProjectStatus::Get( *pProject ).Set( tipText );
      }
#endif
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
      Navigate(wxNavigationKeyEvent::IsForward);
      break;
   case WXK_LEFT:
      Navigate(wxNavigationKeyEvent::IsBackward);
      break;
   case WXK_TAB:
      Navigate(event.ShiftDown()
               ? wxNavigationKeyEvent::IsBackward
               : wxNavigationKeyEvent::IsForward);
      break;
   case WXK_RETURN:
   case WXK_NUMPAD_ENTER:
      if( !mEnabled )
         break;
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
   Refresh( false );
}

void AButton::OnKillFocus(wxFocusEvent & WXUNUSED(event))
{
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
   bool changed = wxWindow::Enable(true);
   if ( !mEnabled ) {
      mEnabled = true;
      Refresh(false);
   }
}

void AButton::Disable()
{
   // Bug 1565: Tooltips not showing on disabled buttons.
   // The fix is to NOT tell windows that the button is disabled.
   // The button's appearance will still change to show it is disabled
   // since we control that rather than windows.
#ifndef __WXMSW__
   wxWindow::Enable(false);
#endif
   if (GetCapture()==this)
      ReleaseMouse();
   if ( mEnabled ) {
      mEnabled = false;
      Refresh(false);
   }
}

void AButton::PushDown()
{
   if (!mButtonIsDown) {
      mButtonIsDown = true;
      this->Refresh(false);
   }
}

void AButton::PopUp()
{
   if (mButtonIsDown) {
      mButtonIsDown = false;

      this->Refresh(false);
   }

   if (GetCapture()==this)
      ReleaseMouse();
}

void AButton::Click()
{
   wxCommandEvent event(wxEVT_COMMAND_BUTTON_CLICKED, GetId());
   event.SetEventObject(this);
   // Be sure to use SafelyProcessEvent so that exceptions do not propagate
   // out of DoDefaultAction
   GetEventHandler()->SafelyProcessEvent(event);
}

void AButton::SetShift(bool shift)
{
   mWasShiftDown = shift;
}

void AButton::SetControl(bool control)
{
   mWasControlDown = control;
}

auto AButton::TemporarilyAllowFocus() -> TempAllowFocus {
   s_AcceptsFocus = true;
   return TempAllowFocus{ &s_AcceptsFocus };
}

#if wxUSE_ACCESSIBILITY

AButtonAx::AButtonAx( wxWindow *window ):
   WindowAccessible( window )
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
   description->clear();

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
   helpText->clear();

   return wxACC_NOT_SUPPORTED;
#endif
}

// Returns the keyboard shortcut for this object or child.
// Return e.g. ALT+K
wxAccStatus AButtonAx::GetKeyboardShortcut( int WXUNUSED(childId), wxString *shortcut )
{
   shortcut->clear();

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
   if( name->empty() )
   {
      *name = ab->GetLabel();
   }

   if( name->empty() )
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

      case AButton::AButtonOverDown:
         *state = wxACC_STATE_SYSTEM_HOTTRACKED | wxACC_STATE_SYSTEM_PRESSED |
            wxACC_STATE_SYSTEM_FOCUSABLE;
      break;

      case AButton::AButtonDis:
         *state = wxACC_STATE_SYSTEM_UNAVAILABLE;
      break;

      default:
         *state = wxACC_STATE_SYSTEM_FOCUSABLE;
      break;
   }

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
