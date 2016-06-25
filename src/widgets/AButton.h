/**********************************************************************

  Audacity: A Digital Audio Editor

  AButton.h

  Dominic Mazzoni


**********************************************************************/

#ifndef __AUDACITY_BUTTON__
#define __AUDACITY_BUTTON__

#include "../MemoryX.h"
#include <vector>

#if wxUSE_ACCESSIBILITY
#include <wx/access.h>
#endif

#include <wx/image.h>
#include <wx/window.h>

#include "ImageRoll.h"

class AButton final : public wxWindow {
   friend class AButtonAx;
   class Listener;

 public:

    // Construct button, specifying images (button up, highlight, button down,
    // and disabled) for the default state
   AButton(wxWindow * parent,
           wxWindowID id,
           const wxPoint & pos,
           const wxSize & size,
           ImageRoll up,
           ImageRoll over,
           ImageRoll down,
           ImageRoll dis,
           bool toggle);

    // Construct button, specifying images (button up, highlight, button down,
    // and disabled) for the default state
   AButton(wxWindow * parent,
           wxWindowID id,
           const wxPoint & pos,
           const wxSize & size,
           wxImage up,
           wxImage over,
           wxImage down,
           wxImage dis,
           bool toggle);

   virtual ~ AButton();

   bool AcceptsFocus() const override { return s_AcceptsFocus; }
   bool AcceptsFocusFromKeyboard() const override { return true; }

   void SetFocusFromKbd() override;

   // Associate a set of four images (button up, highlight, button down,
   // disabled) with one nondefault state of the button
   void SetAlternateImages(unsigned idx,
                                   ImageRoll up,
                                   ImageRoll over,
                                   ImageRoll down,
                                   ImageRoll dis);

   // Associate a set of four images (button up, highlight, button down,
   // disabled) with one nondefault state of the button
   void SetAlternateImages(unsigned idx,
                                   wxImage up,
                                   wxImage over,
                                   wxImage down,
                                   wxImage dis);

   // Choose state of the button
   void SetAlternateIdx(unsigned idx);

   // Make the button change appearance with the modifier keys, no matter
   // where the mouse is:
   // Use state 2 when CTRL is down, else 1 when SHIFT is down, else 0
   void FollowModifierKeys();

   void SetFocusRect(wxRect & r);

   bool IsEnabled() const { return mEnabled; }
   void Disable();
   void Enable();
   void SetEnabled(bool state) {
      state ? Enable() : Disable();
   }

   void PushDown();
   void PopUp();

   void OnErase(wxEraseEvent & event);
   void OnPaint(wxPaintEvent & event);
   void OnSize(wxSizeEvent & event);
   void OnMouseEvent(wxMouseEvent & event);

   // Update the status bar message if the pointer is in the button.
   // Else do nothing.
   void UpdateStatus();

   void OnCaptureLost(wxMouseCaptureLostEvent & event);
   void OnKeyDown(wxKeyEvent & event);
   void OnSetFocus(wxFocusEvent & event);
   void OnKillFocus(wxFocusEvent & event);

   bool WasShiftDown(); // returns true if shift was held down
                                // the last time the button was clicked
   bool WasControlDown(); // returns true if control was held down
                                  // the last time the button was clicked
   bool IsDown(){ return mButtonIsDown;}

   // Double click is detected, but not automatically cleared.
   bool IsDoubleClicked() const { return mIsDoubleClicked; }
   void ClearDoubleClicked() { mIsDoubleClicked = false; }

   void SetButtonToggles( bool toggler ){ mToggle = toggler;}
   void Toggle(){ mButtonIsDown ? PopUp() : PushDown();}
   void Click();
   void SetShift(bool shift);
   void SetControl(bool control);

   enum AButtonState {
      AButtonUp,
      AButtonOver,
      AButtonDown,
      AButtonDis
   };

   AButtonState GetState();

   void UseDisabledAsDownHiliteImage(bool flag);

 private:
   static bool s_AcceptsFocus;
   struct Resetter { void operator () (bool *p) const { if(p) *p = false; } };
   using TempAllowFocus = std::unique_ptr<bool, Resetter>;

 public:
   static TempAllowFocus TemporarilyAllowFocus();

 private:

   bool HasAlternateImages(unsigned idx);

   void Init(wxWindow * parent,
             wxWindowID id,
             const wxPoint & pos,
             const wxSize & size,
             ImageRoll up,
             ImageRoll over,
             ImageRoll down,
             ImageRoll dis,
             bool toggle);

   unsigned mAlternateIdx;
   bool mToggle;   // This bool, if true, makes the button able to
                   // process events when it is in the down state, and
                   // moving to the opposite state when it is clicked.
                   // It is used for the Pause button, and possibly
                   // others.  If false, it (should) behave just like
                   // a standard button.

   bool mWasShiftDown;
   bool mWasControlDown;

   bool mCursorIsInWindow;
   bool mButtonIsFocused;
   bool mButtonIsDown;
   bool mIsClicking;
   bool mEnabled;
   bool mUseDisabledAsDownHiliteImage;
   bool mIsDoubleClicked {};

   struct ImageArr { ImageRoll mArr[4]; };
   std::vector<ImageArr> mImages;

   wxRect mFocusRect;
   bool mForceFocusRect;

   std::unique_ptr<Listener> mListener;

public:

    DECLARE_EVENT_TABLE()
};

#if wxUSE_ACCESSIBILITY

class AButtonAx final : public wxWindowAccessible
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

#endif
