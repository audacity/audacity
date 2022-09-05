/**********************************************************************

  Audacity: A Digital Audio Editor

  AButton.h

  Dominic Mazzoni


**********************************************************************/

#ifndef __AUDACITY_BUTTON__
#define __AUDACITY_BUTTON__

#include <vector>
#include <array>
#include <memory>

#include <wx/setup.h> // for wxUSE_* macros
#include <wx/window.h> // to inherit
#include <wx/image.h>

class wxImage;
class TranslatableString;

class AUDACITY_DLL_API AButton : public wxWindow {
   friend class AButtonAx;
   class Listener;

public:

   enum Type
   {
      TextButton,
      ImageButton,
      FrameButton
   };

   AButton(wxWindow* parent = nullptr,
      wxWindowID id = wxID_ANY,
      const wxPoint& pos = wxDefaultPosition,
      const wxSize& size = wxDefaultSize,
      bool toggle = false);

    // Construct button, specifying images (button up, highlight, button down,
    // and disabled) for the default state
   AButton(wxWindow * parent,
           wxWindowID id,
           const wxPoint & pos,
           const wxSize & size,
           const wxImage& up,
           const wxImage& over,
           const wxImage& down,
           const wxImage& overDown,
           const wxImage& dis,
           bool toggle);

   virtual ~ AButton();

   void SetButtonType(Type type);

   // hide the inherited function that takes naked wxString:
   void SetToolTip(const TranslatableString &toolTip);
   
   // hide the inherited function that takes naked wxString:
   void SetLabel(const TranslatableString &label);

   bool AcceptsFocus() const override { return s_AcceptsFocus; }
   bool AcceptsFocusFromKeyboard() const override { return mEnabled; }

   void SetFocusFromKbd() override;

   //Same as SetAlternateImages(0, ...);
   void SetImages(const wxImage& up,
                    const wxImage& over,
                    const wxImage& down,
                    const wxImage& overDown,
                    const wxImage& dis);

   // Associate a set of four images (button up, highlight, button down,
   // disabled) with one nondefault state of the button
   void SetAlternateImages(unsigned idx,
                                   const wxImage& up,
                                   const wxImage& over,
                                   const wxImage& down,
                                   const wxImage& overDown,
                                   const wxImage& dis);

   void SetIcon(const wxImage& icon);

   // Choose state of the button
   void SetAlternateIdx(unsigned idx);

   // Make the button change appearance with the modifier keys, no matter
   // where the mouse is:
   // Use state 2 when CTRL is down, else 1 when SHIFT is down, else 0
   void FollowModifierKeys();

   void SetFocusRect(const wxRect & r);

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
   bool IsToggle() const noexcept;
   // When click is over and mouse has moved away, a normal button
   // should pop up.
   void InteractionOver(){ if( !mToggle ) PopUp();}
   void Toggle(){ mButtonIsDown ? PopUp() : PushDown();}
   void Click();
   void SetShift(bool shift);
   void SetControl(bool control);

   wxSize DoGetBestClientSize() const override;

   enum AButtonState {
      AButtonUp,
      AButtonOver,
      AButtonDown,
      AButtonOverDown,
      AButtonDis,

      AButtonStateCount
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

   bool HasAlternateImages(unsigned idx) const;

   void Init(wxWindow* parent, wxWindowID id, const wxPoint& pos, const wxSize& size, bool toggle);

   unsigned mAlternateIdx{0};
   bool mToggle{false};   // This bool, if true, makes the button able to
                   // process events when it is in the down state, and
                   // moving to the opposite state when it is clicked.
                   // It is used for the Pause button, and possibly
                   // others.  If false, it (should) behave just like
                   // a standard button.

   bool mWasShiftDown {false};
   bool mWasControlDown {false};

   bool mCursorIsInWindow{false};
   bool mButtonIsDown{false};
   bool mIsClicking{false};
   bool mEnabled{true};
   bool mUseDisabledAsDownHiliteImage{false};
   bool mIsDoubleClicked{false};

   wxImage mIcon;
   std::vector<std::array<wxImage, AButtonStateCount>> mImages;

   wxRect mFocusRect;
   bool mForceFocusRect{false};

   std::unique_ptr<Listener> mListener;

   Type mType{ImageButton};

public:

    DECLARE_EVENT_TABLE()
};

#endif
