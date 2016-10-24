/**********************************************************************

  Audacity: A Digital Audio Editor

  NumericTextCtrl.h

  Dominic Mazzoni

  See NumericTextCtrl.cpp for documentation on how to use the
  format string to specify how a NumericTextCtrl's fields are
  laid out.

**********************************************************************/

#ifndef __AUDACITY_TIME_TEXT_CTRL__
#define __AUDACITY_TIME_TEXT_CTRL__

#include "../MemoryX.h"
#include <wx/defs.h>
#include <wx/dynarray.h>
#include <wx/event.h>
#include <wx/panel.h>
#include <wx/stattext.h>
#include <wx/string.h>
#include <wx/textctrl.h>

#include "../Audacity.h"

#if wxUSE_ACCESSIBILITY
#include <wx/access.h>
#endif

// One event type for each type of control.  Event is raised when a control
// changes its format.  Owners of controls of the same type can listen and
// update their formats to agree.
DECLARE_EXPORTED_EVENT_TYPE(AUDACITY_DLL_API, EVT_TIMETEXTCTRL_UPDATED, -1);
DECLARE_EXPORTED_EVENT_TYPE(AUDACITY_DLL_API, EVT_FREQUENCYTEXTCTRL_UPDATED, -1);
DECLARE_EXPORTED_EVENT_TYPE(AUDACITY_DLL_API, EVT_BANDWIDTHTEXTCTRL_UPDATED,
                            -1);

/** \brief struct to hold a formatting control string and it's user facing name
 * Used in an array to hold the built-in time formats that are always available
 * to the user */
struct BuiltinFormatString;

class NumericField;
WX_DECLARE_OBJARRAY(NumericField, NumericFieldArray);

class DigitInfo;
WX_DECLARE_OBJARRAY(DigitInfo, DigitInfoArray);

class NumericConverter /* not final */
{
public:

   enum Type {
      TIME,
      FREQUENCY,
      BANDWIDTH,
   };

   NumericConverter(Type type,
                    const wxString & formatName = wxEmptyString,
                    double value = 0.0f,
                    double sampleRate = 1.0f /* to prevent div by 0 */);

   virtual ~NumericConverter();

   // ValueToControls() formats a raw value (either provided as
   // argument, or mValue, depending on the version of the function
   // called). The result is stored to mValueString.
   virtual void ValueToControls();
   virtual void ValueToControls(double rawValue, bool nearest = true);

   // Converts the stored formatted string (mValueString) back to a
   // raw value (mValue).
   virtual void ControlsToValue();

   virtual void ParseFormatString(const wxString & format);

   void PrintDebugInfo();
   void SetFormatName(const wxString & formatName);
   void SetFormatString(const wxString & formatString);
   void SetSampleRate(double sampleRate);
   void SetValue(double newValue);
   void SetMinValue(double minValue);
   void ResetMinValue();
   void SetMaxValue(double maxValue);
   void ResetMaxValue();

   double GetValue();

   wxString GetString();

   wxString GetFormatString();
   int GetFormatIndex();

   int GetNumBuiltins();
   wxString GetBuiltinName(const int index);
   wxString GetBuiltinFormat(const int index);
   wxString GetBuiltinFormat(const wxString & name);

   // Adjust the value by the number "steps" in the active format.
   // Increment if "dir" is 1, decrement if "dir" is -1.
   void Adjust(int steps, int dir);

   void Increment();
   void Decrement();

protected:
   Type           mType;

   double         mValue;

   double         mMinValue;
   double         mMaxValue;
   double         mInvalidValue;

   wxString       mFormatString;

   NumericFieldArray mFields;
   wxString       mPrefix;
   wxString       mValueTemplate;
   wxString       mValueMask;
   // Formatted mValue, by ValueToControls().
   wxString       mValueString;

   double         mScalingFactor;
   double         mSampleRate;
   bool           mNtscDrop;

   int            mFocusedDigit;
   DigitInfoArray mDigits;

   const BuiltinFormatString *mBuiltinFormatStrings;
   int mNBuiltins;
   int mDefaultNdx;
};

class NumericTextCtrl final : public wxControl, public NumericConverter
{
   friend class NumericTextCtrlAx;

 public:
   DECLARE_DYNAMIC_CLASS(NumericTextCtrl)

   NumericTextCtrl(NumericConverter::Type type,
                   wxWindow *parent,
                   wxWindowID id,
                   const wxString &formatName = wxEmptyString,
                   double value = 0.0,
                   double sampleRate = 44100,
                   const wxPoint &pos = wxDefaultPosition,
                   const wxSize &size = wxDefaultSize,
                   bool autoPos = false);

   virtual ~NumericTextCtrl();

   bool Layout() override;
   void Fit() override;

   void SetSampleRate(double sampleRate);
   void SetValue(double newValue);
   void SetFormatString(const wxString & formatString);
   void SetFormatName(const wxString & formatName);

   void SetFieldFocus(int /* digit */);

   void SetReadOnly(bool readOnly = true);
   void EnableMenu(bool enable = true);

   // The text control permits typing DELETE to make the value invalid only if this
   // function has previously been called.
   // Maybe you want something other than the default of -1 to indicate the invalid value
   // this control returns to the program, so you can specify.
   void SetInvalidValue(double invalidValue);

   int GetFocusedField() { return mLastField; }
   int GetFocusedDigit() { return mFocusedDigit; }

private:

   void OnCaptureKey(wxCommandEvent &event);
   void OnKeyDown(wxKeyEvent &event);
   void OnKeyUp(wxKeyEvent &event);
   void OnMouse(wxMouseEvent &event);
   void OnErase(wxEraseEvent &event);
   void OnPaint(wxPaintEvent &event);
   void OnFocus(wxFocusEvent &event);
   void OnContext(wxContextMenuEvent &event);

   // Formats mValue into mValueString, using the method of the base class.
   // Triggers a refresh of the wx window only when the value actually
   // changed since last time a refresh was triggered.
   void ValueToControls() override;
   void ControlsToValue() override;

   // If autoPos was enabled, focus the first non-zero digit
   void UpdateAutoFocus();

   void Updated(bool keyup = false);

private:

   bool           mMenuEnabled;
   bool           mReadOnly;

   std::unique_ptr<wxBitmap> mBackgroundBitmap;

   std::unique_ptr<wxFont> mDigitFont, mLabelFont;
   int            mDigitBoxW;
   int            mDigitBoxH;
   int            mDigitW;
   int            mDigitH;
   int            mBorderLeft;
   int            mBorderTop;
   int            mBorderRight;
   int            mBorderBottom;
   int            mWidth;
   int            mHeight;
   int            mButtonWidth;

   int            mLastField;

   // If true, the focus will be set to the first non-zero digit
   bool           mAutoPos;

   // Keeps track of extra fractional scrollwheel steps
   double         mScrollRemainder;

   NumericConverter::Type mType;

   bool           mAllowInvalidValue;

   DECLARE_EVENT_TABLE()
};

#if wxUSE_ACCESSIBILITY

class NumericTextCtrlAx final : public wxWindowAccessible
{
public:
   NumericTextCtrlAx(NumericTextCtrl * ctrl);

   virtual ~ NumericTextCtrlAx();

   // Performs the default action. childId is 0 (the action for this object)
   // or > 0 (the action for a child).
   // Return wxACC_NOT_SUPPORTED if there is no default action for this
   // window (e.g. an edit control).
   wxAccStatus DoDefaultAction(int childId) override;

   // Retrieves the address of an IDispatch interface for the specified child.
   // All objects must support this property.
   wxAccStatus GetChild(int childId, wxAccessible **child) override;

   // Gets the number of children.
   wxAccStatus GetChildCount(int *childCount) override;

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
   wxAccStatus GetLocation(wxRect & rect, int elementId) override;

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
   wxAccStatus GetState(int childId, long *state) override;

   // Returns a localized string representing the value for the object
   // or child.
   wxAccStatus GetValue(int childId, wxString *strValue) override;

private:
   NumericTextCtrl *mCtrl;
   int mLastField;
   int mLastDigit;
};

#endif // wxUSE_ACCESSIBILITY

#endif // __AUDACITY_TIME_TEXT_CTRL__
