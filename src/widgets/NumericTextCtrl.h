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

#include <wx/defs.h>
#include <wx/dynarray.h>
#include <wx/event.h>
#include <wx/panel.h>
#include <wx/stattext.h>
#include <wx/string.h>
#include <wx/textctrl.h>

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

class NumericConverter
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

   virtual void ValueToControls();
   virtual void ValueToControls(double rawValue, bool nearest = true);
   virtual void ControlsToValue();
   virtual void ParseFormatString(const wxString & format);

   void PrintDebugInfo();
   void SetFormatName(const wxString & formatName);
   void SetFormatString(const wxString & formatString);
   void SetSampleRate(double sampleRate);
   void SetValue(double newValue);
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

   wxString       mFormatString;

   NumericFieldArray mFields;
   wxString       mPrefix;
   wxString       mValueTemplate;
   wxString       mValueMask;
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

class NumericTextCtrl: public wxControl, public NumericConverter
{
   friend class NumericTextCtrlAx;

 public:
   DECLARE_DYNAMIC_CLASS(NumericTextCtrl);

   NumericTextCtrl(NumericConverter::Type type,
                   wxWindow *parent,
                   wxWindowID id,
                   wxString formatName = wxEmptyString,
                   double value = 0.0,
                   double sampleRate = 44100,
                   const wxPoint &pos = wxDefaultPosition,
                   const wxSize &size = wxDefaultSize,
                   bool autoPos = false);

   virtual ~NumericTextCtrl();

   virtual bool Layout();
   virtual void Fit();

   void SetSampleRate(double sampleRate);
   void SetValue(double newValue);
   void SetFormatString(const wxString & formatString);
   void SetFormatName(const wxString & formatName);

   void SetFieldFocus(int digit);

   void SetReadOnly(bool readOnly = true);
   void EnableMenu(bool enable = true);

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

   void ValueToControls();
   void ControlsToValue();

   // If autoPos was enabled, focus the first non-zero digit
   void UpdateAutoFocus();

   void Updated(bool keyup = false);

private:

   bool           mMenuEnabled;
   bool           mReadOnly;

   wxBitmap      *mBackgroundBitmap;

   wxFont        *mDigitFont;
   wxFont        *mLabelFont;
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

   DECLARE_EVENT_TABLE()
};

#if wxUSE_ACCESSIBILITY

class NumericTextCtrlAx: public wxWindowAccessible
{
public:
   NumericTextCtrlAx(NumericTextCtrl * ctrl);

   virtual ~ NumericTextCtrlAx();

   // Performs the default action. childId is 0 (the action for this object)
   // or > 0 (the action for a child).
   // Return wxACC_NOT_SUPPORTED if there is no default action for this
   // window (e.g. an edit control).
   virtual wxAccStatus DoDefaultAction(int childId);

   // Retrieves the address of an IDispatch interface for the specified child.
   // All objects must support this property.
   virtual wxAccStatus GetChild(int childId, wxAccessible **child);

   // Gets the number of children.
   virtual wxAccStatus GetChildCount(int *childCount);

   // Gets the default action for this object (0) or > 0 (the action for a child).
   // Return wxACC_OK even if there is no action. actionName is the action, or the empty
   // string if there is no action.
   // The retrieved string describes the action that is performed on an object,
   // not what the object does as a result. For example, a toolbar button that prints
   // a document has a default action of "Press" rather than "Prints the current document."
   virtual wxAccStatus GetDefaultAction(int childId, wxString *actionName);

   // Returns the description for this object or a child.
   virtual wxAccStatus GetDescription(int childId, wxString *description);

   // Gets the window with the keyboard focus.
   // If childId is 0 and child is NULL, no object in
   // this subhierarchy has the focus.
   // If this object has the focus, child should be 'this'.
   virtual wxAccStatus GetFocus(int *childId, wxAccessible **child);

   // Returns help text for this object or a child, similar to tooltip text.
   virtual wxAccStatus GetHelpText(int childId, wxString *helpText);

   // Returns the keyboard shortcut for this object or child.
   // Return e.g. ALT+K
   virtual wxAccStatus GetKeyboardShortcut(int childId, wxString *shortcut);

   // Returns the rectangle for this object (id = 0) or a child element (id > 0).
   // rect is in screen coordinates.
   virtual wxAccStatus GetLocation(wxRect & rect, int elementId);

   // Gets the name of the specified object.
   virtual wxAccStatus GetName(int childId, wxString *name);

   // Returns a role constant.
   virtual wxAccStatus GetRole(int childId, wxAccRole *role);

   // Gets a variant representing the selected children
   // of this object.
   // Acceptable values:
   // - a null variant (IsNull() returns TRUE)
   // - a list variant (GetType() == wxT("list"))
   // - an integer representing the selected child element,
   //   or 0 if this object is selected (GetType() == wxT("long"))
   // - a "void*" pointer to a wxAccessible child object
   virtual wxAccStatus GetSelections(wxVariant *selections);

   // Returns a state constant.
   virtual wxAccStatus GetState(int childId, long *state);

   // Returns a localized string representing the value for the object
   // or child.
   virtual wxAccStatus GetValue(int childId, wxString *strValue);

private:
   NumericTextCtrl *mCtrl;
   int mLastField;
   int mLastDigit;
};

#endif // wxUSE_ACCESSIBILITY

#endif // __AUDACITY_TIME_TEXT_CTRL__
