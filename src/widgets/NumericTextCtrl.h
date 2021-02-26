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



#include "MemoryX.h"
#include "ComponentInterface.h"
#include "ComponentInterfaceSymbol.h"
#include <vector>
#include <wx/setup.h> // for wxUSE_* macros
#include <wx/defs.h>
#include <wx/control.h> // to inherit

#include "Internat.h"

// One event type for each type of control.  Event is raised when a control
// changes its format.  Owners of controls of the same type can listen and
// update their formats to agree.
DECLARE_EXPORTED_EVENT_TYPE(AUDACITY_DLL_API, EVT_TIMETEXTCTRL_UPDATED, -1);
DECLARE_EXPORTED_EVENT_TYPE(AUDACITY_DLL_API, EVT_FREQUENCYTEXTCTRL_UPDATED, -1);
DECLARE_EXPORTED_EVENT_TYPE(AUDACITY_DLL_API, EVT_BANDWIDTHTEXTCTRL_UPDATED,
                            -1);

/** \brief struct to hold a formatting control string and its user facing name
 * Used in an array to hold the built-in time formats that are always available
 * to the user */
struct BuiltinFormatString;

class NumericField;

class DigitInfo;

class AUDACITY_DLL_API NumericConverter /* not final */
{
public:

   enum Type {
      TIME,
      ATIME, // for Audio time control.
      FREQUENCY,
      BANDWIDTH,
   };

   struct FormatStrings {
      TranslatableString formatStr;
      // How to name the fraction of the unit; not necessary for time formats
      // or when the format string has no decimal point
      TranslatableString fraction;

      FormatStrings(
         const TranslatableString &format = {},
         const TranslatableString &fraction = {})
         : formatStr{ format }, fraction{ fraction }
      {}

      friend bool operator == ( const FormatStrings &x, const FormatStrings &y )
         { return x.formatStr == y.formatStr && x.fraction == y.fraction; }
      friend bool operator != ( const FormatStrings &x, const FormatStrings &y )
         { return !(x == y); }
   };

   static NumericFormatSymbol DefaultSelectionFormat();
   static NumericFormatSymbol TimeAndSampleFormat();
   static NumericFormatSymbol SecondsFormat();
   static NumericFormatSymbol HoursMinsSecondsFormat();
   static NumericFormatSymbol HundredthsFormat();
   static NumericFormatSymbol HertzFormat();
   
   static NumericFormatSymbol LookupFormat( Type type, const wxString& id);

   NumericConverter(Type type,
                    const NumericFormatSymbol & formatName = {},
                    double value = 0.0f,
                    double sampleRate = 1.0f /* to prevent div by 0 */);
   NumericConverter(const NumericConverter&);

   virtual ~NumericConverter();

   // ValueToControls() formats a raw value (either provided as
   // argument, or mValue, depending on the version of the function
   // called). The result is stored to mValueString.
   virtual void ValueToControls();
   virtual void ValueToControls(double rawValue, bool nearest = true);

   // Converts the stored formatted string (mValueString) back to a
   // raw value (mValue).
   virtual void ControlsToValue();

private:
   void ParseFormatString(const TranslatableString & untranslatedFormat);

public:
   void PrintDebugInfo();

   // returns true iff the format name really changed:
   bool SetFormatName(const NumericFormatSymbol & formatName);

   // returns true iff the format string really changed:
   bool SetFormatString(const FormatStrings & formatString);

   void SetSampleRate(double sampleRate);
   void SetValue(double newValue);
   void SetMinValue(double minValue);
   void ResetMinValue();
   void SetMaxValue(double maxValue);
   void ResetMaxValue();

   double GetValue();

   wxString GetString();

   int GetFormatIndex();

   int GetNumBuiltins();
   NumericFormatSymbol GetBuiltinName(const int index);
   FormatStrings GetBuiltinFormat(const int index);
   FormatStrings GetBuiltinFormat(const NumericFormatSymbol & name);

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

   FormatStrings mFormatString;

   std::vector<NumericField> mFields;
   wxString       mPrefix;
   wxString       mValueTemplate;
   wxString       mValueMask;
   // Formatted mValue, by ValueToControls().
   wxString       mValueString;

   double         mScalingFactor;
   double         mSampleRate;
   bool           mNtscDrop;

   int            mFocusedDigit;
   std::vector<DigitInfo> mDigits;

   const BuiltinFormatString *mBuiltinFormatStrings;
   const size_t mNBuiltins;
   int mDefaultNdx;
};

class AUDACITY_DLL_API NumericTextCtrl final
   : public wxControl, public NumericConverter
{
   friend class NumericTextCtrlAx;

 public:
   DECLARE_DYNAMIC_CLASS(NumericTextCtrl)

   struct Options {
      bool autoPos { true };
      bool readOnly { false };
      bool menuEnabled { true };
      bool hasInvalidValue { false };
      double invalidValue { -1.0 };
      FormatStrings format {};
      bool hasValue { false };
      double value{ -1.0 };

      Options() {}

      Options &AutoPos (bool enable) { autoPos = enable; return *this; }
      Options &ReadOnly (bool enable) { readOnly = enable; return *this; }
      Options &MenuEnabled (bool enable) { menuEnabled = enable; return *this; }
      Options &InvalidValue (bool has, double v = -1.0)
         { hasInvalidValue = has, invalidValue = v; return *this; }
      // use a custom format not in the tables:
      Options &Format (const FormatStrings &f)
         { format = f; return *this; }
      Options &Value (bool has, double v)
         { hasValue = has, value = v; return *this; }
   };

   NumericTextCtrl(wxWindow *parent, wxWindowID winid,
                   NumericConverter::Type type,
                   const NumericFormatSymbol &formatName = {},
                   double value = 0.0,
                   double sampleRate = 44100,
                   const Options &options = {},
                   const wxPoint &pos = wxDefaultPosition,
                   const wxSize &size = wxDefaultSize);

   virtual ~NumericTextCtrl();

   // Hide the inherited function that takes wxString
   void SetName( const TranslatableString &name );

   wxSize ComputeSizing(bool update = true, wxCoord digitW = 0, wxCoord digitH = 0);
   bool Layout() override;
   void Fit() override;

   void SetSampleRate(double sampleRate);
   void SetValue(double newValue);

   // returns true iff the format string really changed:
   bool SetFormatString(const FormatStrings & formatString);

   // returns true iff the format name really changed:
   bool SetFormatName(const NumericFormatSymbol & formatName);

   void SetFieldFocus(int /* digit */);

   wxSize GetDimensions() { return wxSize(mWidth + mButtonWidth, mHeight); }
   wxSize GetDigitSize() { return wxSize(mDigitBoxW, mDigitBoxH); }
   void SetDigitSize(int width, int height);
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

#endif // __AUDACITY_TIME_TEXT_CTRL__
