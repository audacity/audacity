/**********************************************************************

  Audacity: A Digital Audio Editor

  NumericConverter.h

  Dominic Mazzoni

  See NumericConverter.cpp for documentation on how to use the
  format string to specify how a NumericTextCtrl's fields are
  laid out.

  Paul Licameli split from NumericTextCtrl.h

**********************************************************************/
#ifndef __AUDACITY_NUMERIC_CONVERTER__
#define __AUDACITY_NUMERIC_CONVERTER__

#include "ComponentInterfaceSymbol.h"
#include "TranslatableString.h"

/** \brief struct to hold a formatting control string and its user facing name
 * Used in an array to hold the built-in time formats that are always available
 * to the user */
struct BuiltinFormatString;

struct NumericField
{
public:
   NumericField(bool _frac, int _base, int _range, bool _zeropad)
   {
      frac = _frac;
      base = _base;
      range = _range;
      zeropad = _zeropad;
      digits = 0;
   }
   NumericField( const NumericField & ) = default;
   NumericField &operator = ( const NumericField & ) = default;
   //NumericField( NumericField && ) = default;
   //NumericField &operator = ( NumericField && ) = default;
   void CreateDigitFormatStr();
   bool frac; // is it a fractional field
   int base;  // divide by this (multiply, after decimal point)
   int range; // then take modulo this
   int digits;
   int pos;   // Index of this field in the ValueString
   int fieldX; // x-position of the field on-screen
   int fieldW; // width of the field on-screen
   int labelX; // x-position of the label on-screen
   bool zeropad;
   wxString label;
   wxString formatStr;
   wxString str;
};

struct DigitInfo
{
   DigitInfo(int _field, int _index, int _pos)
   {
      field = _field;
      index = _index;
      pos = _pos;
   }
   int field; // Which field
   int index; // Index of this digit within the field
   int pos;   // Position in the ValueString
};

class NUMERIC_FORMATS_API NumericConverter /* not final */
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
#endif // __AUDACITY_NUMERIC_CONVERTER__
