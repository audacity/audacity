/////////////////////////////////////////////////////////////////////////////
// Name:        wx/valnum.h
// Purpose:     Numeric validator classes.
// Author:      Vadim Zeitlin based on the submission of Fulvio Senore
// Created:     2010-11-06
// Copyright:   (c) 2010 wxWidgets team
//              (c) 2011 Vadim Zeitlin <vadim@wxwidgets.org>
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

#ifndef _WIDGETS_VALNUM_H_
#define _WIDGETS_VALNUM_H_

#include <memory>
#include <wx/setup.h> // for wxUSE_* macros
#include <wx/defs.h>

#if wxUSE_VALIDATORS

#include <wx/textctrl.h> // complete type needed in template function
#include <wx/validate.h> // to inherit

#include <limits>

class TranslatableString;

// Bit masks used for numeric validator styles.
enum class NumValidatorStyle : int
{
    DEFAULT               = 0x0,
    THOUSANDS_SEPARATOR   = 0x1,
    ZERO_AS_BLANK         = 0x2,
    NO_TRAILING_ZEROES    = 0x4,
    ONE_TRAILING_ZERO     = 0x8,
    TWO_TRAILING_ZEROES   = 0x10,
    THREE_TRAILING_ZEROES = 0x20
};

inline NumValidatorStyle operator | (NumValidatorStyle x, NumValidatorStyle y)
{ return NumValidatorStyle( int(x) | int(y) ); }

inline int operator & (NumValidatorStyle x, NumValidatorStyle y)
{ return int(x) & int(y); }

// ----------------------------------------------------------------------------
// Base class for all numeric validators.
// ----------------------------------------------------------------------------

class SHUTTLEGUI_API NumValidatorBase /* not final */ : public wxValidator
{
public:
    // Change the validator style. Usually it's specified during construction.
    void SetStyle(NumValidatorStyle style) { m_style = style; }

    // Called when the value in the window must be validated.
    // This function can pop up an error message.
    bool Validate(wxWindow * parent) override;

protected:
    NumValidatorBase(NumValidatorStyle style)
    {
        m_style = style;
        m_minSet = false;
        m_maxSet = false;
    }

    NumValidatorBase(const NumValidatorBase& other) : wxValidator()
    {
        m_style = other.m_style;
        m_minSet = other.m_minSet;
        m_maxSet = other.m_maxSet;
    }

    bool HasFlag(NumValidatorStyle style) const
    {
        return (m_style & style) != 0;
    }

    // Get the text entry of the associated control. Normally shouldn't ever
    // return NULL (and will assert if it does return it) but the caller should
    // still test the return value for safety.
    wxTextEntry *GetTextEntry() const;

    // Convert NumValidatorStyle::THOUSANDS_SEPARATOR and NumValidatorStyle::NO_TRAILING_ZEROES
    // bits of our style to the corresponding NumberFormatter::Style values.
    int GetFormatFlags() const;

    // Return true if pressing a '-' key is acceptable for the current control
    // contents and insertion point. This is meant to be called from the
    // derived class IsCharOk() implementation.
    bool IsMinusOk(const wxString& val, int pos) const;

    // Return the string which would result from inserting the given character
    // at the specified position.
    wxString GetValueAfterInsertingChar(const wxString &valArg, int pos, wxChar ch) const
    {
        wxString val(valArg);
        val.insert(pos, ch);
        return val;
    }

    bool m_minSet;
    bool m_maxSet;

private:
    // Check whether the specified character can be inserted in the control at
    // the given position in the string representing the current controls
    // contents.
    //
    // Notice that the base class checks for '-' itself so it's never passed to
    // this function.
    virtual bool IsCharOk(const wxString& val, int pos, wxChar ch) const = 0;

    // NormalizeString the contents of the string if it's a valid number, return
    // empty string otherwise.
    virtual wxString NormalizeString(const wxString& s) const = 0;

    // Do all checks to ensure this is a valid value.
    // Returns 'true' if the control has valid value.
    // Otherwise the cause is indicated in 'errMsg'.
    virtual bool DoValidateNumber(TranslatableString * errMsg) const = 0;

    // Event handlers.
    void OnChar(wxKeyEvent& event);
    void OnPaste(wxClipboardTextEvent& event);
    void OnKillFocus(wxFocusEvent& event);
    

    // Determine the current insertion point and text in the associated control.
    void GetCurrentValueAndInsertionPoint(wxString& val, int& pos) const;


    // Combination of wxVAL_NUM_XXX values.
    NumValidatorStyle m_style;

    DECLARE_EVENT_TABLE()

    DECLARE_NO_ASSIGN_CLASS(NumValidatorBase)
};

namespace Private
{

// This is a helper class used by IntegerValidator and FloatingPointValidator
// below that implements Transfer{To,From}Window() adapted to the type of the
// variable.
//
// The template argument B is the name of the base class which must derive from
// NumValidatorBase and define LongestValueType type and {To,As}String()
// methods i.e. basically be one of {Integer,Number}ValidatorBase classes.
//
// The template argument T is just the type handled by the validator that will
// inherit from this one.
template <class B, typename T>
class NumValidator /* final */ : public B
{
public:
    typedef B BaseValidator;
    typedef T ValueType;

    typedef typename BaseValidator::LongestValueType LongestValueType;

    // FIXME-VC6: This compiler fails to compile the assert below with a
    // nonsensical error C2248: "'LongestValueType' : cannot access protected
    // typedef declared in class 'IntegerValidatorBase'" so just disable the
    // check for it.
#ifndef __VISUALC6__
    wxCOMPILE_TIME_ASSERT
    (
        sizeof(ValueType) <= sizeof(LongestValueType),
        UnsupportedType
    );
#endif // __VISUALC6__

    void SetMin(ValueType min)
    {
        this->DoSetMin(min);
        BaseValidator::m_minSet = (min != std::numeric_limits<T>::lowest());
    }

    void SetMax(ValueType max)
    {
        this->DoSetMax(max);
        BaseValidator::m_maxSet = (max != std::numeric_limits<T>::max());
    }

    void SetRange(ValueType min, ValueType max)
    {
        SetMin(min);
        SetMax(max);
    }

    bool TransferToWindow() override
    {
        if ( m_value )
        {
            wxTextEntry * const control = BaseValidator::GetTextEntry();
            if ( !control )
                return false;

            control->ChangeValue(NormalizeValue(*m_value));
        }

        return true;
    }

    bool TransferFromWindow() override
    {
        if ( m_value )
        {
            wxTextEntry * const control = BaseValidator::GetTextEntry();
            if ( !control )
                return false;

            // If window is disabled, simply return
            if ( !this->m_validatorWindow->IsEnabled() )
                return true;

            const wxString s(control->GetValue());
            LongestValueType value;
            if ( s.empty() && BaseValidator::HasFlag(NumValidatorStyle::ZERO_AS_BLANK) )
                value = 0;
            else if ( !BaseValidator::FromString(s, &value) )
                return false;

            if ( !this->IsInRange(value) )
                return false;

            *m_value = static_cast<ValueType>(value);
        }

        return true;
    }

protected:
    NumValidator(ValueType *value, NumValidatorStyle style)
        : BaseValidator(style),
          m_value(value)
    {
    }

    // Implement NumValidatorBase virtual method which is the same for
    // both integer and floating point numbers.
    wxString NormalizeString(const wxString& s) const override
    {
        LongestValueType value;
        return BaseValidator::FromString(s, &value) ? NormalizeValue(value)
                                                    : wxString();
    }

private:
    // Just a helper which is a common part of TransferToWindow() and
    // NormalizeString(): returns string representation of a number honouring
    // NumValidatorStyle::ZERO_AS_BLANK flag.
    wxString NormalizeValue(LongestValueType value) const
    {
        wxString s;
        if ( value != 0 || !BaseValidator::HasFlag(NumValidatorStyle::ZERO_AS_BLANK) )
            s = this->ToString(value);

        return s;
    }


    ValueType * const m_value;

    DECLARE_NO_ASSIGN_CLASS(NumValidator)
};

} // namespace Private

// ----------------------------------------------------------------------------
// Validators for integer numbers.
// ----------------------------------------------------------------------------

// Base class for integer numbers validator. This class contains all non
// type-dependent code of wxIntegerValidator<> and always works with values of
// type LongestValueType. It is not meant to be used directly, please use
// IntegerValidator<> only instead.
class SHUTTLEGUI_API IntegerValidatorBase /* not final */
   : public NumValidatorBase
{
protected:
    // Define the type we use here, it should be the maximal-sized integer type
    // we support to make it possible to base IntegerValidator<> for any type
    // on it.
#ifdef wxLongLong_t
    typedef wxLongLong_t LongestValueType;
#else
    typedef long LongestValueType;
#endif

    IntegerValidatorBase(NumValidatorStyle style)
        : NumValidatorBase(style)
    {
        wxASSERT_MSG( !(style & NumValidatorStyle::NO_TRAILING_ZEROES),
                      wxT("This style doesn't make sense for integers.") );
        wxASSERT_MSG( !(style & NumValidatorStyle::ONE_TRAILING_ZERO),
                      wxT("This style doesn't make sense for integers.") );
        wxASSERT_MSG( !(style & NumValidatorStyle::TWO_TRAILING_ZEROES),
                      wxT("This style doesn't make sense for integers.") );
        wxASSERT_MSG( !(style & NumValidatorStyle::THREE_TRAILING_ZEROES),
                      wxT("This style doesn't make sense for integers.") );
    }

    IntegerValidatorBase(const IntegerValidatorBase& other)
        : NumValidatorBase(other)
    {
        m_min = other.m_min;
        m_max = other.m_max;
    }

    // Provide methods for NumValidator use.
    wxString ToString(LongestValueType value) const;
    static bool FromString(const wxString& s, LongestValueType *value);

    void DoSetMin(LongestValueType min) { m_min = min; }
    void DoSetMax(LongestValueType max) { m_max = max; }

    bool IsInRange(LongestValueType value) const
    {
        return m_min <= value && value <= m_max;
    }

    // Implement NumValidatorBase pure virtual method.
    bool IsCharOk(const wxString& val, int pos, wxChar ch) const override;
    bool DoValidateNumber(TranslatableString * errMsg) const override;

private:
    // Minimal and maximal values accepted (inclusive).
    LongestValueType m_min, m_max;

    DECLARE_NO_ASSIGN_CLASS(IntegerValidatorBase)
};

// Validator for integer numbers. It can actually work with any integer type
// (short, int or long and long long if supported) and their unsigned versions
// as well.
template <typename T>
class IntegerValidator final
    : public Private::NumValidator<IntegerValidatorBase, T>
{
public:
    typedef T ValueType;

    typedef
        Private::NumValidator<IntegerValidatorBase, T> Base;

    // Ctor for an integer validator.
    //
    // Sets the range appropriately for the type, including setting 0 as the
    // minimal value for the unsigned types.
    IntegerValidator(
         ValueType *value = NULL,
         NumValidatorStyle style = NumValidatorStyle::DEFAULT,
         ValueType min = std::numeric_limits<ValueType>::min(),
         ValueType max = std::numeric_limits<ValueType>::max())
      : Base(value, style)
    {
       this->SetRange(min, max);
    }

    // Clone is required by wxwidgets; implemented via copy constructor
    wxObject *Clone() const override { return safenew IntegerValidator(*this); }

private:
    DECLARE_NO_ASSIGN_CLASS(IntegerValidator)
};

// Helper function for creating integer validators which allows to avoid
// explicitly specifying the type as it deduces it from its parameter.
template <typename T>
inline IntegerValidator<T>
MakeIntegerValidator(T *value, NumValidatorStyle style = NumValidatorStyle::DEFAULT)
{
    return IntegerValidator<T>(value, style);
}

// ----------------------------------------------------------------------------
// Validators for floating point numbers.
// ----------------------------------------------------------------------------

// Similar to IntegerValidatorBase, this class is not meant to be used
// directly, only FloatingPointValidator<> should be used in the user code.
class SHUTTLEGUI_API FloatingPointValidatorBase /* not final */
   : public NumValidatorBase
{
public:
    // Set precision i.e. the number of digits shown (and accepted on input)
    // after the decimal point. By default this is set to the maximal precision
    // supported by the type handled by the validator.
    void SetPrecision(unsigned precision) { m_precision = precision; }

protected:
    // Notice that we can't use "long double" here because it's not supported
    // by NumberFormatter yet, so restrict ourselves to just double (and
    // float).
    typedef double LongestValueType;

    FloatingPointValidatorBase(NumValidatorStyle style)
        : NumValidatorBase(style)
    {
    }

    FloatingPointValidatorBase(const FloatingPointValidatorBase& other)
        : NumValidatorBase(other)
    {
        m_precision = other.m_precision;

        m_min = other.m_min;
        m_max = other.m_max;
    }

    // Provide methods for NumValidator use.
    wxString ToString(LongestValueType value) const;
    static bool FromString(const wxString& s, LongestValueType *value);

    void DoSetMin(LongestValueType min) { m_min = min; }
    void DoSetMax(LongestValueType max) { m_max = max; }

    bool IsInRange(LongestValueType value) const
    {
        return m_min <= value && value <= m_max;
    }

    // Implement NumValidatorBase pure virtual method.
    bool IsCharOk(const wxString& val, int pos, wxChar ch) const override;
    bool DoValidateNumber(TranslatableString * errMsg) const override;

    //Checks that it doesn't have too many decimal digits.
    bool ValidatePrecision(const wxString& s) const;

private:
    // Maximum number of decimals digits after the decimal separator.
    unsigned m_precision;

    // Minimal and maximal values accepted (inclusive).
    LongestValueType m_min, m_max;

    DECLARE_NO_ASSIGN_CLASS(FloatingPointValidatorBase)
};

// Validator for floating point numbers. It can be used with float, double or
// long double values.
template <typename T>
class FloatingPointValidator final
    : public Private::NumValidator<FloatingPointValidatorBase, T>
{
public:
    typedef T ValueType;
    typedef Private::NumValidator<FloatingPointValidatorBase, T> Base;

    // Ctor using implicit (maximal) precision for this type.
    FloatingPointValidator(ValueType *value = NULL,
                             NumValidatorStyle style = NumValidatorStyle::DEFAULT)
        : Base(value, style)
    {
        DoSetMinMax();

        this->SetPrecision(std::numeric_limits<ValueType>::digits10);
    }

    // Ctor specifying an explicit precision.
    FloatingPointValidator(int precision,
                      ValueType *value = NULL,
                      NumValidatorStyle style = NumValidatorStyle::DEFAULT,
                      ValueType min = std::numeric_limits<ValueType>::lowest(),
                      ValueType max =  std::numeric_limits<ValueType>::max())
        : Base(value, style)
    {
        this->SetRange( min, max );

        this->SetPrecision(precision);
    }

    // Clone is required by wxwidgets; implemented via copy constructor
    wxObject *Clone() const override
    {
        return safenew FloatingPointValidator(*this);
    }

private:
    void DoSetMinMax()
    {
        // NB: Do not use min(), it's not the smallest representable value for
        //     the floating point types but rather the smallest representable
        //     positive value.
        this->DoSetMin( std::numeric_limits<ValueType>::lowest());
        this->DoSetMax( std::numeric_limits<ValueType>::max());
    }
};

// Helper similar to MakeIntValidator().
//
// NB: Unfortunately we can't just have a MakeNumericValidator() which would
//     return either IntegerValidator<> or FloatingPointValidator<> so we
//     do need two different functions.
template <typename T>
inline FloatingPointValidator<T>
MakeFloatingPointValidator(T *value, NumValidatorStyle style = NumValidatorStyle::DEFAULT)
{
    return FloatingPointValidator<T>(value, style);
}

template <typename T>
inline FloatingPointValidator<T>
MakeFloatingPointValidator(int precision, T *value, NumValidatorStyle style = NumValidatorStyle::DEFAULT)
{
    return FloatingPointValidator<T>(precision, value, style);
}

// Sometimes useful for specifying max and min values for validators, when they
// must have the same precision as the validated value
SHUTTLEGUI_API double RoundValue(int precision, double value);

#endif // wxUSE_VALIDATORS

#endif // _WIDGETS_VALNUM_H_
