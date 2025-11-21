/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file NumericConverterFormatter.h

 Dmitry Vedenko

 **********************************************************************/
#pragma once

#include <optional>
#include <vector>

#include <wx/string.h>

#include "Observer.h"

class AudacityProject;

struct NUMERIC_FORMATS_API NumericField final
{
private:
    NumericField(size_t digits, bool zeropad);

public:
    static NumericField ForRange(size_t range, bool zeropad = true, size_t minDigits = 0);
    static NumericField WithDigits(size_t digits, bool zeropad = true);

    NumericField(const NumericField&) = default;
    NumericField& operator=(const NumericField&) = default;
    // NumericField( NumericField && ) = default;
    // NumericField &operator = ( NumericField && ) = default;
    size_t digits { 0 };

    wxString label;
    wxString formatStr;

    size_t pos { wxString::npos }; // Index of this field in the ValueString
};

using NumericFields = std::vector<NumericField>;

struct NUMERIC_FORMATS_API DigitInfo final
{
    size_t field; // Which field
    size_t index; // Index of this digit within the field
    size_t pos;  // Position in the ValueString
};

using DigitInfos = std::vector<DigitInfo>;

struct NumericConverterFormatChangedMessage final
{
    double value;
    bool shrunk;
};

struct NUMERIC_FORMATS_API NumericConverterFormatter /* not final */ : public Observer::Publisher<NumericConverterFormatChangedMessage>
{
    virtual ~NumericConverterFormatter();

    struct NUMERIC_FORMATS_API ConversionResult final
    {
        wxString valueString;
        std::vector<wxString> fieldValueStrings;
    };

    //! Potentially updates the format so it can fit the `value`. Default implementation is empty.
    virtual void UpdateFormatForValue(double value, bool canShrink);
    //! @post result: `GetFields().size() == result.fieldValueStrings.size()`
    virtual ConversionResult
    ValueToString(double value, bool nearest) const = 0;

    virtual std::optional<double>
    StringToValue(const wxString& value) const = 0;

    virtual double SingleStep(double value, int digitIndex, bool upwards) const = 0;

    const wxString& GetPrefix() const;
    const NumericFields& GetFields() const;
    const DigitInfos& GetDigitInfos() const;

protected:
    wxString mPrefix;

    NumericFields mFields;
    DigitInfos mDigits;
};
