/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxWidgets

   Dan Horgan

******************************************************************//**

\file Validators.h
\brief Contains declarations and definitions for Validator, OptionValidator,
BoolValidator, DoubleValidator, RangeValidator, IntValidator and AndValidator
classes.

\class Validator
\brief A Validator is an object which checks whether a wxVariant satisfies
a certain criterion. This is a base validator which allows anything.

\class OptionValidator
\brief Parameter must be one of the defined options

\class BoolValidator
\brief Parameter must be a boolean

\class BoolArrayValidator
\brief Parameter must be char array of booleans, e.g. "011010001"

\class DoubleValidator
\brief Parameter must be a floating-point number

\class RangeValidator
\brief Parameter must lie between the two given numbers

\class IntValidator
\brief Parameter must be integral

\class AndValidator
\brief Parameter must pass both of the supplied validators

*//*******************************************************************/

#ifndef __VALIDATORS__
#define __VALIDATORS__

class wxArrayString;

#include <memory>
#include "IteratorX.h"

#include <wx/variant.h> // member variable

class Validator /* not final */
{
private:
    wxVariant mConverted;

public:
    Validator() {}
    virtual ~Validator() {}
    void SetConverted(const wxVariant& v)
    {
        mConverted = v;
    }

    const wxVariant& GetConverted()
    {
        return mConverted;
    }

    /// Judge whether the passed value satisfies the Validator
    virtual bool Validate(const wxVariant& v)
    {
        SetConverted(v);
        return true;
    }

    /// Return a description (for error messages)
    /// should be of the form 'v must be $description'
    virtual wxString GetDescription() const
    {
        return wxT("any value");
    }

    /// This MUST be overridden, to avoid slicing!
    using Holder = std::unique_ptr<Validator>;
    virtual Holder GetClone() const = 0;
};

class DefaultValidator final : public Validator
{
public:
    virtual Holder GetClone() const
    {
        return std::make_unique<DefaultValidator>(*this);
    }
};

class OptionValidator final : public Validator
{
private:
    wxArrayString mOptions;

public:
    void AddOption(const wxString& option)
    {
        mOptions.push_back(option);
    }

    void AddOptions(const wxArrayString& options)
    {
        mOptions.insert(mOptions.begin(), options.begin(), options.end());
    }

    bool Validate(const wxVariant& v) override
    {
        SetConverted(v);
        return make_iterator_range(mOptions).contains(v.GetString());
    }

    wxString GetDescription() const override
    {
        wxString desc = wxT("one of: ");
        int optionCount = mOptions.size();
        int i = 0;
        for (i = 0; i + 1 < optionCount; ++i) {
            desc += mOptions[i] + wxT(", ");
        }
        desc += mOptions[optionCount - 1];
        return desc;
    }

    Holder GetClone() const override
    {
        auto v = std::make_unique<OptionValidator>();
        v->mOptions = mOptions;
        // This std::move is needed to "upcast" the pointer type
        return std::move(v);
    }
};

class BoolValidator final : public Validator
{
public:
    bool Validate(const wxVariant& v) override
    {
        bool val;
        if (!v.Convert(&val)) {
            return false;
        }
        SetConverted(val);
        return GetConverted().IsType(wxT("bool"));
    }

    wxString GetDescription() const override
    {
        return wxT("true/false or 1/0 or yes/no");
    }

    Holder GetClone() const override
    {
        return std::make_unique<BoolValidator>();
    }
};

class BoolArrayValidator final : public Validator
{
public:
    virtual bool Validate(const wxVariant& v) override
    {
        wxString val;       // Validate a string of chars containing only 0, 1 and x.
        if (!v.Convert(&val)) {
            return false;
        }
        SetConverted(val);
        for (size_t i=0; i != val.length(); i++) {
            if (val[i] != '0' && val[i] != '1' && val[i] != 'x' && val[i] != 'X') {
                return false;
            }
        }
        return true;
    }

    wxString GetDescription() const override
    {
        return wxT("0X101XX101...etc. where 0=false, 1=true, and X=don't care. Numbering starts at leftmost = track 0");
    }

    Holder GetClone() const override
    {
        return std::make_unique<BoolArrayValidator>();
    }
};

class DoubleValidator final : public Validator
{
public:
    bool Validate(const wxVariant& v) override
    {
        double val;
        if (!v.Convert(&val)) {
            return false;
        }
        SetConverted(val);
        return GetConverted().IsType(wxT("double"));
    }

    wxString GetDescription() const override
    {
        return wxT("a floating-point number");
    }

    Holder GetClone() const override
    {
        return std::make_unique<DoubleValidator>();
    }
};

class RangeValidator final : public Validator
{
private:
    double mLower, mUpper;
public:
    RangeValidator(double l, double u)
        : mLower(l), mUpper(u)
    { }
    bool Validate(const wxVariant& v) override
    {
        double val;
        if (!v.Convert(&val)) {
            return false;
        }
        SetConverted(val);
        return (mLower < val) && (val < mUpper);
    }

    wxString GetDescription() const override
    {
        return wxString::Format(wxT("between %f and %f"), mLower, mUpper);
    }

    Holder GetClone() const override
    {
        return std::make_unique<RangeValidator>(mLower, mUpper);
    }
};

class IntValidator final : public Validator
{
public:
    bool Validate(const wxVariant& v) override
    {
        double val;
        if (!v.Convert(&val)) {
            return false;
        }
        SetConverted(val);
        if (!GetConverted().IsType(wxT("double"))) {
            return false;
        }
        return (long)val == val;
    }

    wxString GetDescription() const override
    {
        return wxT("an integer");
    }

    Holder GetClone() const override
    {
        return std::make_unique<IntValidator>();
    }
};

/*
class AndValidator final : public Validator
{
private:
   Validator::Holder v1, v2;
public:
   AndValidator(Validator::Holder &&u1, Validator::Holder &&u2)
      : v1(std::move(u1)), v2(std::move(u2))
   { }
   bool Validate(const wxVariant &v) override
   {
      return v1->Validate(v) && v2->Validate(v);
   }
   wxString GetDescription() const override
   {
      return v1->GetDescription() + wxT(" and ") + v2->GetDescription();
   }
   Validator *GetClone() const override
   {
      return std::make_unique<AndValidator>(v1->GetClone(), v2->GetClone());
   }
};*/

#endif /* End of include guard: __VALIDATORS__ */
