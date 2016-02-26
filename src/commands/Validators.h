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

class Validator /* not final */
{
private:
   wxVariant mConverted;

public:
   Validator() {};
   virtual ~Validator() {};
   void SetConverted (const wxVariant &v)
   {
      mConverted = v;
   }
   const wxVariant &GetConverted()
   {
      return mConverted;
   }

   /// Judge whether the passed value satisfies the Validator
   virtual bool Validate(const wxVariant &v)
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
   virtual Validator *GetClone() const
   {
      return new Validator();
   }
};

class OptionValidator final : public Validator
{
private:
   wxArrayString mOptions;

public:
   void AddOption(const wxString &option)
   {
      mOptions.Add(option);
   }
   void AddOptions(const wxArrayString &options)
   {
      mOptions.insert(mOptions.begin(), options.begin(), options.end());
   }
   bool Validate(const wxVariant &v) override
   {
      SetConverted(v);
      return (mOptions.Index(v.GetString()) != wxNOT_FOUND);
   }
   wxString GetDescription() const override
   {
      wxString desc = wxT("one of: ");
      int optionCount = mOptions.GetCount();
      int i = 0;
      for (i = 0; i+1 < optionCount; ++i)
      {
         desc += mOptions[i] + wxT(", ");
      }
      desc += mOptions[optionCount-1];
      return desc;
   }
   Validator *GetClone() const override
   {
      OptionValidator *v = new OptionValidator();
      v->mOptions = mOptions;
      return v;
   }
};

class BoolValidator final : public Validator
{
public:
   bool Validate(const wxVariant &v) override
   {
      bool val;
      if (!v.Convert(&val)) return false;
      SetConverted(val);
      return GetConverted().IsType(wxT("bool"));
   }
   wxString GetDescription() const override
   {
      return wxT("true/false or 1/0 or yes/no");
   }
   Validator *GetClone() const override
   {
      return new BoolValidator();
   }
};

class BoolArrayValidator final : public Validator
{
public:
   virtual bool Validate(const wxVariant &v)
   {
      wxString val;         // Validate a string of chars containing only 0, 1 and x.
      if (!v.Convert(&val))
         return false;
      SetConverted(val);
      for(size_t i=0; i != val.Len(); i++)
         if( val[i] != '0' && val[i] != '1' && val[i] != 'x' && val[i] != 'X')
            return false;
      return true;
   }
   wxString GetDescription() const override
   {
      return wxT("0X101XX101...etc.  where 0=false, 1=true, and X=don't care.  Numbering starts at leftmost = track 0");
   }
   Validator *GetClone() const override
   {
      return new BoolArrayValidator();
   }
};

class DoubleValidator final : public Validator
{
public:
   bool Validate(const wxVariant &v) override
   {
      double val;
      if (!v.Convert(&val)) return false;
      SetConverted(val);
      return GetConverted().IsType(wxT("double"));
   }
   wxString GetDescription() const override
   {
      return wxT("a floating-point number");
   }
   Validator *GetClone() const override
   {
      return new DoubleValidator();
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
   bool Validate(const wxVariant &v) override
   {
      double val;
      if (!v.Convert(&val)) return false;
      SetConverted(val);
      return ((mLower < val) && (val < mUpper));
   }
   wxString GetDescription() const override
   {
      return wxString::Format(wxT("between %f and %f"), mLower, mUpper);
   }
   Validator *GetClone() const override
   {
      return new RangeValidator(mLower, mUpper);
   }
};

class IntValidator final : public Validator
{
public:
   bool Validate(const wxVariant &v) override
   {
      double val;
      if (!v.Convert(&val)) return false;
      SetConverted(val);
      if (!GetConverted().IsType(wxT("double"))) return false;
      return ((long)val == val);
   }
   wxString GetDescription() const override
   {
      return wxT("an integer");
   }
   Validator *GetClone() const override
   {
      return new IntValidator();
   }
};

/*
class AndValidator final : public Validator
{
private:
   Validator &v1, &v2;
public:
   AndValidator(Validator *u1, Validator *u2)
      : v1(*u1), v2(*u2)
   { }
   bool Validate(const wxVariant &v) override
   {
      if (!v1.Validate(v)) return false;
      return v2.Validate(v);
   }
   wxString GetDescription() const override
   {
      return v1.GetDescription() + wxT(" and ") + v2.GetDescription();
   }
   Validator *GetClone() const override
   {
      return new AndValidator(v1.GetClone(), v2.GetClone());
   }
};*/

#endif /* End of include guard: __VALIDATORS__ */
