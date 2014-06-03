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

class Validator
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

class OptionValidator : public Validator
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
   virtual bool Validate(const wxVariant &v)
   {
      SetConverted(v);
      return (mOptions.Index(v.GetString()) != wxNOT_FOUND);
   }
   virtual wxString GetDescription() const
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
   virtual Validator *GetClone() const
   {
      OptionValidator *v = new OptionValidator();
      v->mOptions = mOptions;
      return v;
   }
};

class BoolValidator : public Validator
{
public:
   virtual bool Validate(const wxVariant &v)
   {
      bool val;
      if (!v.Convert(&val)) return false;
      SetConverted(val);
      return GetConverted().IsType(wxT("bool"));
   }
   virtual wxString GetDescription() const
   {
      return wxT("true/false or 1/0 or yes/no");
   }
   virtual Validator *GetClone() const
   {
      return new BoolValidator();
   }
};

class BoolArrayValidator : public Validator
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
   virtual wxString GetDescription() const
   {
      return wxT("0X101XX101...etc.  where 0=false, 1=true, and X=don't care.  Numbering starts at leftmost = track 0");
   }
   virtual Validator *GetClone() const
   {
      return new BoolArrayValidator();
   }
};

class DoubleValidator : public Validator
{
public:
   virtual bool Validate(const wxVariant &v)
   {
      double val;
      if (!v.Convert(&val)) return false;
      SetConverted(val);
      return GetConverted().IsType(wxT("double"));
   }
   virtual wxString GetDescription() const
   {
      return wxT("a floating-point number");
   }
   virtual Validator *GetClone() const
   {
      return new DoubleValidator();
   }
};

class RangeValidator : public Validator
{
private:
   double mLower, mUpper;
public:
   RangeValidator(double l, double u)
      : mLower(l), mUpper(u)
   { }
   virtual bool Validate(const wxVariant &v)
   {
      double val;
      if (!v.Convert(&val)) return false;
      SetConverted(val);
      return ((mLower < val) && (val < mUpper));
   }
   virtual wxString GetDescription() const
   {
      return wxString::Format(wxT("between %d and %d"), mLower, mUpper);
   }
   virtual Validator *GetClone() const
   {
      return new RangeValidator(mLower, mUpper);
   }
};

class IntValidator : public Validator
{
public:
   virtual bool Validate(const wxVariant &v)
   {
      double val;
      if (!v.Convert(&val)) return false;
      SetConverted(val);
      if (!GetConverted().IsType(wxT("double"))) return false;
      return ((long)val == val);
   }
   virtual wxString GetDescription() const
   {
      return wxT("an integer");
   }
   virtual Validator *GetClone() const
   {
      return new IntValidator();
   }
};

/*
class AndValidator : public Validator
{
private:
   Validator &v1, &v2;
public:
   AndValidator(Validator *u1, Validator *u2)
      : v1(*u1), v2(*u2)
   { }
   virtual bool Validate(const wxVariant &v)
   {
      if (!v1.Validate(v)) return false;
      return v2.Validate(v);
   }
   virtual wxString GetDescription() const
   {
      return v1.GetDescription() + wxT(" and ") + v2.GetDescription();
   }
   virtual Validator *GetClone() const
   {
      return new AndValidator(v1.GetClone(), v2.GetClone());
   }
};*/

#endif /* End of include guard: __VALIDATORS__ */
