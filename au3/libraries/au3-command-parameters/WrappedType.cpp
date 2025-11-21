/**********************************************************************

  WrappedType.cpp

  James Crook
  (C) Audacity Developers, 2007

  wxWidgets license. See Licensing.txt

**********************************************************************//*!

\class WrappedType
\brief
  Used in type conversions, this wrapper for ints, strings, doubles and
  enums provides conversions between all the types.  Functions that
  work on wrapped types can quickly be reused to work on any of
  these types.  This cuts out a lot of repetitive code.

  JKC: This class grows in size with the square of the number of
  types it supports.  It has to do all conversions between all pairs,
  so try to re-use existing types if you can.

  It's probable that not all the cases are actually used in practice.
  The most heavily used will be conversions to <-> from string.

*//**********************************************************************/

#include "WrappedType.h"

#include <wx/wxprec.h>
#include "Internat.h"

/// @return true iff the wrapped type is a string.
bool WrappedType::IsString()
{
    return eWrappedType == eWrappedString;
}

wxString WrappedType::ReadAsString()
{
    switch (eWrappedType) {
    case eWrappedString:
        return *mpStr;
        break;
    case eWrappedInt:
        return wxString::Format(wxT("%i"), *mpInt);
        break;
    case eWrappedDouble:
        return wxString::Format(wxT("%.8g"), *mpDouble);
        break;
    case eWrappedBool:
        return (*mpBool) ? wxT("true") : wxT("false");
        break;
    case eWrappedEnum:
        wxASSERT(false);
        break;
    default:
        wxASSERT(false);
        break;
    }
    return wxT("ERROR"); //Compiler pacifier
}

int WrappedType::ReadAsInt()
{
    switch (eWrappedType) {
    case eWrappedString:
    {
        long l;
        mpStr->ToLong(&l);
        return (int)l;
    }
    case eWrappedInt:
        return *mpInt;
    case eWrappedDouble:
        return (int)*mpDouble;
    case eWrappedBool:
        return (*mpBool) ? 1 : 0;
    case eWrappedEnum:
        wxASSERT(false);
        break;
    default:
        wxASSERT(false);
        break;
    }
    return -1;//Compiler pacifier
}

double WrappedType::ReadAsDouble()
{
    switch (eWrappedType) {
    case eWrappedString:
        return Internat::CompatibleToDouble(*mpStr);
        break;
    case eWrappedInt:
        return (double)*mpInt;
        break;
    case eWrappedDouble:
        return *mpDouble;
        break;
    case eWrappedBool:
        return (*mpBool) ? 1.0 : 0.0;
        break;
    case eWrappedEnum:
        wxASSERT(false);
        break;
    default:
        wxASSERT(false);
        break;
    }
    return -1.0f;//Compiler pacifier
}

bool WrappedType::ReadAsBool()
{
    switch (eWrappedType) {
    case eWrappedString:
        return mpStr->IsSameAs(wxT("true"), false); // case free comparison.
        break;
    case eWrappedInt:
        return *mpInt != 0;
        break;
    case eWrappedDouble:
        wxASSERT(false);// DANGEROUS USE OF WrappedType.  Can't rely on equality.
        return *mpDouble != 0.0f; // this is what the code would be...
        break;
    case eWrappedBool:
        return *mpBool;
        break;
    case eWrappedEnum:
        wxASSERT(false);
        break;
    default:
        wxASSERT(false);
        break;
    }
    return false;//Compiler pacifier
}

void WrappedType::WriteToAsString(const wxString& InStr)
{
    switch (eWrappedType) {
    case eWrappedString:
        *mpStr = InStr;
        break;
    case eWrappedInt:
    {
        long l;
        InStr.ToLong(&l);
        *mpInt = (int)l;
        break;
    }
    case eWrappedDouble:
        *mpDouble = Internat::CompatibleToDouble(InStr);
        break;
    case eWrappedBool:
        *mpBool = InStr.IsSameAs(wxT("true"), false); // case free comparison.;
        break;
    case eWrappedEnum:
        wxASSERT(false);
        break;
    default:
        wxASSERT(false);
        break;
    }
}

void WrappedType::WriteToAsInt(const int InInt)
{
    switch (eWrappedType) {
    case eWrappedString:
        *mpStr = wxString::Format(wxT("%i"), InInt);
        break;
    case eWrappedInt:
        *mpInt = InInt;
        break;
    case eWrappedDouble:
        *mpDouble = (double)InInt;
        break;
    case eWrappedBool:
        *mpBool = (InInt != 0);
        break;
    case eWrappedEnum:
        wxASSERT(false);
        break;
    default:
        wxASSERT(false);
        break;
    }
}

void WrappedType::WriteToAsDouble(const double InDouble)
{
    switch (eWrappedType) {
    case eWrappedString:
        *mpStr = wxString::Format(wxT("%.8g"), InDouble);
        break;
    case eWrappedInt:
        *mpInt = (int)InDouble;
        break;
    case eWrappedDouble:
        *mpDouble = InDouble;
        break;
    case eWrappedBool:
        wxASSERT(false);
        *mpBool = InDouble != 0.0;
        break;
    case eWrappedEnum:
        wxASSERT(false);
        break;
    default:
        wxASSERT(false);
        break;
    }
}

void WrappedType::WriteToAsBool(const bool InBool)
{
    switch (eWrappedType) {
    case eWrappedString:
        *mpStr = InBool ? wxT("true") : wxT("false");
        break;
    case eWrappedInt:
        *mpInt = InBool ? 1 : 0;
        break;
    case eWrappedDouble:
        *mpDouble = InBool ? 1.0 : 0.0;
        break;
    case eWrappedBool:
        *mpBool = InBool;
        break;
    case eWrappedEnum:
        wxASSERT(false);
        break;
    default:
        wxASSERT(false);
        break;
    }
}
