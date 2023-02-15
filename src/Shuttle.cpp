/**********************************************************************

  Shuttle.cpp

  James Crook
  (C) Audacity Developers, 2007

  wxWidgets license. See Licensing.txt

*******************************************************************//**

\file Shuttle.cpp
\brief Implements Shuttle, ShuttleParams, their subclasses, and Enums.

*//****************************************************************//**

\class Enums
\brief Enums is a helper class for Shuttle.  It defines enumerations
which are used in effects dialogs, in the effects themselves and in
preferences.

(If it grows big, we will move it out of shuttle.h).

*//*******************************************************************/


#include "Shuttle.h"

#include <wx/defs.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/listctrl.h>

#include "WrappedType.h"
//#include "effects/Effect.h"

#ifdef _MSC_VER
// If this is compiled with MSVC (Visual Studio)
#pragma warning( push )
#pragma warning( disable: 4100 ) // unused parameters.
#endif //_MSC_VER


// The ShouldSet and CouldGet functions have an important side effect
// on the pOptionalFlag.  They 'use it up' and clear it down for the next parameter.


template<bool Const>
SettingsVisitorBase<Const>::~SettingsVisitorBase() = default;

template<bool Const>
auto SettingsVisitorBase<Const>::Optional( [[maybe_unused]] Ref<bool> var )
   -> SettingsVisitorBase &
{
   pOptionalFlag = nullptr;
   return *this;
}

template<bool Const>
auto SettingsVisitorBase<Const>::OptionalY( Ref<bool> var )
   -> SettingsVisitorBase &
{
   return Optional( var );
}

template<bool Const>
auto SettingsVisitorBase<Const>::OptionalN( Ref<bool> var )
   -> SettingsVisitorBase &
{
   return Optional( var );
}

// Tests for parameter being optional.
// Prepares for next parameter by clearing the pointer.
// Reports on whether the parameter should be set, i.e. should set 
// if it was chosen to be set, or was not optional.
template<bool Const>
bool SettingsVisitorBase<Const>::ShouldSet()
{
   if( !pOptionalFlag )
      return true;
   bool result = *pOptionalFlag;
   pOptionalFlag = NULL;
   return result;
}

// These are functions to override.  They do nothing.
template<bool Const>
void SettingsVisitorBase<Const>::Define(Arg<bool>, const wxChar *,
   bool, bool, bool, bool)
{}

template<bool Const>
void SettingsVisitorBase<Const>::Define(Arg<size_t>, const wxChar *,
   int, int, int, int)
{}

template<bool Const>
void SettingsVisitorBase<Const>::Define(Arg<int>, const wxChar *,
   int, int, int, int)
{}

template<bool Const>
void SettingsVisitorBase<Const>::Define(
   Arg<float>, const wxChar *, float, float, float, float)
{}

template<bool Const>
void SettingsVisitorBase<Const>::Define(
   Arg<double>, const wxChar *, float, float, float, float )
{}

template<bool Const>
void SettingsVisitorBase<Const>::Define(
   Arg<double>, const wxChar *, double, double, double, double)
{}

template<bool Const>
void SettingsVisitorBase<Const>::Define(
   Ref<wxString>, const wxChar *, wxString, wxString, wxString, wxString)
{}

template<bool Const>
void SettingsVisitorBase<Const>::DefineEnum(
   Arg<int>, const wxChar *, int, const EnumValueSymbol [], size_t)
{}

// Explicit instantiations
template class SettingsVisitorBase<false>;
template class SettingsVisitorBase<true>;

#ifdef _MSC_VER
// If this is compiled with MSVC (Visual Studio)
#pragma warning( pop )
#endif //_MSC_VER
