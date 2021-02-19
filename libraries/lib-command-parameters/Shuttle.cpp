/**********************************************************************

  Shuttle.cpp

  James Crook
  (C) Audacity Developers, 2007

  wxWidgets license. See Licensing.txt

*******************************************************************//**

\file Shuttle.cpp
\brief Implements Shuttle, ShuttleParams, their subclasses, and Enums.

*//****************************************************************//**

\class Shuttle
\brief Moves data from one place to another, converting it as required.

  Shuttle provides a base class for transferring parameter data into and
  out of classes into some other structure.  This is a common
  requirement and is needed for:
    - Prefs data
    - Command line parameter data
    - Project data in XML

  The 'Master' is the string side of the shuttle transfer, the 'Client'
  is the binary data side of the transfer.

  \see ShuttleBase
  \see ShuttleGui

*//****************************************************************//**

\class Enums
\brief Enums is a helper class for Shuttle.  It defines enumerations
which are used in effects dialogs, in the effects themselves and in
preferences.

(If it grows big, we will move it out of shuttle.h).

*//*******************************************************************/

#include "Shuttle.h"
#include "WrappedType.h"
#include <wx/crt.h>

Shuttle::Shuttle()
{
}

Shuttle::~Shuttle() = default;

bool Shuttle::TransferBool( const wxString & Name, bool & bValue, const bool & bDefault )
{
   if( mbStoreInClient )
   {
      bValue = bDefault;
      if( ExchangeWithMaster( Name ))
      {
         if( !mValueString.empty() )
            bValue = mValueString.GetChar(0) == wxT('y');
      }
   }
   else
   {
      mValueString = (bValue==0) ? wxT("no"):wxT("yes");
      return ExchangeWithMaster( Name );
   }
   return true;
}

bool Shuttle::TransferFloat( const wxString & Name, float & fValue, const float &fDefault )
{
   if( mbStoreInClient )
   {
      fValue = fDefault;
      if( ExchangeWithMaster( Name ))
      {
         if( !mValueString.empty() )
            fValue = wxAtof( mValueString );
      }
   }
   else
   {
      mValueString = wxString::Format(wxT("%f"),fValue);
      return ExchangeWithMaster( Name );
   }
   return true;
}

bool Shuttle::TransferDouble( const wxString & Name, double & dValue, const double &dDefault )
{
   if( mbStoreInClient )
   {
      dValue = dDefault;
      if( ExchangeWithMaster( Name ))
      {
         if( !mValueString.empty() )
            dValue = wxAtof( mValueString );
      }
   }
   else
   {
      //%f is format string for double
      mValueString = wxString::Format(wxT("%f"),dValue);
      return ExchangeWithMaster( Name );
   }
   return true;
}

bool Shuttle::TransferInt( const wxString & Name, int & iValue, const int & iDefault )
{
   if( mbStoreInClient )
   {
      iValue = iDefault;
      if( ExchangeWithMaster( Name ))
      {
         iValue = wxAtoi( mValueString );
      }
   }
   else
   {
      mValueString = wxString::Format(wxT("%i"),iValue);
      return ExchangeWithMaster( Name );
   }
   return true;
}


bool Shuttle::TransferInt( const wxString & Name, wxLongLong_t & iValue, const wxLongLong_t & iDefault )
{
   return TransferLongLong(Name, iValue, iDefault);
}

bool Shuttle::TransferLongLong( const wxString & Name, wxLongLong_t & iValue, const wxLongLong_t & iDefault )
{
   if( mbStoreInClient )
   {
      iValue = iDefault;
      if( ExchangeWithMaster( Name ))
      {
         iValue = wxAtoi( mValueString );
      }
   }
   else
   {
      /// \todo Fix for long long values.
      mValueString = wxString::Format(wxT("%lld"), (long long) iValue);
      return ExchangeWithMaster( Name );
   }
   return true;
}


bool Shuttle::TransferEnum( const wxString & Name, int & iValue,
      const int nChoices, const wxString * pFirstStr)
{
   if( mbStoreInClient )
   {
      iValue = 0;// default index if none other selected.
      if( ExchangeWithMaster( Name ))
      {
         wxString str = mValueString;
         if( str.Left( 1 ) == wxT('"') && str.Right( 1 ) == wxT('"') )
         {
            str = str.Mid( 2, str.length() - 2 );
         }

         for( int i = 0; i < nChoices; i++ )
         {
            if( str == pFirstStr[i] )
            {
               iValue = i;
               break;
            }
         }
      }
   }
   else
   {
      //TIDY-ME: Out of range configuration values are silently discarded...
      if( iValue > nChoices )
         iValue = 0;
      if( iValue < 0 )
         iValue = 0;
      mValueString = pFirstStr[iValue];
      if( mValueString.Find( wxT(' ') ) != wxNOT_FOUND )
      {
         mValueString = wxT('"') + pFirstStr[iValue] + wxT('"');  //strings have quotes around them
      }
      return ExchangeWithMaster( Name );
   }
   return true;
}

bool Shuttle::TransferString( const wxString & Name, wxString & strValue, const wxString & WXUNUSED(strDefault) )
{
   if( mbStoreInClient )
   {
      if( ExchangeWithMaster( Name ))
      {
         strValue = mValueString;
      }
      else
         return false;
   }
   else
   {
      mValueString = wxT('"') + strValue + wxT('"');  //strings have quotes around them
      return ExchangeWithMaster( Name );
   }
   return true;
}

bool Shuttle::TransferWrappedType( const wxString & Name, WrappedType & W )
{
   if( mbStoreInClient )
   {
      if( ExchangeWithMaster( Name ))
      {
         W.WriteToAsString( mValueString );
      }
   }
   else
   {
      mValueString = W.ReadAsString();
      return ExchangeWithMaster( Name );
   }
   return true;
}


bool Shuttle::ExchangeWithMaster(const wxString & WXUNUSED(Name))
{
   // ExchangeWithMaster() will usually be over-ridden
   // in derived classes.  We could have made it an
   // abstract function.
   wxASSERT( false );
   return true;
}

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
