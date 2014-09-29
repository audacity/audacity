/**********************************************************************

  Shuttle.cpp

  James Crook
  (C) Audacity Developers, 2007

  wxWidgets license. See Licensing.txt

*******************************************************************//**

\file Shuttle.cpp
\brief Implements Shuttle, ShuttleCli and Enums.

*//****************************************************************//**

\class Shuttle
\brief Moves data from one place to another, converting it as required.

  Shuttle provides a base class for transfering parameter data into and
  out of clasess into some other structure.  This is a common
  requirement and is needed for:
    - Prefs data
    - Command line parameter data
    - Project data in XML

  The 'Master' is the string side of the shuttle transfer, the 'Client'
  is the binary data side of the transfer.

  \see ShuttleBase
  \see ShuttleGui

*//****************************************************************//**

\class ShuttleCli
\brief Derived from Shuttle, this class exchanges string parameters
with a binary representation.

  \see ShuttleBase
  \see ShuttleGui

*//****************************************************************//**

\class Enums
\brief Enums is a helper class for Shuttle.  It defines enumerations
which are used in effects dialogs, in the effects themselves and in
preferences.

(If it grows big, we will move it out of shuttle.h).

*//*******************************************************************/

//#include "Audacity.h"

#include <wx/defs.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/listctrl.h>
#include <wx/radiobut.h>
#include <wx/button.h>

//#include "Project.h"
#include "Shuttle.h"
#include "WrappedType.h"
//#include "commands/CommandManager.h"
//#include "effects/Effect.h"

const int Enums::NumDbChoices = 13;

const wxString Enums::DbChoices[] =
   {wxT("-20 dB"), wxT("-25 dB"), wxT("-30 dB"),
    wxT("-35 dB"), wxT("-40 dB"), wxT("-45 dB"),
    wxT("-50 dB"), wxT("-55 dB"), wxT("-60 dB"),
    wxT("-65 dB"), wxT("-70 dB"), wxT("-75 dB"),
    wxT("-80 dB")};

const double Enums::Db2Signal[] =
//     -20dB    -25dB    -30dB    -35dB    -40dB    -45dB    -50dB    -55dB    -60dB    -65dB     -70dB     -75dB     -80dB    Off
   { 0.10000, 0.05620, 0.03160, 0.01780, 0.01000, 0.00562, 0.00316, 0.00178, 0.00100, 0.000562, 0.000316, 0.000178, 0.0001000, 0.0 };


const wxString * Enums::GetDbChoices()
{
   return DbChoices;
}


Shuttle::Shuttle()
{
}

bool Shuttle::TransferBool( const wxString & Name, bool & bValue, const bool & bDefault )
{
   if( mbStoreInClient )
   {
      bValue = bDefault;
      if( ExchangeWithMaster( Name ))
      {
         if( !mValueString.IsEmpty() )
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
         if( !mValueString.IsEmpty() )
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
         if( !mValueString.IsEmpty() )
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
      mValueString = wxString::Format(wxT("%d"),iValue);
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
            str = str.Mid( 2, str.Length() - 2 );
         }

         for( int i = 0; i < nChoices; i++ )
         {
            if( str.IsSameAs( pFirstStr[i] ))
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

// This variant uses values of the form
// param1=value1 param2=value2
bool ShuttleCli::ExchangeWithMaster(const wxString & Name)
{
   if( !mbStoreInClient )
   {
      mParams += wxT(" ");
      mParams +=Name;
      mParams += wxT("=");
      mParams +=mValueString;
   }
   else
   {
      int i;
      mParams = wxT(" ")+mParams;
      i=mParams.Find( wxT(" ")+Name+wxT("=") );
      if( i>=0 )
      {
         int j=i+2+Name.Length();
         wxString terminator = wxT(' ');
         if(mParams.GetChar(j) == wxT('"')) //Strings are surrounded by quotes
         {
            terminator = wxT('"');
            j++;
         }
         i=j;
         while( j<(int)mParams.Length() && mParams.GetChar(j) != terminator )
            j++;
         mValueString = mParams.Mid(i,j-i);
         return true;
      }
      return false;
   }
   return true;
}
