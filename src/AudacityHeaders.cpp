/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityHeaders.cpp

  Dominic Mazzoni

**********************************************************************/

#include "AudacityHeaders.h"


const wxString& GetCustomTranslation(const wxString& str1)
{
   return wxGetTranslation( str1 );
   //return str1;
   //return wxString( "AAA" );
}
