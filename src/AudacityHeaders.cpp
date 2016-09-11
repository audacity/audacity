/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityHeaders.cpp

  Dominic Mazzoni

**********************************************************************/

#include "AudacityHeaders.h"

// This allows us to replace Audacity by DarkAudacity without peppering the source
// code with changes.  We split out this step, the customisation, as it is
// used on its own (without translation) in the wxTS macro.
const wxString& GetCustomSubstitution(const wxString& str2)
{
   // If true, already converted.
   if( str2.Contains( "DarkAudacity" ))
      return str2;
   if( !str2.Contains( "Audacity" ))
      return str2;
   wxString str3 = str2;
   str3.Replace( "Audacity", "DarkAudacity" );
   str3.Replace( " an DarkAudacity", " a DarkAudacity" );
   return wxTranslations::GetUntranslatedString(str3);
}

const wxString& GetCustomTranslation(const wxString& str1)
{
   const wxString& str2 = wxGetTranslation( str1 );
   return GetCustomSubstitution( str2 );
}
