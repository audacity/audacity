/**********************************************************************

  Audacity: A Digital Audio Editor

  HelpText.h

  James Crook

**********************************************************************/

#ifndef __AUDACITY_HELP_TEXT__
#define __AUDACITY_HELP_TEXT__

class TranslatableString;
class wxString;
#include "Identifier.h"

struct URLStringTag;
//! Distinct type for URLs
using URLString = TaggedIdentifier< URLStringTag >;

AUDACITY_DLL_API  wxString HelpText( const wxString & Key );
AUDACITY_DLL_API TranslatableString TitleText( const wxString & Key );

extern AUDACITY_DLL_API const wxString VerCheckArgs();
extern AUDACITY_DLL_API const URLString VerCheckUrl();
extern AUDACITY_DLL_API const wxString VerCheckHtml();
extern AUDACITY_DLL_API wxString FormatHtmlText( const wxString & Text );

#endif
