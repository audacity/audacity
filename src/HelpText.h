/**********************************************************************

  Sneedacity: A Digital Audio Editor

  HelpText.h

  James Crook

**********************************************************************/

#ifndef __SNEEDACITY_HELP_TEXT__
#define __SNEEDACITY_HELP_TEXT__

class TranslatableString;
class wxString;
#include "Identifier.h"

struct URLStringTag;
//! Distinct type for URLs
using URLString = TaggedIdentifier< URLStringTag >;

SNEEDACITY_DLL_API  wxString HelpText( const wxString & Key );
SNEEDACITY_DLL_API TranslatableString TitleText( const wxString & Key );

extern SNEEDACITY_DLL_API const wxString VerCheckArgs();
extern SNEEDACITY_DLL_API const URLString VerCheckUrl();
extern SNEEDACITY_DLL_API const wxString VerCheckHtml();
extern SNEEDACITY_DLL_API wxString FormatHtmlText( const wxString & Text );

#endif
