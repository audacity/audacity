/**********************************************************************

  Audacity: A Digital Audio Editor

  HelpText.h

  James Crook

**********************************************************************/

#ifndef __AUDACITY_HELP_TEXT__
#define __AUDACITY_HELP_TEXT__

class TranslatableString;
class wxString;

AUDACITY_DLL_API  wxString HelpText( const wxString & Key );
AUDACITY_DLL_API TranslatableString TitleText( const wxString & Key );

extern AUDACITY_DLL_API const wxString VerCheckArgs();
extern AUDACITY_DLL_API const wxString VerCheckUrl();
extern AUDACITY_DLL_API const wxString VerCheckHtml();
extern AUDACITY_DLL_API wxString FormatHtmlText( const wxString & Text );

#endif
