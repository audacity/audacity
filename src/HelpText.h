/**********************************************************************

  Audacity: A Digital Audio Editor

  HelpText.h

  James Crook

**********************************************************************/

#ifndef __AUDACITY_HELP_TEXT__
#define __AUDACITY_HELP_TEXT__

class TranslatableString;
class wxString;

wxString HelpText( const wxString & Key );
TranslatableString TitleText( const wxString & Key );

extern const wxString VerCheckArgs();
extern const wxString VerCheckUrl();
extern const wxString VerCheckHtml();
extern wxString FormatHtmlText( const wxString & Text );

#endif
