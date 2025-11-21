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

WX_INIT_API wxString HelpText(const wxString& Key);
WX_INIT_API TranslatableString TitleText(const wxString& Key);

extern WX_INIT_API const wxString VerCheckArgs();
extern WX_INIT_API const URLString VerCheckUrl();
extern WX_INIT_API const wxString VerCheckHtml();
extern WX_INIT_API wxString FormatHtmlText(const wxString& Text);

#endif
