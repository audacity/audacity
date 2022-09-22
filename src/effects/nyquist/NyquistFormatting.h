/**********************************************************************

  Audacity: A Digital Audio Editor

  @file NyquistFormatting.h

  Dominic Mazzoni

  Paul Licameli split from NyquistControls.h

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NYQUIST_FORMATTING__
#define __AUDACITY_EFFECT_NYQUIST_FORMATTING__

#include "FileNames.h"

namespace NyquistFormatting {

wxString EscapeString(const wxString & inStr);

double GetCtrlValue(const wxString &s);

/*!
 A file path given to Nyquist may be a platform-independent canonicalized
 form using certain abbreviations that are expanded into the platform-dependent
 equivalent.

 If the path names only a directory, also append "/untitled" plus extension
 */
void resolveFilePath(wxString & path, FileExtension extension = {});

}
#endif
