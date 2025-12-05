/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2010 Audacity Team.
   License: GPL v2 or later.  See License.txt.

   MultiDialog.h

   Monty
   Vaughan Johnson

**********************************************************************/

#ifndef __AUDACITY_MULTIDIALOG__
#define __AUDACITY_MULTIDIALOG__

#include <wx/defs.h>

#include <wx/chartype.h> // for typedef wxChar
#include "Identifier.h" // for ManualPageID
#include "Internat.h" // for TranslatableStrings
class wxString;

// Display a dialog with radio buttons.
// Return the zero-based index of the chosen button.
int ShowMultiDialog(const TranslatableString& message, const TranslatableString& title, const TranslatableStrings& buttons,
                    const ManualPageID& helpPage, const TranslatableString& boxMsg, bool log);

#endif // __AUDACITY_MULTIDIALOG__
