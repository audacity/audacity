/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2010 Audacity Team.
   License: GPL v2.  See License.txt.

   MultiDialog.h

   Monty
   Vaughan Johnson

**********************************************************************/

#ifndef __AUDACITY_MULTIDIALOG__
#define __AUDACITY_MULTIDIALOG__

#include <wx/defs.h>

#include <wx/chartype.h> // for typedef wxChar
#include "Internat.h" // for TranslatableStrings
class wxString;

const TranslatableString &DefaultMultiDialogMessage();

// Display a dialog with radio buttons.
// Return the zero-based index of the chosen button.
int ShowMultiDialog(const TranslatableString &message,
                    const TranslatableString &title,
                    const TranslatableStrings &buttons,
                    const wxString & helpPage,
                    const TranslatableString &boxMsg
                       = DefaultMultiDialogMessage(),
                    bool log = true);

#endif // __AUDACITY_MULTIDIALOG__
