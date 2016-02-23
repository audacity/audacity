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
#include <wx/window.h>

// Display a dialog with radio buttons.
// Return the zero-based index of the chosen button.
int ShowMultiDialog(const wxString &message,
                    const wxString &title,
                    const wxChar **buttons, const wxString &boxMsg = _("Please select an action"), bool log = true);

#endif // __AUDACITY_MULTIDIALOG__
