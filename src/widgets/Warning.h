/**********************************************************************

  Audacity: A Digital Audio Editor

  Warning.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_WARNING__
#define __AUDACITY_WARNING__

#include "../Audacity.h"
#include <wx/defs.h>
#include <wx/window.h>

/// Displays a warning dialog with a check box that says
/// "Don't show this warning again".  If the user checks
/// the box, the internalDialogName is noted in the
/// preferences.  The internalDialogName is never seen by
/// the user; it should be unique to each message.
int ShowWarningDialog(wxWindow *parent,
                      const wxString &internalDialogName,
                      const wxString &message,
                      bool showCancelButton = false);

#endif // __AUDACITY_WARNING__
