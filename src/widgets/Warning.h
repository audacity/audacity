/**********************************************************************

  Audacity: A Digital Audio Editor

  Warning.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_WARNING__
#define __AUDACITY_WARNING__

#include "../Audacity.h"

#include <wx/defs.h>
class wxString;

// "Don't show this warning again"
 const wxString &DefaultWarningFooter();

/// Displays a warning dialog with a check box.  If the user checks
/// the box, the internalDialogName is noted in the
/// preferences.  The internalDialogName is never seen by
/// the user; it should be unique to each message.
int ShowWarningDialog(wxWindow *parent,
                      const wxString &internalDialogName,
                      const wxString &message,
                      bool showCancelButton = false,
                      // This message appears by the checkbox:
                      const wxString &footer = DefaultWarningFooter());

#endif // __AUDACITY_WARNING__
