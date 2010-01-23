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
void ShowWarningDialog(wxWindow *parent,
                       wxString internalDialogName,
                       wxString message);                   

#endif // __AUDACITY_WARNING__

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 2b69f33b-2dc8-4b9f-99a1-65d57f554133

