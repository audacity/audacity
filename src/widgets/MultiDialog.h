/**********************************************************************

  Audacity: A Digital Audio Editor

  ListWarning.h

  Monty

**********************************************************************/

#ifndef __AUDACITY_MULTIDIALOG__
#define __AUDACITY_MULTIDIALOG__

#include "../Audacity.h"
#include <wx/defs.h>
#include <wx/window.h>

/// Displays a MessageBox-like warning dialog with a scrolling list
/// window
int ShowMultiDialog(wxString prompt,
                    wxString title,
                    const wxChar **buttons);

#endif // __AUDACITY_MULTIDIALOG__

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

