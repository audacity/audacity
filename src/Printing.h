/**********************************************************************

  Audacity: A Digital Audio Editor

  Printing.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_PRINTING__
#define __AUDACITY_PRINTING__

#include <wx/defs.h>

class wxString;
class wxWindow;
class TrackList;
class TrackPanel;

void HandlePageSetup(wxWindow *parent);
void HandlePrint(
   wxWindow *parent, const wxString &name, TrackList *tracks,
   TrackPanel &panel);

#endif // __AUDACITY_PRINTING__

