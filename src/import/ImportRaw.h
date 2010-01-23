/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportRaw.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_IMPORT_RAW__
#define __AUDACITY_IMPORT_RAW__

#include "Import.h"

class WaveTrack;
class DirManager;
class wxWindow;

int ImportRaw(wxWindow *parent, wxString fileName,
              TrackFactory *trackFactory, Track ***outTracks);

#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 1bed8f22-2559-4a6a-8480-9fb70630b11c

