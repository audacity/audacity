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
