/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportRaw.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_IMPORT_RAW__
#define __AUDACITY_IMPORT_RAW__

#include "../MemoryX.h"

class TrackFactory;
class WaveTrack;
class DirManager;
class wxString;
class wxWindow;

#include <vector>

using TrackHolders = std::vector<std::unique_ptr<WaveTrack>>;


void ImportRaw(wxWindow *parent, const wxString &fileName,
   TrackFactory *trackFactory, TrackHolders &outTracks);

#endif
