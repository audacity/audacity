/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportRaw.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_IMPORT_RAW__
#define __AUDACITY_IMPORT_RAW__

#include <memory>

class AudacityProject;
class Track;
class WaveTrackFactory;
class wxString;
class wxWindow;

#include <vector>

using TrackHolders = std::vector<std::shared_ptr<Track> >;

void ImportRaw(const AudacityProject& project, wxWindow* parent, const wxString& fileName, WaveTrackFactory* trackFactory,
               TrackHolders& outTracks);

#endif
