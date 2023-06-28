#include "ProjectTempoListener.h"
#include "Project.h"
#include "ProjectTimeSignature.h"
#include "WaveTrack.h"

ProjectTempoListener::ProjectTempoListener(
   AudacityProject& project, TrackList& trackList)
    : mTrackList { trackList }
    , mTrackListSubstription { trackList.Subscribe(
         [&project](const TrackListEvent& event) {
            if (event.mType == TrackListEvent::ADDITION)
            {
               const auto track = event.mpTrack.lock();
               if (auto waveTrack = dynamic_cast<WaveTrack*>(track.get()))
                  waveTrack->SetProjectTempo(
                     ProjectTimeSignature::Get(project).GetTempo());
            }
         }) }
{
}

void ProjectTempoListener::SetProjectTempo(double newTempo)
{
   for (auto track : mTrackList)
      if (auto waveTrack = dynamic_cast<WaveTrack*>(track))
         waveTrack->SetProjectTempo(newTempo);
}
