#include "ProjectTempoObserver.h"
#include "ProjectTimeSignature.h"
#include "Track.h"
#include "WaveTrack.h"

static const AudacityProject::AttachedObjects::RegisteredFactory key {
   [](AudacityProject& project) {
      return std::make_shared<ProjectTempoObserver>(&project);
   }
};

ProjectTempoObserver& ProjectTempoObserver::Get(AudacityProject& project)
{
   return project.AttachedObjects::Get<ProjectTempoObserver>(key);
}

const ProjectTempoObserver&
ProjectTempoObserver::Get(const AudacityProject& project)
{
   return Get(const_cast<AudacityProject&>(project));
}

ProjectTempoObserver::ProjectTempoObserver(AudacityProject* project)
{
   assert(project);
   auto& timeSignature = ProjectTimeSignature::Get(*project);
   auto& trackList = TrackList::Get(*project);
   const auto projectTempo = timeSignature.GetTempo();
   for (const auto& track : trackList.Any<WaveTrack>())
   {
      track->OnProjectTempoChange(projectTempo, projectTempo);
   }
   mTrackListSubscription =
      trackList.Subscribe([project](const TrackListEvent& event) {
         if (event.mType == TrackListEvent::ADDITION)
         {
            const auto track = event.mpTrack.lock();
            if (auto waveTrack = dynamic_cast<WaveTrack*>(track.get()))
            {
               const auto projectTempo =
                  ProjectTimeSignature::Get(*project).GetTempo();
               waveTrack->OnProjectTempoChange(projectTempo, projectTempo);
            }
         }
      });
   mProjectTimeSignatureSubscription = timeSignature.Subscribe(
      [project](const TimeSignatureChangedMessage& msg) {
         auto& trackList = TrackList::Get(*project);
         for (auto waveTrack : trackList.Any<WaveTrack>())
         {
            waveTrack->OnProjectTempoChange(msg.oldTempo, msg.newTempo);
         }
      });
}
