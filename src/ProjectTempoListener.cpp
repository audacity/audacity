#include "ClientData.h"
#include "Observer.h"
#include "Project.h"
#include "ProjectTimeSignature.h"
#include "Track.h"

#include <cassert>

class ProjectTempoListener final : public ClientData::Base
{
public:
   ProjectTempoListener(AudacityProject& project, TrackList& trackList);
   void OnProjectTempoChange(double newTempo);

private:
   TrackList& mTrackList;
   Observer::Subscription mTrackListSubstription;
   Observer::Subscription mProjectTimeSignatureSubscription;
};

static const AttachedProjectObjects::RegisteredFactory key {
   [](AudacityProject& project) {
      return std::make_shared<ProjectTempoListener>(
         project, TrackList::Get(project));
   }
};

ProjectTempoListener::ProjectTempoListener(
   AudacityProject& project, TrackList& trackList)
    : mTrackList { trackList }
    , mTrackListSubstription { trackList.Subscribe(
         [this, &project](const TrackListEvent& event) {
            if (event.mType == TrackListEvent::ADDITION)
            {
               const auto tempo = ProjectTimeSignature::Get(project).GetTempo();
               if (const auto track = event.mpTrack.lock()) {
                  // TODO wide wave tracks: just call on the track itself
                  if (auto pLeader = *mTrackList.Find(track.get()))
                     pLeader->OnProjectTempoChange(tempo);
               }
            }
         }) }
{
   mProjectTimeSignatureSubscription =
      ProjectTimeSignature::Get(project).Subscribe(
         [this](const TimeSignatureChangedMessage& event) {
            OnProjectTempoChange(event.newTempo);
         });
   assert(mTrackList.empty()); // No need to call `OnProjectTempoChange` yet ...
}

void ProjectTempoListener::OnProjectTempoChange(double newTempo)
{
   for (auto track : mTrackList)
      track->OnProjectTempoChange(newTempo);
}
