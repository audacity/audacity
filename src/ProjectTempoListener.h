#pragma once

#include "Observer.h"
#include "ProjectTempoListenerInterface.h"

class AudacityProject;
class TrackList;

class ProjectTempoListener final : public ProjectTempoListenerInterface
{
public:
   ProjectTempoListener(AudacityProject& project, TrackList& trackList);
   void SetProjectTempo(double newTempo) override;

private:
   TrackList& mTrackList;
   Observer::Subscription mTrackListSubstription;
};
