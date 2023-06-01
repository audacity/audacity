#pragma once

#include "ClientData.h"
#include "Observer.h"
#include "Project.h"

class STRETCHING_SAMPLE_TRACK_API ProjectTempoObserver : public ClientData::Base
{
public:
   explicit ProjectTempoObserver(AudacityProject* pOwner);

   static ProjectTempoObserver& Get(AudacityProject& project);
   static const ProjectTempoObserver& Get(const AudacityProject& project);

private:
   Observer::Subscription mProjectTimeSignatureSubscription;
   Observer::Subscription mTrackListSubscription;
};
