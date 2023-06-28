#pragma once

#include "Observer.h"
#include "Project.h"
#include "ProjectTempoListenerInterface.h"

#include <vector>

class NUMERIC_FORMATS_API ProjectTempoPublisher
{
public:
   ProjectTempoPublisher(
      AudacityProject& project, ProjectTempoListenerInterface& listener);

private:
   AudacityProject& mProject;
   ProjectTempoListenerInterface& mListener;
   Observer::Subscription mProjectTimeSignatureSubscription;
};
