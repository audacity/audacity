#include "ProjectTempoPublisher.h"
#include "ProjectTempoListenerInterface.h"
#include "ProjectTimeSignature.h"

ProjectTempoPublisher::ProjectTempoPublisher(
   AudacityProject& project, ProjectTempoListenerInterface& listener)
    : mProject { project }
    , mListener { listener }
{
   auto& timeSignature = ProjectTimeSignature::Get(project);
   mListener.SetProjectTempo(timeSignature.GetTempo());
   mProjectTimeSignatureSubscription =
      timeSignature.Subscribe([this](const TimeSignatureChangedMessage& event) {
         mListener.SetProjectTempo(event.newTempo);
      });
}
