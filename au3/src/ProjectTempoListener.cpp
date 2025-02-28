#include "ClientData.h"
#include "Observer.h"
#include "Project.h"
#include "ProjectTimeSignature.h"
#include "TempoChange.h"
#include "Track.h"

#include <cassert>

#include "ViewInfo.h"

class ProjectTempoListener final : public ClientData::Base
{
public:
    ProjectTempoListener(AudacityProject& project, TrackList& trackList);
    void OnProjectTempoChange(double newTempo);

private:
    AudacityProject& mProject;
    ViewInfo& mViewInfo;
    double mTempo { 0 };
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
    : mProject{project}
    , mViewInfo{ViewInfo::Get(project)}
    , mTempo{ProjectTimeSignature::Get(project).GetTempo()}
    , mTrackList{trackList}
    , mTrackListSubstription{trackList.Subscribe(
                                 [this](const TrackListEvent& event) {
        if (event.mType == TrackListEvent::ADDITION) {
            const auto tempo = ProjectTimeSignature::Get(mProject).GetTempo();
            if (const auto track = event.mpTrack.lock())
                DoProjectTempoChange(*track, tempo);
        }
    })}
{
    mProjectTimeSignatureSubscription
        =ProjectTimeSignature::Get(project).Subscribe(
              [this](const TimeSignatureChangedMessage& event) {
        OnProjectTempoChange(event.newTempo);
    });
    assert(mTrackList.empty()); // No need to call `OnProjectTempoChange` yet ...
}

void ProjectTempoListener::OnProjectTempoChange(double newTempo)
{
    for (auto track : mTrackList) {
        DoProjectTempoChange(*track, newTempo);
    }

    if (!mViewInfo.playRegion.Empty() && mTempo > 0 && newTempo > 0) {
        const auto tempoRate = mTempo / newTempo;
        mViewInfo.playRegion.SetTimes(
            mViewInfo.playRegion.GetStart() * tempoRate,
            mViewInfo.playRegion.GetEnd() * tempoRate
            );
    }
    mTempo = newTempo;
}
