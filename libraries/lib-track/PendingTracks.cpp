/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file PendingTracks.cpp
 
 Paul Licameli
 
 **********************************************************************/
#include "PendingTracks.h"
#include "Project.h"
#include "Track.h"

static const AudacityProject::AttachedObjects::RegisteredFactory
sPendingTracksKey{
  [](AudacityProject &project){
     return std::make_shared<PendingTracks>(project);
   }
};

PendingTracks &PendingTracks::Get(AudacityProject &project)
{
   return project.AttachedObjects::Get<PendingTracks>(sPendingTracksKey);
}

const PendingTracks &PendingTracks::Get(const AudacityProject &project)
{
   return Get(const_cast<AudacityProject &>(project));
}

PendingTracks::PendingTracks(AudacityProject &project)
   : mTracks{ TrackList::Get(project) }
   , mTrackListSubscription { mTracks.Subscribe(
      [this](const TrackListEvent &event){
         switch (event.mType) {
         case TrackListEvent::PERMUTED:
         case TrackListEvent::RESIZING:
         case TrackListEvent::ADDITION:
         case TrackListEvent::DELETION:
            UpdatePendingTracks();
            break;
         default:
            break;
         }
         // Pass along to downstream listeners
         Publish(event);
   })}
{}

PendingTracks::~PendingTracks() = default;

void PendingTracks::RegisterPendingNewTracks(TrackList &&list)
{
   mTracks.RegisterPendingNewTracks(std::move(list));
}

std::shared_ptr<Track>
PendingTracks::SubstitutePendingChangedTrack(Track &track) const
{
   return track.SubstitutePendingChangedTrack();
}

std::shared_ptr<const Track>
PendingTracks::SubstitutePendingChangedTrack(const Track &track) const
{
   return track.SubstitutePendingChangedTrack();
}

std::shared_ptr<const Track>
PendingTracks::SubstituteOriginalTrack(const Track &track) const
{
   return track.SubstituteOriginalTrack();
}

Track* PendingTracks::RegisterPendingChangedTrack(Updater updater, Track *src)
{
   return mTracks.RegisterPendingChangedTrack(move(updater), src);
}

void PendingTracks::UpdatePendingTracks()
{
   mTracks.UpdatePendingTracks();
}

void PendingTracks::ClearPendingTracks()
{
   mTracks.ClearPendingTracks();
}

bool PendingTracks::ApplyPendingTracks()
{
   std::vector<std::shared_ptr<TrackList>> additions;
   std::shared_ptr<TrackList> pendingUpdates = TrackList::Temporary(nullptr);
   {
      // Always clear, even if one of the update functions throws
      auto cleanup = finally([&]{
         mTracks.ClearPendingTracks(&additions, &pendingUpdates);
      });
      UpdatePendingTracks();
   }
   return mTracks.ApplyPendingTracks(move(additions), move(pendingUpdates));
}
