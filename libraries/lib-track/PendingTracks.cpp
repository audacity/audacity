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
   mUpdaters.push_back(move(updater));
   auto result = mTracks.RegisterPendingChangedTrack(src);
   mPendingUpdates.push_back(result->SharedPointer());
   return result;
}

void PendingTracks::UpdatePendingTracks()
{
   if (mPendingUpdates.empty())
      return;
   auto pUpdater = mUpdaters.begin();
   for (const auto &pendingTrack : mPendingUpdates) {
      auto src = mTracks.FindById(pendingTrack->GetId());
      // Copy just a part of the track state, according to the update
      // function
      const auto &updater = *pUpdater;
      if (pendingTrack && src) {
         if (updater)
            updater(*pendingTrack, *src);
      }
      ++pUpdater;
   }
}

void PendingTracks::ClearPendingTracks()
{
   mUpdaters.clear();
   mPendingUpdates.clear();
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
      // Clear updaters before any more track list events are processed
      mUpdaters.clear();
      mPendingUpdates.clear();
   }
   return mTracks.ApplyPendingTracks(move(additions), move(pendingUpdates));
}
