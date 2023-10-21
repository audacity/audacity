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
   mTracks.Append(std::move(list), false);
}

namespace {
// function-making function
auto finder(TrackId id, int &distance) {
   // Predicate returns true if any channel of the track's channel group has
   // the given id and as a side-effect reports the position of the channel in
   // the group
   return [id, &distance](const auto &pTrack) {
      const auto channels = TrackList::Channels(&*pTrack);
      const auto begin = channels.begin();
      const auto end = channels.end();
      const auto pred = [id](const auto &ptr) { return ptr->GetId() == id; };
      auto iter = std::find_if(begin, end, pred);
      if (iter == end) {
         distance = -1;
         return false;
      }
      else {
         distance = std::distance(begin, iter);
         return true;
      }
   };
}
}

std::shared_ptr<Track>
PendingTracks::SubstitutePendingChangedTrack(Track &track) const
{
   // Linear search.  Tracks in a project are usually very few.
   auto pTrack = &track;
   // track might not be a leader
   if (!mPendingUpdates.empty()) {
      const auto id = track.GetId();
      const auto end = mPendingUpdates.end();
      int distance{ -1 };
      // Find the leader of the group of shadow tracks containing the id
      if (const auto it =
          std::find_if(mPendingUpdates.begin(), end, finder(id, distance))
          ; it != end) {
         // Find the correct corresponding channel
         auto channelIter = TrackList::Channels(&**it).begin();
         std::advance(channelIter, distance);
         pTrack = *channelIter;
      }
   }
   return pTrack->SharedPointer();
}

std::shared_ptr<const Track>
PendingTracks::SubstitutePendingChangedTrack(const Track &track) const
{
   return SubstitutePendingChangedTrack(const_cast<Track&>(track));
}

std::shared_ptr<const Track>
PendingTracks::SubstituteOriginalTrack(const Track &track) const
{
   auto pTrack = &track;
   // track might not be a leader
   if (!mPendingUpdates.empty()) {
      const auto id = track.GetId();
      const auto end = mPendingUpdates.end();
      int distance1{ -1 };
      // Find the leader of the group of shadow tracks containing the id
      if (const auto it =
          std::find_if(mPendingUpdates.begin(), end, finder(id, distance1))
         ; it != end)
      {
         const auto end2 = mTracks.end();
         int distance2{ -1 };
         // Find the leader of the group of original tracks containing the id
         const auto it2 =
            std::find_if(mTracks.begin(), end2, finder(id, distance2));
         if (it2 != end2) {
            // This should be provable from how RegisterPendingChangedTrack
            // constructs the substitutes
            assert(distance1 == distance2);
            // Find the correct corresponding channel
            auto channelIter = TrackList::Channels(&**it2).begin();
            std::advance(channelIter, distance2);
            pTrack = *channelIter;
         }
      }
   }
   return pTrack->SharedPointer();
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

bool PendingTracks::HasPendingTracks() const
{
   if (!mPendingUpdates.empty())
      return true;
   const auto end = mTracks.end();
   return (end != std::find_if(mTracks.begin(), end, [](const Track *t){
      return t->GetId() == TrackId{};
   }));
}
