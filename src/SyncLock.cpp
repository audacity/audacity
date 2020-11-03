/**********************************************************************

Audacity: A Digital Audio Editor

@file SyncLock.cpp
@brief implements sync-lock logic

Paul Licameli split from Track.cpp

**********************************************************************/

#include "SyncLock.h"

#include "ProjectSettings.h"
#include "Track.h"

namespace {
inline bool IsSyncLockableNonSeparatorTrack( const Track *pTrack )
{
   return pTrack && GetSyncLockPolicy::Call(*pTrack) == SyncLockPolicy::Grouped;
}

inline bool IsSeparatorTrack( const Track *pTrack )
{
   return pTrack &&
      GetSyncLockPolicy::Call(*pTrack) == SyncLockPolicy::EndSeparator;
}

bool IsGoodNextSyncLockTrack(const Track *t, bool inSeparatorSection)
{
   if (!t)
      return false;
   const bool isSeparator = IsSeparatorTrack(t);
   if (inSeparatorSection)
      return isSeparator;
   else if (isSeparator)
      return true;
   else
      return IsSyncLockableNonSeparatorTrack( t );
}
}

bool SyncLock::IsSyncLockSelected( const Track *pTrack )
{
   if (!pTrack)
      return false;
   auto pList = pTrack->GetOwner();
   if (!pList)
      return false;

   auto p = pList->GetOwner();
   if (!p || !ProjectSettings::Get( *p ).IsSyncLocked())
      return false;

   auto shTrack = pTrack->SubstituteOriginalTrack();
   if (!shTrack)
      return false;

   const auto pOrigTrack = shTrack.get();
   auto trackRange = Group( pOrigTrack );

   if (trackRange.size() <= 1) {
      // Not in a sync-locked group.
      // Return true iff selected and of a sync-lockable type.
      return (IsSyncLockableNonSeparatorTrack( pOrigTrack ) ||
              IsSeparatorTrack( pOrigTrack )) && pTrack->GetSelected();
   }

   // Return true iff any track in the group is selected.
   return *(trackRange + &Track::IsSelected).begin();
}

bool SyncLock::IsSelectedOrSyncLockSelected( const Track *pTrack )
{
   return pTrack && (pTrack->IsSelected() || IsSyncLockSelected(pTrack));
}

namespace {
std::pair<Track *, Track *> FindSyncLockGroup(  Track *pMember)
{
   if (!pMember)
      return { nullptr, nullptr };

   // A non-trivial sync-locked group is a maximal sub-sequence of the tracks
   // consisting of any positive number of audio tracks followed by zero or
   // more label tracks.

   // Step back through any label tracks.
   auto pList = pMember->GetOwner();
   auto member = pList->Find(pMember);
   while (*member && IsSeparatorTrack(*member) )
      --member;

   // Step back through the wave and note tracks before the label tracks.
   Track *first = nullptr;
   while (*member && IsSyncLockableNonSeparatorTrack(*member)) {
      first = *member;
      --member;
   }

   if (!first)
      // Can't meet the criteria described above.  In that case,
      // consider the track to be the sole member of a group.
      return { pMember, pMember };

   auto last = pList->Find(first);
   auto next = last;
   bool inLabels = false;

   while (*++next) {
      if ( ! IsGoodNextSyncLockTrack(*next, inLabels) )
         break;
      last = next;
      inLabels = IsSeparatorTrack(*last);
   }

   return { first, *last };
}

}

TrackIterRange< Track > SyncLock::Group( Track *pTrack )
{
   auto pList = pTrack->GetOwner();
   auto tracks = FindSyncLockGroup(const_cast<Track*>( pTrack ) );
   return pList->Any().StartingWith(tracks.first).EndingAfter(tracks.second);
}

DEFINE_ATTACHED_VIRTUAL(GetSyncLockPolicy) {
   return [](auto&){ return SyncLockPolicy::Isolated; };
}
