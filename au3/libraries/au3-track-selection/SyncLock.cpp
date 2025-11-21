/**********************************************************************

Audacity: A Digital Audio Editor

@file SyncLock.cpp
@brief implements sync-lock logic

Paul Licameli split from Track.cpp

**********************************************************************/

#include "SyncLock.h"

#include "PendingTracks.h"
#include "Prefs.h"
#include "Project.h"
#include "Track.h"

static const AudacityProject::AttachedObjects::RegisteredFactory
    sSyncLockStateKey{
    []( AudacityProject& project ){
        auto result = std::make_shared< SyncLockState >(project);
        return result;
    }
};

SyncLockState& SyncLockState::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get< SyncLockState >(
        sSyncLockStateKey);
}

const SyncLockState& SyncLockState::Get(const AudacityProject& project)
{
    return Get(const_cast< AudacityProject& >(project));
}

SyncLockState::SyncLockState(AudacityProject& project)
    : mProject{project}
    , mIsSyncLocked(SyncLockTracks.Read())
{
}

bool SyncLockState::IsSyncLocked() const
{
    return mIsSyncLocked;
}

void SyncLockState::SetSyncLock(bool flag)
{
    if (flag != mIsSyncLocked) {
        mIsSyncLocked = flag;
        Publish({ flag });
    }
}

namespace {
inline bool IsSyncLockableNonSeparatorTrack(const Track& track)
{
    return GetSyncLockPolicy::Call(track) == SyncLockPolicy::Grouped;
}

inline bool IsSeparatorTrack(const Track& track)
{
    return GetSyncLockPolicy::Call(track) == SyncLockPolicy::EndSeparator;
}

bool IsGoodNextSyncLockTrack(const Track& t, bool inSeparatorSection)
{
    const bool isSeparator = IsSeparatorTrack(t);
    if (inSeparatorSection) {
        return isSeparator;
    } else if (isSeparator) {
        return true;
    } else {
        return IsSyncLockableNonSeparatorTrack(t);
    }
}
}

bool SyncLock::IsSyncLockSelected(const Track& track)
{
    auto pList = track.GetOwner();
    if (!pList) {
        return false;
    }

    auto p = pList->GetOwner();
    if (!p || !SyncLockState::Get(*p).IsSyncLocked()) {
        return false;
    }

    auto& orig = PendingTracks::Get(*p).SubstituteOriginalTrack(track);
    auto trackRange = Group(orig);

    if (trackRange.size() <= 1) {
        // Not in a sync-locked group.
        // Return true iff selected and of a sync-lockable type.
        return (IsSyncLockableNonSeparatorTrack(orig)
                || IsSeparatorTrack(orig)) && track.GetSelected();
    }

    // Return true iff any track in the group is selected.
    return *(trackRange + &Track::IsSelected).begin();
}

bool SyncLock::IsSelectedOrSyncLockSelected(const Track& track)
{
    return track.IsSelected() || IsSyncLockSelected(track);
}

namespace {
std::pair<Track*, Track*> FindSyncLockGroup(Track& member)
{
    // A non-trivial sync-locked group is a maximal sub-sequence of the tracks
    // consisting of any positive number of audio tracks followed by zero or
    // more label tracks.

    // Step back through any label tracks.
    auto pList = member.GetOwner();
    auto ppMember = pList->Find(&member);
    while (*ppMember && IsSeparatorTrack(**ppMember)) {
        --ppMember;
    }

    // Step back through the wave and note tracks before the label tracks.
    Track* first = nullptr;
    while (*ppMember && IsSyncLockableNonSeparatorTrack(**ppMember)) {
        first = *ppMember;
        --ppMember;
    }

    if (!first) {
        // Can't meet the criteria described above.  In that case,
        // consider the track to be the sole member of a group.
        return { &member, &member };
    }

    auto last = pList->Find(first);
    auto next = last;
    bool inLabels = false;

    while (*++next) {
        if (!IsGoodNextSyncLockTrack(**next, inLabels)) {
            break;
        }
        last = next;
        inLabels = IsSeparatorTrack(**last);
    }

    return { first, *last };
}
}

TrackIterRange<Track> SyncLock::Group(Track& track)
{
    auto pList = track.GetOwner();
    assert(pList); // precondition
    auto tracks = FindSyncLockGroup(const_cast<Track&>(track));
    return pList->Any()
           .StartingWith(tracks.first).EndingAfter(tracks.second);
}

DEFINE_ATTACHED_VIRTUAL(GetSyncLockPolicy) {
    return [](auto&){ return SyncLockPolicy::Isolated; };
}

BoolSetting SyncLockTracks{ "/GUI/SyncLockTracks", false };
