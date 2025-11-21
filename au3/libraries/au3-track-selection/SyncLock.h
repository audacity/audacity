/**********************************************************************

Audacity: A Digital Audio Editor

@file SyncLock.cpp
@brief Defines groupings of tracks that should keep contents synchronized

Paul Licameli split from Track.cpp

**********************************************************************/
#ifndef __AUDACITY_SYNC_LOCK__
#define __AUDACITY_SYNC_LOCK__

#include "Prefs.h"
#include "Track.h" // for TrackIterRange
#include "AttachedVirtualFunction.h"
#include "Observer.h"

//! Sent after sync lock setting changes, with its new state
struct SyncLockChangeMessage {
    const bool on;
};

class TRACK_SELECTION_API SyncLockState final : public ClientData::Base, public Observer::Publisher<SyncLockChangeMessage>
{
public:
    static SyncLockState& Get(AudacityProject& project);
    static const SyncLockState& Get(const AudacityProject& project);
    explicit SyncLockState(AudacityProject& project);
    SyncLockState(const SyncLockState&) = delete;
    SyncLockState& operator=(const SyncLockState&) = delete;

    bool IsSyncLocked() const;
    void SetSyncLock(bool flag);

private:
    AudacityProject& mProject;
    bool mIsSyncLocked{ false };
};

class TRACK_SELECTION_API SyncLock
{
public:
    //! @return sync lock is on, and some member of track's group is selected
    static bool IsSyncLockSelected(const Track& track);

    //! @return pTrack is not null, sync lock is on, and some member of its
    //! group is selected
    /*!
     Useful as a predicate for track iteration, which must test a pointer
     */
    static bool IsSyncLockSelectedP(const Track* pTrack)
    { return pTrack && IsSyncLockSelected(*pTrack); }

    //! @return track is selected, or is sync-lock selected
    static bool IsSelectedOrSyncLockSelected(const Track& track);

    //! @return pTrack is not null, and is selected, or is sync-lock selected
    /*!
     Useful as a predicate for track iteration, which must test a pointer
     */
    static bool IsSelectedOrSyncLockSelectedP(const Track* pTrack)
    { return pTrack && IsSelectedOrSyncLockSelected(*pTrack); }

    /*! @pre `track.GetOwner() != nullptr` */
    static TrackIterRange<Track> Group(Track& track);

    /*! @copydoc Group */
    static TrackIterRange<const Track> Group(const Track& track)
    {
        return Group(const_cast<Track&>(track)).Filter<const Track>();
    }
};

//! Describes how a track participates in sync-lock groupings
enum class SyncLockPolicy {
    Isolated, //!< Never part of a group
    Grouped, //!< Can be part of a group
    EndSeparator, //!< Delimits the end of a group (of which it is a part)
};

struct GetSyncLockPolicyTag;

//! Describe how this track participates in sync-lock groupings; defaults to Isolated
using GetSyncLockPolicy
    =AttachedVirtualFunction<
          GetSyncLockPolicyTag,
          SyncLockPolicy,
          const Track
          >;
DECLARE_EXPORTED_ATTACHED_VIRTUAL(TRACK_SELECTION_API, GetSyncLockPolicy);

extern TRACK_SELECTION_API BoolSetting SyncLockTracks;

#endif
