/**********************************************************************

 Audacity: A Digital Audio Editor

 @file PendingTracks.h

 Paul Licameli

 **********************************************************************/
#ifndef __AUDACITY_PENDING_TRACKS__
#define __AUDACITY_PENDING_TRACKS__

#include "ClientData.h"
#include "Observer.h"

class AudacityProject;
class Channel;
class Track;
class TrackId;
class TrackList;
struct TrackListEvent;

class TRACK_API PendingTracks final : public ClientData::Base, public Observer::Publisher<TrackListEvent>
{
public:
    static PendingTracks& Get(AudacityProject& project);
    static const PendingTracks& Get(const AudacityProject& project);

    explicit PendingTracks(AudacityProject& project);
    PendingTracks(const PendingTracks&) = delete;
    PendingTracks& operator=(const PendingTracks&) = delete;
    ~PendingTracks();

    //! Like RegisterPendingChangedTrack, but for a list of new tracks,
    //! not a replacement track.
    /*!
     Caller supplies the list, and there are no updates.
     Pending tracks will have an unassigned TrackId.
     Pending new tracks WILL occur in iterations, always after actual
     tracks, and in the sequence that they were added.  They can be
     distinguished from actual tracks because they have default TrackId.
     */
    void RegisterPendingNewTracks(TrackList&& list);

    //! Find anything registered with TrackList::RegisterPendingChangedTrack and
    //! not yet cleared or applied; if no such exists, return the given channel
    Channel& SubstitutePendingChangedChannel(Channel& channel) const;
    const Channel& SubstitutePendingChangedChannel(const Channel& channel) const;

    //! Find anything registered with TrackList::RegisterPendingChangedTrack and
    //! not yet cleared or applied; if no such exists, return the given track
    Track& SubstitutePendingChangedTrack(Track& track) const;
    const Track& SubstitutePendingChangedTrack(const Track& track) const;

    //! If the channel is in a pending changed track, return the corresponding
    //! original; else return the channel
    const Channel& SubstituteOriginalChannel(const Channel& channel) const;

    //! If the track is a pending changed track, return the corresponding
    //! original; else return the track
    const Track& SubstituteOriginalTrack(const Track& track) const;

    //! The tracks supplied to this function will have the same number of
    //! channels
    using Updater = std::function<void (Track& dest, const Track& src)>;

    //! Start a deferred update of the project.
    //
    /*!
     The return value is a duplicate of the given track.
     While ApplyPendingTracks or ClearPendingTracks is not yet called,
     there may be other direct changes to the project that push undo history.
     Meanwhile the returned object can accumulate other changes for a deferred
     push, and temporarily shadow the actual project track for display purposes.
     The Updater function, if not null, merges state (from the actual project
     into the pending track) which is not meant to be overridden by the
     accumulated pending changes.
     Pending track will have the same TrackId as the actual.
     Pending changed tracks will not occur in iterations.
     */
    Track* RegisterPendingChangedTrack(
        Updater updater, Track* src);

    //! Invoke the updaters of pending tracks.  Pass any exceptions from the
    //! updater functions.
    void UpdatePendingTracks();

    //! Forget pending track additions and changes;
    /*!
     if requested, give back the pending added tracks, as channel groups,
     stored in the vector at their original positions in iteration order and
     nulls corresponding with non-added tracks in original iteration order; no
     trailing nulls
    */
    void ClearPendingTracks(
        std::vector<std::shared_ptr<Track> >* pAdded = nullptr);

    //! Change the state of the project.
    //
    /*!
     Strong guarantee for project state in case of exceptions.
     Will always clear the pending updates.
     Return true if the state of the track list really did change.
     */
    bool ApplyPendingTracks();

    bool HasPendingTracks() const;

private:
    std::pair<Track*, Channel*>
    DoSubstitutePendingChangedChannel(Track& track, size_t channelIndex) const;

    std::pair<const Track*, const Channel*>
    DoSubstituteOriginalChannel(const Track& track, size_t channelIndex) const;

    TrackList& mTracks;
    Observer::Subscription mTrackListSubscription;
    std::vector<Updater> mUpdaters;
    std::shared_ptr<TrackList> mPendingUpdates;
};

#endif
