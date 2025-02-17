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
    [](AudacityProject& project){
        return std::make_shared<PendingTracks>(project);
    }
};

PendingTracks& PendingTracks::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get<PendingTracks>(sPendingTracksKey);
}

const PendingTracks& PendingTracks::Get(const AudacityProject& project)
{
    return Get(const_cast<AudacityProject&>(project));
}

PendingTracks::PendingTracks(AudacityProject& project)
    : mTracks{TrackList::Get(project)}
    , mTrackListSubscription{mTracks.Subscribe(
                                 [this](const TrackListEvent& event){
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
    })},
    mPendingUpdates{ TrackList::Temporary(mTracks.GetOwner()) }
{}

PendingTracks::~PendingTracks() = default;

void PendingTracks::RegisterPendingNewTracks(TrackList&& list)
{
    mTracks.Append(std::move(list), false);
}

std::pair<Track*, Channel*>
PendingTracks::DoSubstitutePendingChangedChannel(
    Track& track, size_t channelIndex) const
{
    // Linear search.  Tracks in a project are usually very few.
    auto pTrack = &track;
    if (!mPendingUpdates->empty()) {
        const auto end = mPendingUpdates->end();
        // Find the shadow track with the id
        const auto pred = [id = track.GetId()](const auto& pTrack){
            return pTrack->GetId() == id;
        };
        if (const auto it = std::find_if(mPendingUpdates->begin(), end, pred)
            ; it != end) {
            pTrack = *it;
            // Find the correct corresponding channel
            const auto& channels = pTrack->Channels();
            const auto size = channels.size();
            // This should be provable from how RegisterPendingChangedTrack
            // constructs the substitutes
            assert(channelIndex < size);
            auto channelIter = channels.begin();
            std::advance(channelIter, std::min<int>(channelIndex, size - 1));
            return { pTrack, (*channelIter).get() };
        }
    }
    return {};
}

Channel& PendingTracks::SubstitutePendingChangedChannel(Channel& channel) const
{
    const auto pTrack = dynamic_cast<Track*>(&channel.GetChannelGroup());
    if (!pTrack) {
        return channel;
    }
    const auto index = channel.GetChannelIndex();
    auto [_, pChannel] = DoSubstitutePendingChangedChannel(*pTrack, index);
    return pChannel ? *pChannel : channel;
}

const Channel&
PendingTracks::SubstitutePendingChangedChannel(const Channel& channel) const
{
    return SubstitutePendingChangedChannel(const_cast<Channel&>(channel));
}

Track& PendingTracks::SubstitutePendingChangedTrack(Track& track) const
{
    auto [pTrack, _] = DoSubstitutePendingChangedChannel(track, 0);
    return pTrack ? *pTrack : track;
}

const Track& PendingTracks::SubstitutePendingChangedTrack(const Track& track)
const
{
    return SubstitutePendingChangedTrack(const_cast<Track&>(track));
}

std::pair<const Track*, const Channel*>
PendingTracks::DoSubstituteOriginalChannel(
    const Track& track, size_t channelIndex) const
{
    auto pTrack = &track;
    if (!mPendingUpdates->empty()) {
        const auto end = mPendingUpdates->end();
        // Find the shadow track with the id
        const auto pred = [id = track.GetId()](const auto& pTrack){
            return pTrack->GetId() == id;
        };
        if (const auto it
                =std::find_if(mPendingUpdates->begin(), end, pred); it != end) {
            const auto end2 = mTracks.end();
            // Find the original track with the id
            if (const auto it2 = std::find_if(mTracks.begin(), end2, pred)
                ; it2 != end2) {
                pTrack = *it2;
                // Find the correct corresponding channel
                const auto& channels = pTrack->Channels();
                const auto size = channels.size();
                // This should be provable from how RegisterPendingChangedTrack
                // constructs the substitutes
                assert(channelIndex < size);

                auto channelIter = channels.begin();
                std::advance(channelIter, std::min<int>(channelIndex, size - 1));
                return { pTrack, (*channelIter).get() };
            }
        }
    }
    return {};
}

const Channel& PendingTracks::SubstituteOriginalChannel(const Channel& channel)
const
{
    const auto pTrack
        =dynamic_cast<const Track*>(&channel.GetChannelGroup());
    if (!pTrack) {
        return channel;
    }
    const auto index = channel.GetChannelIndex();
    const auto [_, pChannel] = DoSubstituteOriginalChannel(*pTrack, index);
    return pChannel ? *pChannel : channel;
}

const Track& PendingTracks::SubstituteOriginalTrack(const Track& track) const
{
    const auto [pTrack, _] = DoSubstituteOriginalChannel(track, 0);
    return pTrack ? *pTrack : track;
}

Track* PendingTracks::RegisterPendingChangedTrack(Updater updater, Track* src)
{
    auto track
        =src->Duplicate(Track::DuplicateOptions {}.ShallowCopyAttachments());

    mUpdaters.push_back(move(updater));
    mPendingUpdates->Add(track);
    return track.get();
}

void PendingTracks::UpdatePendingTracks()
{
    if (mPendingUpdates->empty()) {
        return;
    }
    auto pUpdater = mUpdaters.begin();
    for (const auto& pendingTrack : *mPendingUpdates) {
        auto src = mTracks.FindById(pendingTrack->GetId());
        // Copy just a part of the track state, according to the update
        // function
        const auto& updater = *pUpdater;
        if (pendingTrack && src) {
            if (updater) {
                updater(*pendingTrack, *src);
            }
        }
        ++pUpdater;
    }
}

/*! @excsafety{No-fail} */
void PendingTracks::ClearPendingTracks(
    std::vector<std::shared_ptr<Track> >* pAdded)
{
    mUpdaters.clear();
    mPendingUpdates->Clear();

    if (pAdded) {
        pAdded->clear();
    }

    auto [it, end] = mTracks.Any();
    while (it != end) {
        const auto pTrack = *it;
        ++it;
        if (pTrack->GetId() == TrackId{}) {
            if (pAdded) {
                pAdded->emplace_back(mTracks.Remove(*pTrack));
            }
        } else {
            if (pAdded) {
                pAdded->push_back(nullptr);
            }
        }
    }

    if (pAdded) {
        // Remove trailing nulls
        while (!pAdded->empty() && !pAdded->back()) {
            pAdded->pop_back();
        }
    }
}

/*! @excsafety{Strong} */
bool PendingTracks::ApplyPendingTracks()
{
    std::vector<std::shared_ptr<Track> > additions;
    auto updated = TrackList::Temporary(mTracks.GetOwner());
    {
        // Always clear, even if one of the update functions throws
        Finally Do{ [&]{ ClearPendingTracks(&additions); } };
        UpdatePendingTracks();
        // Clear updaters before any more track list events are processed
        mUpdaters.clear();
        updated.swap(mPendingUpdates);
    }

    bool result = false;

    // Remaining steps must be No-fail-guarantee so that this function
    // gives Strong-guarantee

    std::vector<std::shared_ptr<Track> > reinstated;

    for (const auto pendingTrack : *updated) {
        pendingTrack->ReparentAllAttachments();
    }

    while (!updated->empty()) {
        auto iter = updated->begin();
        auto pendingTrack = *iter;
        auto src = mTracks.FindById(pendingTrack->GetId());
        if (src) {
            mTracks.ReplaceOne(*src, std::move(*updated));
            result = true;
        } else {
            // Perhaps a track marked for pending changes got deleted by
            // some other action.  Recreate it so we don't lose the
            // accumulated changes.
            reinstated.push_back(pendingTrack->SharedPointer());
            updated->Remove(*pendingTrack);
        }
    }

    // If there are tracks to reinstate, append them to the list.
    for (auto& pendingTrack : reinstated) {
        if (pendingTrack) {
            mTracks.Add(move(pendingTrack)), result = true;
        }
    }

    // Put the pending added tracks back into the list, preserving their
    // positions and assigning ids.
    auto iter = mTracks.begin();
    for (auto& pendingTrack : additions) {
        auto next = iter;
        ++next;
        if (pendingTrack) {
            // This emits appropriate track list events
            mTracks.Insert(*iter, pendingTrack, true);
        } else {
            assert(iter != mTracks.end()); // Deduce that from ClearPendingTrack
        }
        iter = next;
    }

    return result;
}

bool PendingTracks::HasPendingTracks() const
{
    if (!mPendingUpdates->empty()) {
        return true;
    }
    const auto end = mTracks.end();
    return end != std::find_if(mTracks.begin(), end, [](const Track* t){
        return t->GetId() == TrackId {};
    });
}
