/**********************************************************************

  Audacity: A Digital Audio Editor

  Track.cpp

  Dominic Mazzoni

*******************************************************************//**

\class Track
\brief Fundamental data object of Audacity, displayed in the TrackPanel.
Classes derived form it include the WaveTrack, NoteTrack, LabelTrack
and TimeTrack.

*//*******************************************************************/
#include "Track.h"

#include <algorithm>
#include <cassert>
#include <numeric>

#include <float.h>
#include <wx/file.h>
#include <wx/textfile.h>
#include <wx/log.h>

#include "BasicUI.h"
#include "Project.h"

#include "InconsistencyException.h"

#ifdef _MSC_VER
//Disable truncation warnings
#pragma warning( disable : 4786 )
#endif

Track::Track()
{
}

Track::Track(const Track& orig, ProtectedCreationArg&&)
{
}

// Copy all the track properties except the actual contents
void Track::Init(const Track& orig)
{
    mId = orig.mId;
    ChannelGroupAttachments& base = *this;
    // Intentional slice assignment deep-copies the attachment array:
    base = orig;
    CopyGroupProperties(orig);
    mLinkType = orig.mLinkType;
}

void Track::ReparentAllAttachments()
{
    this->AttachedTrackObjects::ForEach([&](auto& attachment){
        attachment.Reparent(this->SharedPointer());
    });
}

const wxString& Track::GetName() const
{
    return mName;
}

void Track::SetName(const wxString& n)
{
    auto& name = mName;
    if (name != n) {
        name = n;
        Notify(true);
    }
}

bool Track::GetSelected() const
{
    return mSelected;
}

void Track::SetSelected(bool s)
{
    auto& selected = mSelected;
    if (selected != s) {
        selected = s;
        auto pList = mList.lock();
        if (pList) {
            pList->SelectionEvent(*this);
        }
    }
}

void Track::CopyAttachments(Track& dst, const Track& src, bool deep)
{
    if (!deep) {
        // Share the satellites with the original, though they do not point
        // back to the duplicate track
        AttachedTrackObjects& attachments = dst;
        attachments = src; // shallow copy
    } else {
        src.AttachedTrackObjects::ForEach([&](auto& attachment){
            // Copy view state that might be important to undo/redo
            attachment.CopyTo(dst);
        });
    }
}

auto Track::Duplicate(DuplicateOptions options) const -> Holder
{
    // invoke "virtual constructor" to copy track object proper:
    auto result = Clone(options.backup);
    CopyAttachments(*result, *this, !options.shallowCopyAttachments);
    return result;
}

Track::~Track()
{
}

TrackNodePointer Track::GetNode() const
{
    return mNode;
}

void Track::SetOwner
    (const std::weak_ptr<TrackList>& list, TrackNodePointer node)
{
    // BUG: When using this function to clear an owner, we may need to clear
    // focused track too.  Otherwise focus could remain on an invisible (or deleted) track.
    mList = list;
    mNode = node;
}

void Track::SetLinkType(LinkType linkType, bool completeList)
{
    DoSetLinkType(linkType, completeList);
    if (const auto pList = mList.lock()) {
        pList->RecalcPositions(mNode);
        pList->ResizingEvent(mNode);
    }
}

void Track::CopyGroupProperties(const Track& other)
{
    mName = other.mName;
    mSelected = other.mSelected;
}

void Track::DoSetLinkType(LinkType linkType, bool completeList)
{
    auto oldType = GetLinkType();
    if (linkType == oldType) {
        // No change
        return;
    }

    if (oldType == LinkType::None) {
        // Becoming linked

        // First ensure that the previous does not link to this
        if (auto partner = GetLinkedTrack()) {
            partner->mLinkType = LinkType::None;
        }
        assert(!GetLinkedTrack());

        // Change my link type
        mLinkType = linkType;

        // Keep link consistency, while still in un-zipped state
        if (auto partner = GetLinkedTrack()) {
            partner->mLinkType = LinkType::None;
            partner->CopyGroupProperties(*this);
        }
    } else if (linkType == LinkType::None) {
        // Becoming unlinked
        if (HasLinkedTrack()) {
            if (auto partner = GetLinkedTrack()) {
                // Make independent copy of group data in the partner, which should
                // have had none
                ChannelGroupAttachments& base = *partner;
                // Intentional slice assignment deep-copies the attachment array:
                base = *this;
                partner->CopyGroupProperties(*this);
                partner->mLinkType = LinkType::None;
            }
        }
        mLinkType = LinkType::None;
    } else {
        // Remaining linked, changing the type
        mLinkType = linkType;
    }

    // Assertion checks only in a debug build, does not have side effects!
    assert(!completeList || LinkConsistencyCheck());
}

Track* Track::GetLinkedTrack() const
{
    auto pList = GetOwner();
    if (!pList) {
        return nullptr;
    }

    if (!pList->isNull(mNode)) {
        if (HasLinkedTrack()) {
            auto next = pList->getNext(mNode);
            if (!pList->isNull(next)) {
                return next->get();
            }
        }

        if (mNode != pList->ListOfTracks::begin()) {
            auto prev = pList->getPrev(mNode);
            if (!pList->isNull(prev)) {
                auto track = prev->get();
                if (track && track->HasLinkedTrack()) {
                    return track;
                }
            }
        }
    }

    return nullptr;
}

bool Track::HasLinkedTrack() const noexcept
{
    return mLinkType != LinkType::None;
}

std::optional<TranslatableString> Track::GetErrorOpening() const
{
    return {};
}

void Track::Notify(bool allChannels, int code)
{
    auto pList = mList.lock();
    if (pList) {
        pList->DataEvent(SharedPointer(), allChannels, code);
    }
}

void Track::SyncLockAdjust(double oldT1, double newT1)
{
    const auto endTime = GetEndTime();
    if (newT1 > oldT1 && oldT1 > endTime) {
        return;
    }
    if (newT1 > oldT1) {
        auto cutChannels = Cut(oldT1, endTime);
        Paste(newT1, *cutChannels);
    } else if (newT1 < oldT1) {
        // Remove from the track
        Clear(newT1, oldT1);
    }
}

bool Track::Any() const
{ return true; }

bool Track::IsSelected() const
{ return GetSelected(); }

bool Track::IsLeader() const
{
    return !GetLinkedTrack() || HasLinkedTrack();
}

bool Track::LinkConsistencyFix(bool doFix)
{
    assert(!doFix || IsLeader());
    // Sanity checks for linked tracks; unsetting the linked property
    // doesn't fix the problem, but it likely leaves us with orphaned
    // sample blocks instead of much worse problems.
    bool err = false;
    if (HasLinkedTrack()) {
        if (auto link = GetLinkedTrack()) {
            // A linked track's partner should never itself be linked
            if (link->HasLinkedTrack()) {
                err = true;
                if (doFix) {
                    wxLogWarning(
                        L"Left track %s had linked right track %s with extra right "
                        "track link.\n   Removing extra link from right track.",
                        GetName(), link->GetName());
                    link->SetLinkType(LinkType::None);
                }
            }
        } else {
            err = true;
            if (doFix) {
                wxLogWarning(
                    L"Track %s had link to NULL track. Setting it to not be linked.",
                    GetName());
                SetLinkType(LinkType::None);
            }
        }
    }
    return !err;
}

// TrackList
//
// The TrackList sends events whenever certain updates occur to the list it
// is managing.  Any other classes that may be interested in get these updates
// should use TrackList::Subscribe().
//

// same value as in the default constructed TrackId:
long TrackList::sCounter = -1;

static const AudacityProject::AttachedObjects::RegisteredFactory key{
    [](AudacityProject& project) { return TrackList::Create(&project); }
};

TrackList& TrackList::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get< TrackList >(key);
}

const TrackList& TrackList::Get(const AudacityProject& project)
{
    return Get(const_cast< AudacityProject& >(project));
}

TrackList::TrackList(AudacityProject* pOwner)
    : mOwner{pOwner}
{
}

// Factory function
TrackListHolder TrackList::Create(AudacityProject* pOwner)
{
    return std::make_shared<TrackList>(pOwner);
}

#if 0
TrackList& TrackList::operator=(TrackList&& that)
{
    if (this != &that) {
        this->Clear();
        Swap(that);
    }
    return *this;
}

#endif

void TrackList::Swap(TrackList& that)
{
    auto SwapLOTs = [](
        ListOfTracks& a, const std::weak_ptr< TrackList >& aSelf,
        ListOfTracks& b, const std::weak_ptr< TrackList >& bSelf )
    {
        a.swap(b);
        for (auto it = a.begin(), last = a.end(); it != last; ++it) {
            (*it)->SetOwner(aSelf, it);
        }
        for (auto it = b.begin(), last = b.end(); it != last; ++it) {
            (*it)->SetOwner(bSelf, it);
        }
    };

    const auto self = shared_from_this();
    const auto otherSelf = that.shared_from_this();
    SwapLOTs(*this, self, that, otherSelf);
}

TrackList::~TrackList()
{
    Clear(false);
}

wxString TrackList::MakeUniqueTrackName(const wxString& baseTrackName) const
{
    int n = 1;
    while (true) {
        auto name = wxString::Format("%s %d", baseTrackName, n++);

        bool found { false };
        for (const auto track : Tracks<const Track>()) {
            if (track->GetName() == name) {
                found = true;
                break;
            }
        }
        if (!found) {
            return name;
        }
    }
}

void TrackList::RecalcPositions(TrackNodePointer node)
{
    if (isNull(node)) {
        return;
    }

    Track* t;

    auto prev = getPrev(node);
    if (!isNull(prev)) {
        t = prev->get();
    }

    const auto theEnd = End();
    for (auto n = DoFind(node->get()); n != theEnd; ++n) {
        t = *n;
    }
}

void TrackList::QueueEvent(TrackListEvent event)
{
    BasicUI::CallAfter([wThis = weak_from_this(), event = std::move(event)]{
        if (auto pThis = wThis.lock()) {
            pThis->Publish(event);
        }
    });
}

void TrackList::SelectionEvent(Track& track)
{
    for (auto channel : Channels(&track)) {
        QueueEvent({
            TrackListEvent::SELECTION_CHANGE, channel->shared_from_this() });
    }
}

void TrackList::DataEvent(
    const std::shared_ptr<Track>& pTrack, bool allChannels, int code)
{
    auto doQueueEvent = [this, code](const std::shared_ptr<Track>& theTrack){
        QueueEvent({ TrackListEvent::TRACK_DATA_CHANGE, theTrack, code });
    };
    if (allChannels) {
        for (auto channel : Channels(pTrack.get())) {
            doQueueEvent(channel->shared_from_this());
        }
    } else {
        doQueueEvent(pTrack);
    }
}

void TrackList::PermutationEvent(TrackNodePointer node)
{
    QueueEvent({ TrackListEvent::PERMUTED, *node });
}

void TrackList::DeletionEvent(std::weak_ptr<Track> node, bool duringReplace)
{
    QueueEvent(
        { TrackListEvent::DELETION, std::move(node), duringReplace ? 1 : 0 });
}

void TrackList::AdditionEvent(TrackNodePointer node, EventPublicationSynchrony synchrony)
{
    if (synchrony == EventPublicationSynchrony::Synchronous) {
        Publish({ TrackListEvent::ADDITION, *node });
    } else {
        QueueEvent({ TrackListEvent::ADDITION, *node });
    }
}

void TrackList::ResizingEvent(TrackNodePointer node)
{
    QueueEvent({ TrackListEvent::RESIZING, *node });
}

auto TrackList::EmptyRange() const
-> TrackIterRange< Track >
{
    auto it = const_cast<TrackList*>(this)->getEnd();
    return {
        { it, it, it, &Track::Any },
        { it, it, it, &Track::Any }
    };
}

auto TrackList::DoFind(Track* pTrack) -> TrackIter<Track>
{
    if (!pTrack || pTrack->GetOwner().get() != this) {
        return EndIterator<Track>();
    } else {
        return MakeTrackIterator<Track>(pTrack->GetNode());
    }
}

auto TrackList::Find(Track* pTrack) -> TrackIter<Track>
{
    auto iter = DoFind(pTrack);
    while (*iter && !(*iter)->IsLeader()) {
        --iter;
    }
    return iter.Filter(&Track::IsLeader);
}

void TrackList::Insert(const Track* before,
                       const Track::Holder& pSrc, bool assignIds)
{
    assert(before == nullptr || Find(before) != EndIterator<const Track>());

    if (before == nullptr) {
        Add(pSrc, assignIds ? DoAssignId::Yes : DoAssignId::No);
        return;
    }

    std::vector<Track*> arr;
    arr.reserve(Size() + 1);
    for (const auto track : *this) {
        if (track == before) {
            arr.push_back(pSrc.get());
        }
        arr.push_back(track);
    }
    Add(pSrc, assignIds ? DoAssignId::Yes : DoAssignId::No);
    Permute(arr);
}

void TrackList::Permute(const std::vector<Track*>& tracks)
{
    std::vector<TrackNodePointer> permutation;
    for (const auto pTrack : tracks) {
        for (const auto pChannel : Channels(pTrack)) {
            permutation.push_back(pChannel->GetNode());
        }
    }
    for (const auto iter : permutation) {
        ListOfTracks::value_type track = *iter;
        erase(iter);
        Track* pTrack = track.get();
        pTrack->SetOwner(shared_from_this(), insert(ListOfTracks::end(), track));
    }
    auto n = getBegin();
    RecalcPositions(n);
    PermutationEvent(n);
}

Track* TrackList::FindById(TrackId id)
{
    // Linear search.  Tracks in a project are usually very few.
    // Search only the non-pending tracks.
    auto it = std::find_if(ListOfTracks::begin(), ListOfTracks::end(),
                           [=](const ListOfTracks::value_type& ptr){ return ptr->GetId() == id; });
    if (it == ListOfTracks::end()) {
        return {};
    }
    return it->get();
}

Track* TrackList::DoAddToHead(const std::shared_ptr<Track>& t)
{
    Track* pTrack = t.get();
    push_front(ListOfTracks::value_type(t));
    auto n = getBegin();
    pTrack->SetOwner(shared_from_this(), n);
    pTrack->SetId(TrackId { ++sCounter });
    RecalcPositions(n);
    AdditionEvent(n, EventPublicationSynchrony::Asynchronous);
    return front().get();
}

Track* TrackList::DoAdd(
    const std::shared_ptr<Track>& t, DoAssignId assignId,
    EventPublicationSynchrony synchrony)
{
    if (!ListOfTracks::empty()) {
        auto& pLast = *ListOfTracks::rbegin();
        if (pLast->mLinkType != Track::LinkType::None) {
            t->CopyGroupProperties(*pLast);
        }
    }

    push_back(t);

    auto n = getPrev(getEnd());

    t->SetOwner(shared_from_this(), n);
    if (mAssignsIds && assignId == DoAssignId::Yes) {
        t->SetId(TrackId { ++sCounter });
    }
    RecalcPositions(n);
    AdditionEvent(n, synchrony);
    return back().get();
}

Track::Holder TrackList::ReplaceOne(Track& t, TrackList&& with)
{
    assert(t.GetOwner().get() == this);
    assert(!with.empty());

    auto save = t.shared_from_this();

    //! Move one track to the temporary list
    auto node = t.GetNode();
    t.SetOwner({}, {});

    //! Redirect the list element of this
    const auto iter = with.ListOfTracks::begin();
    const auto pTrack = *iter;
    *node = pTrack;
    with.erase(iter);
    pTrack->SetOwner(shared_from_this(), node);
    pTrack->SetId(save->GetId());
    RecalcPositions(node);
    DeletionEvent(save, true);
    AdditionEvent(node, EventPublicationSynchrony::Asynchronous);
    return save;
}

std::shared_ptr<Track> TrackList::Remove(Track& track)
{
    auto* t = &track;
    auto iter = getEnd();
    auto node = t->GetNode();
    t->SetOwner({}, {});

    std::shared_ptr<Track> holder;
    if (!isNull(node)) {
        holder = *node;

        iter = getNext(node);
        erase(node);
        if (!isNull(iter)) {
            RecalcPositions(iter);
        }

        DeletionEvent(t->shared_from_this(), false);
    }
    return holder;
}

void TrackList::BeginUndoRedo(EventPublicationSynchrony synchrony)
{
    if (synchrony == EventPublicationSynchrony::Asynchronous) {
        QueueEvent({ TrackListEvent::UNDO_REDO_BEGIN });
    } else {
        Publish({ TrackListEvent::UNDO_REDO_BEGIN });
    }
}

void TrackList::EndUndoRedo(EventPublicationSynchrony synchrony)
{
    if (synchrony == EventPublicationSynchrony::Asynchronous) {
        QueueEvent({ TrackListEvent::UNDO_REDO_END });
    } else {
        Publish({ TrackListEvent::UNDO_REDO_END });
    }
}

void TrackList::Clear(bool sendEvent)
{
    // Null out the back-pointers to this in tracks, in case there
    // are outstanding shared_ptrs to those tracks, making them outlive
    // the temporary ListOfTracks below.
    for (auto pTrack: Tracks<Track>()) {
        pTrack->SetOwner({}, {});

        if (sendEvent) {
            DeletionEvent(pTrack->shared_from_this(), false);
        }
    }

    ListOfTracks tempList;
    tempList.swap(*this);
}

/// Return a track in the list that comes after Track t
Track* TrackList::GetNext(Track& t, bool linked) const
{
    auto node = t.GetNode();
    if (!isNull(node)) {
        if (linked && t.HasLinkedTrack()) {
            node = getNext(node);
        }

        if (!isNull(node)) {
            node = getNext(node);
        }

        if (!isNull(node)) {
            return node->get();
        }
    }
    return nullptr;
}

Track* TrackList::GetPrev(Track& t, bool linked) const
{
    TrackNodePointer prev;
    auto node = t.GetNode();
    if (!isNull(node)) {
        // linked is true and input track second in team?
        if (linked) {
            prev = getPrev(node);
            if (!isNull(prev)
                && !t.HasLinkedTrack() && t.GetLinkedTrack()) {
                // Make it the first
                node = prev;
            }
        }

        prev = getPrev(node);
        if (!isNull(prev)) {
            // Back up once
            node = prev;

            // Back up twice sometimes when linked is true
            if (linked) {
                prev = getPrev(node);
                if (!isNull(prev)
                    && !(*node)->HasLinkedTrack()
                    && (*node)->GetLinkedTrack()) {
                    node = prev;
                }
            }

            return node->get();
        }
    }
    return nullptr;
}

bool TrackList::CanMoveUp(Track& t) const
{
    return GetPrev(t, true) != nullptr;
}

bool TrackList::CanMoveDown(Track& t) const
{
    return GetNext(t, true) != nullptr;
}

// This is used when you want to swap the track at s1 with the track at s2.
// The complication is that the tracks are stored in a single
// linked list.
void TrackList::SwapNodes(TrackNodePointer s1, TrackNodePointer s2)
{
    // if a null pointer is passed in, we want to know about it
    wxASSERT(!isNull(s1));
    wxASSERT(!isNull(s2));

    // Safety check...
    if (s1 == s2) {
        return;
    }

    // Be sure s1 is the earlier iterator
    {
        const auto begin = ListOfTracks::begin();
        auto d1 = std::distance(begin, s1);
        auto d2 = std::distance(begin, s2);
        if (d1 > d2) {
            std::swap(s1, s2);
        }
    }

    // For saving the removed tracks
    using Saved = ListOfTracks::value_type;
    Saved saved1, saved2;

    auto doSave = [&](Saved& saved, TrackNodePointer& s) {
        saved = *s, s = erase(s);
    };

    doSave(saved1, s1);
    // The two ranges are assumed to be disjoint but might abut
    const bool same = (s1 == s2);
    doSave(saved2, s2);
    if (same) {
        // Careful, we invalidated s1 in the second doSave!
        s1 = s2;
    }

    // Reinsert them
    auto doInsert = [&](Saved& saved, TrackNodePointer& s) {
        const auto pTrack = saved.get();
        // Insert before s, and reassign s to point at the new node before
        // old s; which is why we saved pointers in backwards order
        pTrack->SetOwner(shared_from_this(), s = insert(s, saved));
    };
    // This does not invalidate s2 even when it equals s1:
    doInsert(saved2, s1);
    // Even if s2 was same as s1, this correctly inserts the saved1 range
    // after the saved2 range, when done after:
    doInsert(saved1, s2);

    // Now correct the Index in the tracks, and other things
    RecalcPositions(s1);
    PermutationEvent(s1);
}

bool TrackList::MoveUp(Track& t)
{
    Track* p = GetPrev(t, true);
    if (p) {
        SwapNodes(p->GetNode(), t.GetNode());
        return true;
    }
    return false;
}

bool TrackList::MoveDown(Track& t)
{
    Track* n = GetNext(t, true);
    if (n) {
        SwapNodes(t.GetNode(), n->GetNode());
        return true;
    }
    return false;
}

bool TrackList::empty() const
{
    return Begin() == End();
}

namespace {
// Abstract the common pattern of the following two member functions
inline double Accumulate(const TrackList& list,
                         double (Track::*memfn)() const, double ident,
                         const double& (*combine)(const double&, const double&))
{
    // Default the answer to zero for empty list
    if (list.empty()) {
        return 0.0;
    }

    // Otherwise accumulate minimum or maximum of track values
    return list.Any().accumulate(ident, combine, memfn);
}
}

double TrackList::GetStartTime() const
{
    return Accumulate(*this, &Track::GetStartTime,
                      std::numeric_limits<double>::max(), std::min);
}

double TrackList::GetEndTime() const
{
    return Accumulate(*this, &Track::GetEndTime,
                      std::numeric_limits<double>::lowest(), std::max);
}

auto Track::ClassTypeInfo() -> const TypeInfo&
{
    static Track::TypeInfo info{
        { "generic", "generic", XO("Generic Track") }, false };
    return info;
}

bool Track::SupportsBasicEditing() const
{
    return true;
}

// Serialize, not with tags of its own, but as attributes within a tag.
void Track::WriteCommonXMLAttributes(
    XMLWriter& xmlFile, bool includeNameAndSelected) const
{
    if (includeNameAndSelected) {
        // May write name and selectedness redundantly for right channels,
        // but continue doing that in case the file is opened in Audacity 3.1.x
        // which does not have unique ChannelGroupData for the track
        xmlFile.WriteAttr(wxT("name"), GetName());
        xmlFile.WriteAttr(wxT("isSelected"), this->GetSelected());
    }
    AttachedTrackObjects::ForEach([&](auto& attachment){
        attachment.WriteXMLAttributes(xmlFile);
    });
}

// Return true iff the attribute is recognized.
bool Track::HandleCommonXMLAttribute(
    const std::string_view& attr, const XMLAttributeValueView& valueView)
{
    long nValue = -1;

    bool handled = false;
    AttachedTrackObjects::ForEach([&](auto& attachment){
        handled = handled || attachment.HandleXMLAttribute(attr, valueView);
    });
    if (handled) {
    }
    // Note that the per-group properties of name and selectedness may have
    // been written redundantly for each channel, and values for the last
    // channel will be the last ones assigned
    else if (attr == "name") {
        SetName(valueView.ToWString());
        return true;
    } else if (attr == "isSelected" && valueView.TryGet(nValue)) {
        this->SetSelected(nValue != 0);
        return true;
    }
    return false;
}

void Track::AdjustPositions()
{
    auto pList = mList.lock();
    if (pList) {
        pList->RecalcPositions(mNode);
        pList->ResizingEvent(mNode);
    }
}

Track::LinkType Track::GetLinkType() const noexcept
{
    return mLinkType;
}

TrackListHolder TrackList::Temporary(AudacityProject* pProject,
                                     const Track::Holder& pTrack)
{
    assert(pTrack == nullptr || pTrack->GetOwner() == nullptr);
    // Make a well formed channel group from these tracks
    auto tempList = Create(pProject);
    if (pTrack) {
        tempList->Add(pTrack);
    }
    tempList->mAssignsIds = false;
    return tempList;
}

void TrackList::AssignUniqueId(const Track::Holder& track)
{
    track->SetId(TrackId { ++sCounter });
}

void TrackList::Append(TrackList&& list, bool assignIds)
{
    auto iter = list.ListOfTracks::begin(),
         end = list.ListOfTracks::end();
    while (iter != end) {
        auto pTrack = *iter;
        iter = list.erase(iter);
        this->Add(pTrack, assignIds ? DoAssignId::Yes : DoAssignId::No);
    }
}

void TrackList::Append(std::shared_ptr<Track> track, bool assignIds)
{
    this->Add(track, assignIds ? DoAssignId::Yes : DoAssignId::No);
}

void TrackList::AppendOne(TrackList&& list)
{
    const auto iter = list.ListOfTracks::begin(),
               end = list.ListOfTracks::end();
    if (iter != end) {
        auto pTrack = *iter;
        list.erase(iter);
        this->Add(pTrack);
    }
}

Track::Holder TrackList::DetachFirst()
{
    auto iter = ListOfTracks::begin();
    auto result = *iter;
    erase(iter);
    result->SetOwner({}, {});
    return result;
}
