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
#include "UndoManager.h"

#include "InconsistencyException.h"

#ifdef _MSC_VER
//Disable truncation warnings
#pragma warning( disable : 4786 )
#endif

Track::Track()
{
   mIndex = 0;

   mOffset = 0.0;
}

Track::Track(const Track& orig, ProtectedCreationArg&&)
    : mProjectTempo { orig.mProjectTempo }
{
   mIndex = 0;
   mOffset = orig.mOffset;
}

// Copy all the track properties except the actual contents
void Track::Init(const Track &orig)
{
   mId = orig.mId;

   // Deep copy of any group data
   mpGroupData = orig.mpGroupData ?
      std::make_unique<ChannelGroupData>(*orig.mpGroupData) : nullptr;
}

const wxString &Track::GetName() const
{
   return GetGroupData().mName;
}

void Track::SetName( const wxString &n )
{
   auto &name = GetGroupData().mName;
   if (name != n) {
      name = n;
      Notify(true);
   }
}

bool Track::GetSelected() const
{
   return GetGroupData().mSelected;
}

void Track::SetSelected(bool s)
{
   auto &selected = GetGroupData().mSelected;
   if (selected != s) {
      selected = s;
      auto pList = mList.lock();
      if (pList)
         pList->SelectionEvent(*this);
   }
}

void Track::EnsureVisible( bool modifyState )
{
   auto pList = mList.lock();
   if (pList)
      pList->EnsureVisibleEvent(SharedPointer(), modifyState);
}

Track::Holder Track::Duplicate() const
{
   // invoke "virtual constructor" to copy track object proper:
   auto result = Clone();

   AttachedTrackObjects::ForEach([&](auto &attachment){
      // Copy view state that might be important to undo/redo
      attachment.CopyTo( *result );
   });

   return result;
}

Track::~Track()
{
}


TrackNodePointer Track::GetNode() const
{
   wxASSERT(mList.lock() == NULL || this == mNode.first->get());
   return mNode;
}

void Track::SetOwner
(const std::weak_ptr<TrackList> &list, TrackNodePointer node)
{
   // BUG: When using this function to clear an owner, we may need to clear
   // focused track too.  Otherwise focus could remain on an invisible (or deleted) track.
   mList = list;
   mNode = node;
}

int Track::GetIndex() const
{
   return mIndex;
}

void Track::SetIndex(int index)
{
   mIndex = index;
}

void Track::SetLinkType(LinkType linkType, bool completeList)
{
   auto pList = mList.lock();
   if (pList && !pList->mPendingUpdates.empty()) {
      auto orig = pList->FindById( GetId() );
      if (orig && orig != this) {
         orig->SetLinkType(linkType);
         return;
      }
   }

   DoSetLinkType(linkType, completeList);

   if (pList) {
      pList->RecalcPositions(mNode);
      pList->ResizingEvent(mNode);
   }
}

void Track::DestroyGroupData()
{
   mpGroupData.reset();
}

Track::ChannelGroupData &Track::MakeGroupData()
{
   if (!mpGroupData)
      // Make on demand
      mpGroupData = std::make_unique<ChannelGroupData>();
   return *mpGroupData;
}

Track::ChannelGroupData &Track::GetGroupData()
{
   auto pTrack = this;
   if (auto pList = GetOwner())
      if (auto pLeader = *pList->FindLeader(pTrack))
         pTrack = pLeader;
   // May make on demand
   return pTrack->MakeGroupData();
}

const Track::ChannelGroupData &Track::GetGroupData() const
{
   // May make group data on demand, but consider that logically const
   return const_cast<Track *>(this)->GetGroupData();
}

void Track::DoSetLinkType(LinkType linkType, bool completeList)
{
   auto oldType = GetLinkType();
   if (linkType == oldType)
      // No change
      return;

   if (oldType == LinkType::None) {
      // Becoming linked

      // First ensure there is no partner
      if (auto partner = GetLinkedTrack())
         partner->mpGroupData.reset();
      assert(!GetLinkedTrack());

      // Change the link type
      MakeGroupData().mLinkType = linkType;

      // If this acquired a partner, it loses any old group data
      if (auto partner = GetLinkedTrack())
         partner->mpGroupData.reset();
   }
   else if (linkType == LinkType::None) {
      // Becoming unlinked
      assert(mpGroupData);
      if (HasLinkedTrack()) {
         if (auto partner = GetLinkedTrack()) {
            // Make independent copy of group data in the partner, which should
            // have had none
            assert(!partner->mpGroupData);
            partner->mpGroupData =
               std::make_unique<ChannelGroupData>(*mpGroupData);
            partner->mpGroupData->mLinkType = LinkType::None;
         }
      }
      mpGroupData->mLinkType = LinkType::None;
   }
   else {
      // Remaining linked, changing the type
      assert(mpGroupData);
      MakeGroupData().mLinkType = linkType;
   }

   // Assertion checks only in a debug build, does not have side effects!
   assert(LinkConsistencyCheck(completeList));
}

Track *Track::GetLinkedTrack() const
{
   auto pList = mList.lock();
   if (!pList)
      return nullptr;

   if (!pList->isNull(mNode)) {
      if (HasLinkedTrack()) {
         auto next = pList->getNext( mNode );
         if ( !pList->isNull( next ) )
            return next.first->get();
      }

      if (mNode.first != mNode.second->begin()) {
         auto prev = pList->getPrev( mNode );
         if ( !pList->isNull( prev ) ) {
            auto track = prev.first->get();
            if (track && track->HasLinkedTrack())
               return track;
         }
      }
   }

   return nullptr;
}

bool Track::HasLinkedTrack() const noexcept
{
    return mpGroupData && mpGroupData->mLinkType != LinkType::None;
}

std::optional<TranslatableString> Track::GetErrorOpening() const
{
   return {};
}

void Track::Notify(bool allChannels, int code)
{
   auto pList = mList.lock();
   if (pList)
      pList->DataEvent(SharedPointer(), allChannels, code);
}

void Track::SyncLockAdjust(double oldT1, double newT1)
{
   if (newT1 > oldT1) {
      // Insert space within the track

      if (oldT1 > GetEndTime())
         return;

      auto tmp = Cut(oldT1, GetEndTime());

      Paste(newT1, tmp.get());
   }
   else if (newT1 < oldT1) {
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

bool Track::IsSelectedLeader() const
   { return IsSelected() && IsLeader(); }

void Track::FinishCopy
(const Track *n, Track *dest)
{
   if (dest) {
      dest->mpGroupData = n->mpGroupData ?
         std::make_unique<ChannelGroupData>(*n->mpGroupData) : nullptr;
   }
}

bool Track::LinkConsistencyFix(bool doFix, bool completeList)
{
   // Sanity checks for linked tracks; unsetting the linked property
   // doesn't fix the problem, but it likely leaves us with orphaned
   // sample blocks instead of much worse problems.
   bool err = false;
   if (completeList && HasLinkedTrack()) {
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
      }
      else {
         err = true;
         if (doFix) {
            wxLogWarning(
               L"Track %s had link to NULL track. Setting it to not be linked.",
               GetName());
            SetLinkType(LinkType::None);
         }
      }
   }
   return ! err;
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
   [](AudacityProject &project) { return TrackList::Create( &project ); }
};

TrackList &TrackList::Get( AudacityProject &project )
{
   return project.AttachedObjects::Get< TrackList >( key );
}

const TrackList &TrackList::Get( const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

TrackList::TrackList( AudacityProject *pOwner )
   : mOwner{ pOwner }
{
}

// Factory function
std::shared_ptr<TrackList> TrackList::Create( AudacityProject *pOwner )
{
   return std::make_shared<TrackList>( pOwner );
}

#if 0
TrackList &TrackList::operator= (TrackList &&that)
{
   if (this != &that) {
      this->Clear();
      Swap(that);
   }
   return *this;
}
#endif

void TrackList::Swap(TrackList &that)
{
   auto SwapLOTs = [](
      ListOfTracks &a, const std::weak_ptr< TrackList > &aSelf,
      ListOfTracks &b, const std::weak_ptr< TrackList > &bSelf )
   {
      a.swap(b);
      for (auto it = a.begin(), last = a.end(); it != last; ++it)
         (*it)->SetOwner(aSelf, {it, &a});
      for (auto it = b.begin(), last = b.end(); it != last; ++it)
         (*it)->SetOwner(bSelf, {it, &b});
   };

   const auto self = shared_from_this();
   const auto otherSelf = that.shared_from_this();
   SwapLOTs( *this, self, that, otherSelf );
   SwapLOTs( this->mPendingUpdates, self, that.mPendingUpdates, otherSelf );
   mUpdaters.swap(that.mUpdaters);
}

TrackList::~TrackList()
{
   Clear(false);
}

wxString TrackList::MakeUniqueTrackName(const wxString& baseTrackName) const
{
   int n = 1;
   while(true)
   {
      auto name = wxString::Format("%s %d", baseTrackName, n++);

      bool found {false};
      for(const auto track : Any())
      {
         if(track->GetName() == name)
         {
            found = true;
            break;
         }
      }
      if(!found)
         return name;
   }
}

void TrackList::RecalcPositions(TrackNodePointer node)
{
   if ( isNull( node ) )
      return;

   Track *t;
   int i = 0;

   auto prev = getPrev( node );
   if ( !isNull( prev ) ) {
      t = prev.first->get();
      i = t->GetIndex() + 1;
   }

   const auto theEnd = end();
   for (auto n = Find( node.first->get() ); n != theEnd; ++n) {
      t = *n;
      t->SetIndex(i++);
   }

   UpdatePendingTracks();
}

void TrackList::QueueEvent(TrackListEvent event)
{
   BasicUI::CallAfter( [wThis = weak_from_this(), event = std::move(event)]{
      if (auto pThis = wThis.lock())
         pThis->Publish(event);
   } );
}

void TrackList::SelectionEvent(Track &track)
{
   for (auto channel : Channels(&track))
      QueueEvent({
         TrackListEvent::SELECTION_CHANGE, channel->shared_from_this() });
}

void TrackList::DataEvent(
   const std::shared_ptr<Track> &pTrack, bool allChannels, int code)
{
   auto doQueueEvent = [this, code](const std::shared_ptr<Track> &theTrack){
      QueueEvent({ TrackListEvent::TRACK_DATA_CHANGE, theTrack, code });
   };
   if (allChannels)
      for (auto channel : Channels(pTrack.get()))
         doQueueEvent(channel->shared_from_this());
   else
      doQueueEvent(pTrack);
}

void TrackList::EnsureVisibleEvent(
   const std::shared_ptr<Track> &pTrack, bool modifyState )
{
   // Substitute leader track
   const auto pLeader = *FindLeader(pTrack.get());
   QueueEvent({ TrackListEvent::TRACK_REQUEST_VISIBLE,
      pLeader ? pLeader->SharedPointer() : nullptr,
      static_cast<int>(modifyState) });
}

void TrackList::PermutationEvent(TrackNodePointer node)
{
   QueueEvent({ TrackListEvent::PERMUTED, *node.first });
}

void TrackList::DeletionEvent(std::weak_ptr<Track> node, bool duringReplace)
{
   QueueEvent(
      { TrackListEvent::DELETION, std::move(node), duringReplace ? 1 : 0 });
}

void TrackList::AdditionEvent(TrackNodePointer node)
{
   QueueEvent({ TrackListEvent::ADDITION, *node.first });
}

void TrackList::ResizingEvent(TrackNodePointer node)
{
   QueueEvent({ TrackListEvent::RESIZING, *node.first });
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

auto TrackList::FindLeader( Track *pTrack )
   -> TrackIter< Track >
{
   auto iter = Find(pTrack);
   while( *iter && ! ( *iter )->IsLeader() )
      --iter;
   return iter.Filter( &Track::IsLeader );
}

bool TrackList::SwapChannels(Track &track)
{
   if (!track.HasLinkedTrack())
      return false;
   auto pOwner = track.GetOwner();
   if (!pOwner)
      return false;
   auto pPartner = pOwner->GetNext(&track, false);
   if (!pPartner)
      return false;

   // Swap channels, avoiding copying of GroupData
   auto pData = move(track.mpGroupData);
   assert(pData);
   pOwner->MoveUp(pPartner);
   pPartner->mpGroupData = move(pData);
   return true;
}

void TrackList::Permute(const std::vector<Track *> &tracks)
{
   std::vector<TrackNodePointer> permutation;
   for (const auto pTrack : tracks)
      for (const auto pChannel : Channels(pTrack))
         permutation.push_back(pChannel->GetNode());
   for (const auto iter : permutation) {
      ListOfTracks::value_type track = *iter.first;
      erase(iter.first);
      Track *pTrack = track.get();
      pTrack->SetOwner(shared_from_this(),
         { insert(ListOfTracks::end(), track), this });
   }
   auto n = getBegin();
   RecalcPositions(n);
   PermutationEvent(n);
}

Track *TrackList::FindById( TrackId id )
{
   // Linear search.  Tracks in a project are usually very few.
   // Search only the non-pending tracks.
   auto it = std::find_if( ListOfTracks::begin(), ListOfTracks::end(),
      [=](const ListOfTracks::value_type &ptr){ return ptr->GetId() == id; } );
   if (it == ListOfTracks::end())
      return {};
   return it->get();
}

Track *TrackList::DoAddToHead(const std::shared_ptr<Track> &t)
{
   Track *pTrack = t.get();
   push_front(ListOfTracks::value_type(t));
   auto n = getBegin();
   pTrack->SetOwner(shared_from_this(), n);
   pTrack->SetId( TrackId{ ++sCounter } );
   RecalcPositions(n);
   AdditionEvent(n);
   return front().get();
}

Track *TrackList::DoAdd(const std::shared_ptr<Track> &t)
{
   if (!ListOfTracks::empty()) {
      auto &pLast = *ListOfTracks::rbegin();
      if (auto &pGroupData = pLast->mpGroupData
         ; pGroupData && pGroupData->mLinkType != Track::LinkType::None
      ) {
         // Assume the newly added track is intended to pair with the last
         // Avoid upsetting assumptions in case this track had its own group
         // data initialized during Duplicate()
         t->mpGroupData.reset();
      }
   }

   push_back(t);

   auto n = getPrev( getEnd() );

   t->SetOwner(shared_from_this(), n);
   t->SetId( TrackId{ ++sCounter } );
   RecalcPositions(n);
   AdditionEvent(n);
   return back().get();
}

auto TrackList::Replace(Track * t, const ListOfTracks::value_type &with) ->
   ListOfTracks::value_type
{
   ListOfTracks::value_type holder;
   if (t && with) {
      auto node = t->GetNode();
      t->SetOwner({}, {});

      holder = *node.first;

      Track *pTrack = with.get();
      *node.first = with;
      pTrack->SetOwner(shared_from_this(), node);
      pTrack->SetId( t->GetId() );
      RecalcPositions(node);

      DeletionEvent(t->shared_from_this(), true);
      AdditionEvent(node);
   }
   return holder;
}

void TrackList::UnlinkChannels(Track& track)
{
   auto list = track.mList.lock();
   if (list.get() == this)
   {
      auto channels = TrackList::Channels(&track);
      for (auto c : channels)
          c->SetLinkType(Track::LinkType::None);
   }
   else
      THROW_INCONSISTENCY_EXCEPTION;
}

bool TrackList::MakeMultiChannelTrack(Track& track, int nChannels, bool aligned)
{
   if (nChannels != 2)
      return false;

   auto list = track.mList.lock();
   if (list.get() == this)
   {
      if (*list->FindLeader(&track) != &track)
         return false;

      auto first = list->Find(&track);
      auto canLink = [&]() -> bool {
         int count = nChannels;
         for (auto it = first, end = TrackList::end(); it != end && count; ++it)
         {
            if ((*it)->HasLinkedTrack())
               return false;
            --count;
         }
         return count == 0;
      }();

      if (!canLink)
         return false;

      (*first)->SetLinkType(aligned ? Track::LinkType::Aligned : Track::LinkType::Group);
   }
   else
      THROW_INCONSISTENCY_EXCEPTION;
   return true;
}

void TrackList::Remove(Track &track)
{
   assert(track.IsLeader());
   auto *t = &track;
   const size_t nChannels = NChannels(*t);
   Track *nextT{};
   for (size_t ii = 0; t != nullptr && ii < nChannels; ++ii, t = nextT) {
      nextT = nullptr;
      auto iter = getEnd();
      auto node = t->GetNode();
      t->SetOwner({}, {});

      if (!isNull(node)) {
         ListOfTracks::value_type holder = *node.first;

         iter = getNext(node);
         erase(node.first);
         if (!isNull(iter)) {
            RecalcPositions(iter);
            nextT = iter.first->get();
         }

         DeletionEvent(t->shared_from_this(), false);
      }
   }
}

void TrackList::Clear(bool sendEvent)
{
   // Null out the back-pointers to this in tracks, in case there
   // are outstanding shared_ptrs to those tracks, making them outlive
   // the temporary ListOfTracks below.
   for ( auto pTrack: *this )
   {
      pTrack->SetOwner({}, {});

      if (sendEvent)
         DeletionEvent(pTrack->shared_from_this(), false);
   }

   for ( auto pTrack: mPendingUpdates )
   {
      pTrack->SetOwner({}, {});

      if (sendEvent)
         DeletionEvent(pTrack, false);
   }

   ListOfTracks tempList;
   tempList.swap( *this );

   ListOfTracks updating;
   updating.swap( mPendingUpdates );

   mUpdaters.clear();
}

/// Return a track in the list that comes after Track t
Track *TrackList::GetNext(Track * t, bool linked) const
{
   if (t) {
      auto node = t->GetNode();
      if ( !isNull( node ) ) {
         if ( linked && t->HasLinkedTrack() )
            node = getNext( node );

         if ( !isNull( node ) )
            node = getNext( node );

         if ( !isNull( node ) )
            return node.first->get();
      }
   }

   return nullptr;
}

Track *TrackList::GetPrev(Track * t, bool linked) const
{
   if (t) {
      TrackNodePointer prev;
      auto node = t->GetNode();
      if ( !isNull( node ) ) {
         // linked is true and input track second in team?
         if (linked) {
            prev = getPrev( node );
            if( !isNull( prev ) &&
                !t->HasLinkedTrack() && t->GetLinkedTrack() )
               // Make it the first
               node = prev;
         }

         prev = getPrev( node );
         if ( !isNull( prev ) ) {
            // Back up once
            node = prev;

            // Back up twice sometimes when linked is true
            if (linked) {
               prev = getPrev( node );
               if( !isNull( prev ) &&
                   !(*node.first)->HasLinkedTrack() && (*node.first)->GetLinkedTrack() )
                  node = prev;
            }

            return node.first->get();
         }
      }
   }

   return nullptr;
}

bool TrackList::CanMoveUp(Track * t) const
{
   return GetPrev(t, true) != NULL;
}

bool TrackList::CanMoveDown(Track * t) const
{
   return GetNext(t, true) != NULL;
}

// This is used when you want to swap the channel group starting
// at s1 with that starting at s2.
// The complication is that the tracks are stored in a single
// linked list.
void TrackList::SwapNodes(TrackNodePointer s1, TrackNodePointer s2)
{
   // if a null pointer is passed in, we want to know about it
   wxASSERT(!isNull(s1));
   wxASSERT(!isNull(s2));

   // Deal with first track in each team
   s1 = ( * FindLeader( s1.first->get() ) )->GetNode();
   s2 = ( * FindLeader( s2.first->get() ) )->GetNode();

   // Safety check...
   if (s1 == s2)
      return;

   // Be sure s1 is the earlier iterator
   if ((*s1.first)->GetIndex() >= (*s2.first)->GetIndex())
      std::swap(s1, s2);

   // For saving the removed tracks
   using Saved = std::vector< ListOfTracks::value_type >;
   Saved saved1, saved2;

   auto doSave = [&] ( Saved &saved, TrackNodePointer &s ) {
      size_t nn = NChannels(**s.first);
      saved.resize( nn );
      // Save them in backwards order
      while( nn-- )
         saved[nn] = *s.first, s.first = erase(s.first);
   };

   doSave( saved1, s1 );
   // The two ranges are assumed to be disjoint but might abut
   const bool same = (s1 == s2);
   doSave( saved2, s2 );
   if (same)
      // Careful, we invalidated s1 in the second doSave!
      s1 = s2;

   // Reinsert them
   auto doInsert = [&] ( Saved &saved, TrackNodePointer &s ) {
      Track *pTrack;
      for (auto & pointer : saved)
         pTrack = pointer.get(),
         // Insert before s, and reassign s to point at the new node before
         // old s; which is why we saved pointers in backwards order
         pTrack->SetOwner(shared_from_this(),
            s = { insert(s.first, pointer), this } );
   };
   // This does not invalidate s2 even when it equals s1:
   doInsert( saved2, s1 );
   // Even if s2 was same as s1, this correctly inserts the saved1 range
   // after the saved2 range, when done after:
   doInsert( saved1, s2 );

   // Now correct the Index in the tracks, and other things
   RecalcPositions(s1);
   PermutationEvent(s1);
}

bool TrackList::MoveUp(Track * t)
{
   if (t) {
      Track *p = GetPrev(t, true);
      if (p) {
         SwapNodes(p->GetNode(), t->GetNode());
         return true;
      }
   }

   return false;
}

bool TrackList::MoveDown(Track * t)
{
   if (t) {
      Track *n = GetNext(t, true);
      if (n) {
         SwapNodes(t->GetNode(), n->GetNode());
         return true;
      }
   }

   return false;
}

bool TrackList::Contains(const Track * t) const
{
   return make_iterator_range( *this ).contains( t );
}

bool TrackList::empty() const
{
   return begin() == end();
}

size_t TrackList::NChannels() const
{
   int cnt = 0;

   if (!empty())
      cnt = getPrev( getEnd() ).first->get()->GetIndex() + 1;

   return cnt;
}

namespace {
   // Abstract the common pattern of the following three member functions
   inline double Accumulate
      (const TrackList &list,
       double (Track::*memfn)() const,
       double ident,
       const double &(*combine)(const double&, const double&))
   {
      // Default the answer to zero for empty list
      if (list.empty()) {
         return 0.0;
      }

      // Otherwise accumulate minimum or maximum of track values
      return list.Any().accumulate(ident, combine, memfn);
   }
}

double TrackList::GetMinOffset() const
{
   return Accumulate(*this, &Track::GetOffset, DBL_MAX, std::min);
}

double TrackList::GetStartTime() const
{
   return Accumulate(*this, &Track::GetStartTime, DBL_MAX, std::min);
}

double TrackList::GetEndTime() const
{
   return Accumulate(*this, &Track::GetEndTime, -DBL_MAX, std::max);
}

std::shared_ptr<Track>
TrackList::RegisterPendingChangedTrack( Updater updater, Track *src )
{
   std::shared_ptr<Track> pTrack;
   if (src) {
      pTrack = src->Clone(); // not duplicate
      // Share the satellites with the original, though they do not point back
      // to the pending track
      ((AttachedTrackObjects&)*pTrack) = *src; // shallow copy
   }

   if (pTrack) {
      mUpdaters.push_back( updater );
      mPendingUpdates.push_back( pTrack );
      auto n = mPendingUpdates.end();
      --n;
      pTrack->SetOwner(shared_from_this(), {n, &mPendingUpdates});
   }

   return pTrack;
}

void TrackList::RegisterPendingNewTrack( const std::shared_ptr<Track> &pTrack )
{
   Add<Track>( pTrack );
   pTrack->SetId( TrackId{} );
}

void TrackList::UpdatePendingTracks()
{
   auto pUpdater = mUpdaters.begin();
   for (const auto &pendingTrack : mPendingUpdates) {
      // Copy just a part of the track state, according to the update
      // function
      const auto &updater = *pUpdater;
      auto src = FindById( pendingTrack->GetId() );
      if (pendingTrack && src) {
         if (updater)
            updater( *pendingTrack, *src );
         pendingTrack->DoSetLinkType(src->GetLinkType());
      }
      ++pUpdater;
   }
}

/*! @excsafety{No-fail} */
void TrackList::ClearPendingTracks( ListOfTracks *pAdded )
{
   for (const auto &pTrack: mPendingUpdates)
      pTrack->SetOwner( {}, {} );
   mPendingUpdates.clear();
   mUpdaters.clear();

   if (pAdded)
      pAdded->clear();

   // To find the first node that remains after the first deleted one
   TrackNodePointer node;
   bool foundNode = false;

   for (auto it = ListOfTracks::begin(), stop = ListOfTracks::end();
        it != stop;) {
      if (it->get()->GetId() == TrackId{}) {
         do {
            if (pAdded)
               pAdded->push_back( *it );
            (*it)->SetOwner( {}, {} );
            DeletionEvent(*it, false);
            it = erase( it );
         }
         while (it != stop && it->get()->GetId() == TrackId{});

         if (!foundNode && it != stop) {
            node = (*it)->GetNode();
            foundNode = true;
         }
      }
      else
         ++it;
   }

   if (!empty()) {
      RecalcPositions(getBegin());
   }
}

/*! @excsafety{Strong} */
bool TrackList::ApplyPendingTracks()
{
   bool result = false;

   ListOfTracks additions;
   ListOfTracks updates;
   {
      // Always clear, even if one of the update functions throws
      auto cleanup = finally( [&] { ClearPendingTracks( &additions ); } );
      UpdatePendingTracks();
      updates.swap( mPendingUpdates );
   }

   // Remaining steps must be No-fail-guarantee so that this function
   // gives Strong-guarantee

   std::vector< std::shared_ptr<Track> > reinstated;

   for (auto &pendingTrack : updates) {
      if (pendingTrack) {
         pendingTrack->AttachedTrackObjects::ForEach([&](auto &attachment){
            attachment.Reparent( pendingTrack );
         });
         auto src = FindById( pendingTrack->GetId() );
         if (src)
            this->Replace(src, pendingTrack), result = true;
         else
            // Perhaps a track marked for pending changes got deleted by
            // some other action.  Recreate it so we don't lose the
            // accumulated changes.
            reinstated.push_back(pendingTrack);
      }
   }

   // If there are tracks to reinstate, append them to the list.
   for (auto &pendingTrack : reinstated)
      if (pendingTrack)
         this->Add( pendingTrack ), result = true;

   // Put the pending added tracks back into the list, preserving their
   // positions.
   bool inserted = false;
   ListOfTracks::iterator first;
   for (auto &pendingTrack : additions) {
      if (pendingTrack) {
         auto iter = ListOfTracks::begin();
         std::advance( iter, pendingTrack->GetIndex() );
         iter = ListOfTracks::insert( iter, pendingTrack );
         pendingTrack->SetOwner( shared_from_this(), {iter, this} );
         pendingTrack->SetId( TrackId{ ++sCounter } );
         if (!inserted) {
            first = iter;
            inserted = true;
         }
      }
   }
   if (inserted) {
      TrackNodePointer node{first, this};
      RecalcPositions(node);
      AdditionEvent(node);
      result = true;
   }

   return result;
}

std::shared_ptr<Track> Track::SubstitutePendingChangedTrack()
{
   // Linear search.  Tracks in a project are usually very few.
   auto pList = mList.lock();
   if (pList) {
      const auto id = GetId();
      const auto end = pList->mPendingUpdates.end();
      auto it = std::find_if(
         pList->mPendingUpdates.begin(), end,
         [=](const ListOfTracks::value_type &ptr){ return ptr->GetId() == id; } );
      if (it != end)
         return *it;
   }
   return SharedPointer();
}

std::shared_ptr<const Track> Track::SubstitutePendingChangedTrack() const
{
   return const_cast<Track*>(this)->SubstitutePendingChangedTrack();
}

std::shared_ptr<const Track> Track::SubstituteOriginalTrack() const
{
   auto pList = mList.lock();
   if (pList) {
      const auto id = GetId();
      const auto pred = [=]( const ListOfTracks::value_type &ptr ) {
         return ptr->GetId() == id; };
      const auto end = pList->mPendingUpdates.end();
      const auto it = std::find_if( pList->mPendingUpdates.begin(), end, pred );
      if (it != end) {
         const auto &list2 = (const ListOfTracks &) *pList;
         const auto end2 = list2.end();
         const auto it2 = std::find_if( list2.begin(), end2, pred );
         if ( it2 != end2 )
            return *it2;
      }
   }
   return SharedPointer();
}

auto Track::ClassTypeInfo() -> const TypeInfo &
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
   XMLWriter &xmlFile, bool includeNameAndSelected) const
{
   if (includeNameAndSelected) {
      // May write name and selectedness redundantly for right channels,
      // but continue doing that in case the file is opened in Audacity 3.1.x
      // which does not have unique ChannelGroupData for the track
      xmlFile.WriteAttr(wxT("name"), GetName());
      xmlFile.WriteAttr(wxT("isSelected"), this->GetSelected());
   }
   AttachedTrackObjects::ForEach([&](auto &attachment){
      attachment.WriteXMLAttributes( xmlFile );
   });
}

// Return true iff the attribute is recognized.
bool Track::HandleCommonXMLAttribute(
   const std::string_view& attr, const XMLAttributeValueView& valueView)
{
   long nValue = -1;

   bool handled = false;
   AttachedTrackObjects::ForEach([&](auto &attachment){
      handled = handled || attachment.HandleXMLAttribute( attr, valueView );
   });
   if (handled)
      ;
   // Note that the per-group properties of name and selectedness may have
   // been written redundantly for each channel, and values for the last
   // channel will be the last ones assigned
   else if (attr == "name") {
      SetName(valueView.ToWString());
      return true;
   }
   else if (attr == "isSelected" && valueView.TryGet(nValue)) {
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

bool TrackList::HasPendingTracks() const
{
   if ( !mPendingUpdates.empty() )
      return true;
   if (end() != std::find_if(begin(), end(), [](const Track *t){
      return t->GetId() == TrackId{};
   }))
      return true;
   return false;
}

Track::LinkType Track::GetLinkType() const noexcept
{
    return mpGroupData ? mpGroupData->mLinkType : LinkType::None;
}

bool Track::IsAlignedWithLeader() const
{
   if (auto owner = GetOwner())
   {
      auto leader = *owner->FindLeader(this);
      return leader != this && leader->GetLinkType() == Track::LinkType::Aligned;
   }
   return false;
}

TrackAttachment &ChannelAttachmentsBase::Get(
   const AttachedTrackObjects::RegisteredFactory &key,
   Track &track, size_t iChannel)
{
   // Precondition of this function; satisfies precondition of factory below
   assert(iChannel < track.NChannels());
   auto &attachments = track.AttachedObjects::Get<ChannelAttachmentsBase>(key);
   auto &objects = attachments.mAttachments;
   if (iChannel >= objects.size())
      objects.resize(iChannel + 1);
   auto &pObject = objects[iChannel];
   if (!pObject) {
      // Create on demand
      pObject = attachments.mFactory(track, iChannel);
      assert(pObject); // Precondition of constructor
   }
   return *pObject;
}

TrackAttachment *ChannelAttachmentsBase::Find(
   const AttachedTrackObjects::RegisteredFactory &key,
   Track *pTrack, size_t iChannel)
{
   assert(!pTrack || iChannel < pTrack->NChannels());
   if (!pTrack)
      return nullptr;
   const auto pAttachments =
      pTrack->AttachedObjects::Find<ChannelAttachmentsBase>(key);
   // do not create on demand
   if (!pAttachments || iChannel >= pAttachments->mAttachments.size())
      return nullptr;
   return pAttachments->mAttachments[iChannel].get();
}

ChannelAttachmentsBase::~ChannelAttachmentsBase() = default;

void ChannelAttachmentsBase::CopyTo(Track &track) const
{
   for (auto &pAttachment : mAttachments)
      if (pAttachment)
         pAttachment->CopyTo(track);
}

void ChannelAttachmentsBase::Reparent(const std::shared_ptr<Track> &parent)
{
   for (auto &pAttachment : mAttachments)
      if (pAttachment)
         pAttachment->Reparent(parent);
}

void ChannelAttachmentsBase::WriteXMLAttributes(XMLWriter &writer) const
{
   for (auto &pAttachment : mAttachments)
      if (pAttachment)
         pAttachment->WriteXMLAttributes(writer);
}

bool ChannelAttachmentsBase::HandleXMLAttribute(
   const std::string_view& attr, const XMLAttributeValueView& valueView)
{
   return std::any_of(mAttachments.begin(), mAttachments.end(),
   [&](auto &pAttachment) {
      return pAttachment && pAttachment->HandleXMLAttribute(attr, valueView);
   });
}

void Track::OnProjectTempoChange(double newTempo)
{
   DoOnProjectTempoChange(mProjectTempo, newTempo);
   mProjectTempo = newTempo;
}

const std::optional<double>& Track::GetProjectTempo() const
{
   return mProjectTempo;
}

// Undo/redo handling of selection changes
namespace {
struct TrackListRestorer final : UndoStateExtension {
   TrackListRestorer(AudacityProject &project)
      : mpTracks{ TrackList::Create(nullptr) }
   {
      for (auto pTrack : TrackList::Get(project)) {
         if ( pTrack->GetId() == TrackId{} )
            // Don't copy a pending added track
            continue;
         mpTracks->Add(pTrack->Duplicate());
      }
   }
   void RestoreUndoRedoState(AudacityProject &project) override {
      auto &dstTracks = TrackList::Get(project);
      dstTracks.Clear();
      for (auto pTrack : mpTracks->Any())
         dstTracks.Add(pTrack->Duplicate());
   }
   bool CanUndoOrRedo(const AudacityProject &project) override {
      return !TrackList::Get(project).HasPendingTracks();
   }
   const std::shared_ptr<TrackList> mpTracks;
};

UndoRedoExtensionRegistry::Entry sEntry {
   [](AudacityProject &project) -> std::shared_ptr<UndoStateExtension> {
      return std::make_shared<TrackListRestorer>(project);
   }
};
}

TrackList *TrackList::FindUndoTracks(const UndoStackElem &state)
{
   auto &exts = state.state.extensions;
   auto end = exts.end(),
      iter = std::find_if(exts.begin(), end, [](auto &pExt){
         return dynamic_cast<TrackListRestorer*>(pExt.get());
      });
   if (iter != end)
      return static_cast<TrackListRestorer*>(iter->get())->mpTracks.get();
   return nullptr;
}
