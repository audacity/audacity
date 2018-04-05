/**********************************************************************

  Audacity: A Digital Audio Editor

  Track.cpp

  Dominic Mazzoni

*******************************************************************//**

\class Track
\brief Fundamental data object of Audacity, placed in the TrackPanel.
Classes derived form it include the WaveTrack, NoteTrack, LabelTrack
and TimeTrack.

\class AudioTrack
\brief A Track that can load/save audio data to/from XML.

\class PlayableTrack
\brief An AudioTrack that can be played and stopped.

*//*******************************************************************/

#include <algorithm>
#include <numeric>
#include "Track.h"

#include <float.h>
#include <wx/file.h>
#include <wx/textfile.h>
#include <wx/log.h>

#include "TimeTrack.h"
#include "WaveTrack.h"
#include "NoteTrack.h"
#include "LabelTrack.h"
#include "Project.h"
#include "DirManager.h"

#include "Experimental.h"

#include "TrackPanel.h" // for TrackInfo

#ifdef _MSC_VER
//Disable truncation warnings
#pragma warning( disable : 4786 )
#endif

#ifdef __WXDEBUG__
   // if we are in a debug build of audacity
   /// Define this to do extended (slow) debuging of TrackListIterator
//   #define DEBUG_TLI
#endif

Track::Track(const std::shared_ptr<DirManager> &projDirManager)
:  vrulerSize(36,0),
   mDirManager(projDirManager)
{
   mSelected  = false;
   mLinked    = false;

   mY = 0;
   mHeight = DefaultHeight;
   mIndex = 0;

   mMinimized = false;

   mOffset = 0.0;

   mChannel = MonoChannel;
}

Track::Track(const Track &orig)
: vrulerSize( orig.vrulerSize )
{
   mY = 0;
   mIndex = 0;
   Init(orig);
   mOffset = orig.mOffset;
}

// Copy all the track properties except the actual contents
void Track::Init(const Track &orig)
{
   mId = orig.mId;

   mDefaultName = orig.mDefaultName;
   mName = orig.mName;

   mDirManager = orig.mDirManager;

   mSelected = orig.mSelected;
   mLinked = orig.mLinked;
   mHeight = orig.mHeight;
   mMinimized = orig.mMinimized;
   mChannel = orig.mChannel;
}

void Track::SetSelected(bool s)
{
   mSelected = s;
}

void Track::Merge(const Track &orig)
{
   mSelected = orig.mSelected;
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
   // focussed track too.  Otherwise focus could remain on an invisible (or deleted) track.
   mList = list;
   mNode = node;
}

int Track::GetMinimizedHeight() const
{
   auto height = TrackInfo::MinimumTrackHeight();

   if (GetLink()) {
      auto halfHeight = height / 2;
      if (GetLinked())
         return halfHeight;
      else
         return height - halfHeight;
   }

   return height;
}

int Track::GetIndex() const
{
   return mIndex;
}

void Track::SetIndex(int index)
{
   mIndex = index;
}

int Track::GetY() const
{
   return mY;
}

void Track::SetY(int y)
{
   auto pList = mList.lock();
   if (pList && !pList->mPendingUpdates.empty()) {
      auto orig = pList->FindById( GetId() );
      if (orig && orig != this) {
         // delegate, and rely on the update to copy back
         orig->SetY(y);
         pList->UpdatePendingTracks();
         return;
      }
   }

   DoSetY(y);
}

void Track::DoSetY(int y)
{
   mY = y;
}

int Track::GetHeight() const
{
   if (mMinimized) {
      return GetMinimizedHeight();
   }

   return mHeight;
}

void Track::SetHeight(int h)
{
   auto pList = mList.lock();
   if (pList && !pList->mPendingUpdates.empty()) {
      auto orig = pList->FindById( GetId() );
      if (orig && orig != this) {
         // delegate, and rely on RecalcPositions to copy back
         orig->SetHeight(h);
         return;
      }
   }

   DoSetHeight(h);

   if (pList) {
      pList->RecalcPositions(mNode);
      pList->ResizingEvent(mNode);
   }
}

void Track::DoSetHeight(int h)
{
   mHeight = h;
}

bool Track::GetMinimized() const
{
   return mMinimized;
}

void Track::SetMinimized(bool isMinimized)
{
   auto pList = mList.lock();
   if (pList && !pList->mPendingUpdates.empty()) {
      auto orig = pList->FindById( GetId() );
      if (orig && orig != this) {
         // delegate, and rely on RecalcPositions to copy back
         orig->SetMinimized(isMinimized);
         return;
      }
   }

   DoSetMinimized(isMinimized);

   if (pList) {
      pList->RecalcPositions(mNode);
      pList->ResizingEvent(mNode);
   }
}

void Track::DoSetMinimized(bool isMinimized)
{
   mMinimized = isMinimized;
}

void Track::SetLinked(bool l)
{
   auto pList = mList.lock();
   if (pList && !pList->mPendingUpdates.empty()) {
      auto orig = pList->FindById( GetId() );
      if (orig && orig != this) {
         // delegate, and rely on RecalcPositions to copy back
         orig->SetLinked(l);
         return;
      }
   }

   DoSetLinked(l);

   if (pList) {
      pList->RecalcPositions(mNode);
      pList->ResizingEvent(mNode);
   }
}

void Track::DoSetLinked(bool l)
{
   mLinked = l;
}

Track *Track::GetLink() const
{
   auto pList = mList.lock();
   if (!pList)
      return nullptr;

   if (!pList->isNull(mNode)) {
      if (mLinked) {
         auto next = pList->getNext( mNode );
         if ( !pList->isNull( next ) )
            return next.first->get();
      }

      if (mNode.first != mNode.second->begin()) {
         auto prev = pList->getPrev( mNode );
         if ( !pList->isNull( prev ) ) {
            auto track = prev.first->get();
            if (track && track->GetLinked())
               return track;
         }
      }
   }

   return nullptr;
}

bool Track::IsSyncLockSelected() const
{
#ifdef EXPERIMENTAL_SYNC_LOCK
   AudacityProject *p = GetActiveProject();
   if (!p || !p->IsSyncLocked())
      return false;

   auto pList = mList.lock();
   SyncLockedTracksIterator git(pList.get());
   Track *t = git.StartWith(const_cast<Track*>(this));

   if (!t) {
      // Not in a sync-locked group.
      return ((this->GetKind() == Track::Wave) || (this->GetKind() == Track::Label)) && GetSelected();
   }

   for (; t; t = git.Next()) {
      if (t->GetSelected())
         return true;
   }
#endif

   return false;
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

std::shared_ptr<Track> Track::FindTrack()
{
   return Pointer( this );
}

void PlayableTrack::Init( const PlayableTrack &orig )
{
   mMute = orig.mMute;
   mSolo = orig.mSolo;
   AudioTrack::Init( orig );
}

void PlayableTrack::Merge( const Track &orig )
{
   auto pOrig = dynamic_cast<const PlayableTrack *>(&orig);
   wxASSERT( pOrig );
   mMute = pOrig->mMute;
   mSolo = pOrig->mSolo;
   AudioTrack::Merge( *pOrig );
}

// Serialize, not with tags of its own, but as attributes within a tag.
void PlayableTrack::WriteXMLAttributes(XMLWriter &xmlFile) const
{
   xmlFile.WriteAttr(wxT("mute"), mMute);
   xmlFile.WriteAttr(wxT("solo"), mSolo);
   AudioTrack::WriteXMLAttributes(xmlFile);
}

// Return true iff the attribute is recognized.
bool PlayableTrack::HandleXMLAttribute(const wxChar *attr, const wxChar *value)
{
   const wxString strValue{ value };
   long nValue;
   if (!wxStrcmp(attr, wxT("mute")) &&
            XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue)) {
      mMute = (nValue != 0);
      return true;
   }
   else if (!wxStrcmp(attr, wxT("solo")) &&
            XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue)) {
      mSolo = (nValue != 0);
      return true;
   }

   return AudioTrack::HandleXMLAttribute(attr, value);
}

// TrackListIterator
TrackListIterator::TrackListIterator(TrackList * val, TrackNodePointer p)
   : l{ val }
   , cur{ p }
{
}

TrackListIterator::TrackListIterator(TrackList * val)
   : l{ val }
   , cur{}
{
   if (l)
      cur = l->getBegin();
}

Track *TrackListIterator::StartWith(Track * val)
{
   if (val == NULL) {
      return First();
   }

   if (l == NULL) {
      return NULL;
   }

   if (val->mList.lock() == NULL)
      return nullptr;

   cur = val->GetNode();
   return cur.first->get();
}

Track *TrackListIterator::First(TrackList * val)
{
   if (val != NULL) {
      l = val;
   }

   if (l == NULL) {
      return NULL;
   }

   cur = l->getBegin();

   if (!l->isNull(cur)) {
      return cur.first->get();
   }

   return nullptr;
}

Track *TrackListIterator::Last(bool skiplinked)
{
   if (l == NULL) {
      return NULL;
   }

   cur = l->getPrev( l->getEnd() );
   if ( l->isNull( cur ) )
      return nullptr;

   // With skiplinked set, we won't return the second channel of a linked pair
   if (skiplinked) {
      auto prev = l->getPrev( cur );
      if ( !l->isNull( prev ) &&
           !(*cur.first)->GetLinked() &&
           (*cur.first)->GetLink()
      )
         cur = prev;
   }

   return cur.first->get();
}

Track *TrackListIterator::Next(bool skipLinked)
{
#ifdef DEBUG_TLI // if we are debugging this bit
   wxASSERT_MSG((!cur || (*l).Contains((*cur).t)), wxT("cur invalid at start of Next(). List changed since iterator created?"));   // check that cur is in the list
#endif

   if (!l || l->isNull(cur))
      return nullptr;

   if (skipLinked &&
       (*cur.first)->GetLinked())
      cur = l->getNext( cur );

   #ifdef DEBUG_TLI // if we are debugging this bit
   wxASSERT_MSG((!cur || (*l).Contains((*cur).t)), wxT("cur invalid after skipping linked tracks."));   // check that cur is in the list
   #endif

   if (!l->isNull(cur))
      cur = l->getNext( cur );

   #ifdef DEBUG_TLI // if we are debugging this bit
   wxASSERT_MSG((!cur || (*l).Contains((*cur).t)), wxT("cur invalid after moving to next track."));   // check that cur is in the list if it is not null
   #endif

   if (!l->isNull(cur))
      return cur.first->get();

   return nullptr;
}

Track *TrackListIterator::Prev(bool skiplinked)
{
   if (!l || l->isNull(cur))
      return nullptr;

   cur = l->getPrev( cur );
   if ( l->isNull( cur ) )
      return nullptr;

   if ( skiplinked ) {
      auto prev = l->getPrev( cur );
      if( !l->isNull( prev ) && (*prev.first)->GetLinked() )
         cur = prev;
   }

   return cur.first->get();
}

Track *TrackListIterator::operator *() const
{
   if ( !l || l->isNull( cur ) )
      return nullptr;
   else
      return cur.first->get();
}

Track *TrackListIterator::RemoveCurrent()
{
   if ( !l || l->isNull( cur ) )
      return nullptr;

   cur = l->Remove( cur.first->get() );

   #ifdef DEBUG_TLI // if we are debugging this bit
   wxASSERT_MSG((!cur || (*l).Contains((*cur).t)), wxT("cur invalid after deletion of track."));   // check that cur is in the list
   #endif

   if ( !l->isNull( cur ) )
      return cur.first->get();

   return nullptr;
}

bool TrackListIterator::operator == (const TrackListIterator &other) const
{
   // Order these steps so as not to use operator == on default-constructed
   // std::list::iterator -- that crashes in the MSVC 2013 standard library
   bool isEnd = !l || l->isNull( cur );
   bool otherIsEnd = !other.l || other.l->isNull( other.cur );

   return (isEnd == otherIsEnd && (isEnd || cur == other.cur));
}

//
// TrackListCondIterator (base class for iterators that iterate over all tracks
// that meet a condition)
//

Track *TrackListCondIterator::StartWith(Track *val)
{
   Track *t = TrackListIterator::StartWith(val);

   if (t && !this->Condition(t))
      return nullptr;

   return t;
}

Track *TrackListCondIterator::First(TrackList *val)
{
   Track *t = TrackListIterator::First(val);

   while (t && !this->Condition(t)) {
      t = TrackListIterator::Next();
   }

   return t;
}

Track *TrackListCondIterator::Next(bool skiplinked)
{
   while (Track *t = TrackListIterator::Next(skiplinked)) {
      if (this->Condition(t)) {
         return t;
      }
   }

   return NULL;
}

Track *TrackListCondIterator::Prev(bool skiplinked)
{
   while (Track *t = TrackListIterator::Prev(skiplinked))
   {
      if (this->Condition(t)) {
         return t;
      }
   }

   return NULL;
}

Track *TrackListCondIterator::Last(bool skiplinked)
{
   Track *t = TrackListIterator::Last(skiplinked);

   while (t && !this->Condition(t)) {
      t = TrackListIterator::Prev(skiplinked);
   }

   return t;
}

// TrackListOfKindIterator
TrackListOfKindIterator::TrackListOfKindIterator(int kind, TrackList * val)
:  TrackListCondIterator(val)
{
   this->kind = kind;
}

bool TrackListOfKindIterator::Condition(Track *t)
{
   return kind == Track::All || t->GetKind() == kind;
}

//SelectedTrackListOfKindIterator
bool SelectedTrackListOfKindIterator::Condition(Track *t)
{
   return TrackListOfKindIterator::Condition(t) && t->GetSelected();
}

// VisibleTrackIterator
//
// Based on TrackListIterator returns only the currently visible tracks.
//
VisibleTrackIterator::VisibleTrackIterator(AudacityProject *project)
:  TrackListCondIterator(project->GetTracks())
{
   mProject = project;
   mPanelRect.SetTop(mProject->mViewInfo.vpos);
   mPanelRect.SetSize(mProject->GetTPTracksUsableArea());
}

bool VisibleTrackIterator::Condition(Track *t)
{
   wxRect r(0, t->GetY(), 1, t->GetHeight());
   if( r.Intersects(mPanelRect) )
      return true;
   auto partner = t->GetLink();
   if ( partner && t->GetLinked() )
      return Condition( partner );
   return false;
}

// SyncLockedTracksIterator
//
// Based on TrackListIterator returns only tracks belonging to the group
// in which the starting track is a member.
//
SyncLockedTracksIterator::SyncLockedTracksIterator(TrackList * val)
:  TrackListIterator(val),
   mInLabelSection(false)
{
}

namespace {
   inline bool IsSyncLockableNonLabelTrack( const Track *pTrack )
   {
      return nullptr != dynamic_cast< const AudioTrack * >( pTrack );
   }
}

Track *SyncLockedTracksIterator::StartWith(Track * member)
{
   Track *t = NULL;

   // A sync-locked group consists of any positive number of wave (or note)
   // tracks followed by any
   // non-negative number of label tracks. Step back through any label tracks,
   // and then through the wave tracks above them.

   while (member && member->GetKind() == Track::Label) {
      member = l->GetPrev(member);
   }

   while (member && IsSyncLockableNonLabelTrack(member)) {
      t = member;
      member = l->GetPrev(member);
   }

   // Make it current (if t is still NULL there are no wave tracks, so we're
   // not in a sync-locked group).
   if (t)
      cur = t->GetNode();

   mInLabelSection = false;

   return t;
}

bool SyncLockedTracksIterator::IsGoodNextTrack(const Track *t) const
{
   if (!t)
      return false;

   const bool isLabel = ( t->GetKind() == Track::Label );
   const bool isSyncLockable = IsSyncLockableNonLabelTrack( t );

   if ( !( isLabel || isSyncLockable ) ) {
      return false;
   }

   if (mInLabelSection && !isLabel) {
      return false;
   }

   return true;
}

Track *SyncLockedTracksIterator::Next(bool skiplinked)
{
   Track *t = TrackListIterator::Next(skiplinked);

   if (!t)
      return nullptr;

   if ( ! IsGoodNextTrack(t) ) {
      cur = l->getEnd();
      return nullptr;
   }

   mInLabelSection = ( t->GetKind() == Track::Label );

   return t;
}

Track *SyncLockedTracksIterator::Prev(bool skiplinked)
{
   Track *t = TrackListIterator::Prev(skiplinked);

   //
   // Ways to end a sync-locked group in reverse
   //

   // Beginning of tracks
   if (!t)
      return nullptr;

   const bool isLabel = ( t->GetKind() == Track::Label );
   const bool isSyncLockable = IsSyncLockableNonLabelTrack( t );

   if ( !( isLabel || isSyncLockable ) ) {
      cur = l->getEnd();
      return nullptr;
   }

   if ( !mInLabelSection && isLabel ) {
      cur = l->getEnd();
      return nullptr;
   }

   mInLabelSection = isLabel;

   return t;
}

Track *SyncLockedTracksIterator::Last(bool skiplinked)
{
   if ( !l || l->isNull( cur ) )
      return nullptr;

   Track *t = cur.first->get();

   while (const auto next = l->GetNext(t, skiplinked)) {
      if ( ! IsGoodNextTrack(next) )
         break;
      t = Next(skiplinked);
   }

   return t;
}


// TrackList
//
// The TrackList sends events whenever certain updates occur to the list it
// is managing.  Any other classes that may be interested in get these updates
// should use TrackList::Connect() or TrackList::Bind().
//
wxDEFINE_EVENT(EVT_TRACKLIST_PERMUTED, wxCommandEvent);
wxDEFINE_EVENT(EVT_TRACKLIST_RESIZING, wxCommandEvent);
wxDEFINE_EVENT(EVT_TRACKLIST_DELETION, wxCommandEvent);

// same value as in the default constructed TrackId:
long TrackList::sCounter = -1;

TrackList::TrackList()
:  wxEvtHandler()
{
}

// Factory function
std::shared_ptr<TrackList> TrackList::Create()
{
   std::shared_ptr<TrackList> result{ safenew TrackList{} };
   result->mSelf = result;
   return result;
}

TrackList &TrackList::operator= (TrackList &&that)
{
   if (this != &that) {
      this->Clear();
      Swap(that);
   }
   return *this;
}

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

   SwapLOTs( *this, mSelf, that, that.mSelf );
   SwapLOTs( this->mPendingUpdates, mSelf, that.mPendingUpdates, that.mSelf );
   mUpdaters.swap(that.mUpdaters);
}

TrackList::~TrackList()
{
   Clear(false);
}

void TrackList::RecalcPositions(TrackNodePointer node)
{
   if ( isNull( node ) )
      return;

   Track *t;
   int i = 0;
   int y = 0;

   auto prev = getPrev( node );
   if ( !isNull( prev ) ) {
      t = prev.first->get();
      i = t->GetIndex() + 1;
      y = t->GetY() + t->GetHeight();
   }

   const auto theEnd = end();
   for (auto n = TrackListIterator{ this, node }; n != theEnd; ++n) {
      t = *n;
      t->SetIndex(i++);
      t->DoSetY(y);
      y += t->GetHeight();
   }

   UpdatePendingTracks();
}

void TrackList::PermutationEvent()
{
   auto e = std::make_unique<wxCommandEvent>(EVT_TRACKLIST_PERMUTED);
   // wxWidgets will own the event object
   QueueEvent(e.release());
}

void TrackList::DeletionEvent()
{
   auto e = std::make_unique<wxCommandEvent>(EVT_TRACKLIST_DELETION);
   // wxWidgets will own the event object
   QueueEvent(e.release());
}

void TrackList::ResizingEvent(TrackNodePointer node)
{
   auto e = std::make_unique<TrackListEvent>(EVT_TRACKLIST_RESIZING);
   e->mpTrack = *node.first;
   // wxWidgets will own the event object
   QueueEvent(e.release());
}

void TrackList::Permute(const std::vector<TrackNodePointer> &permutation)
{
   for (const auto iter : permutation) {
      ListOfTracks::value_type track = std::move(*iter.first);
      erase(iter.first);
      Track *pTrack = track.get();
      pTrack->SetOwner(mSelf,
                       { insert(ListOfTracks::end(), std::move(track)), this });
   }
   auto n = getBegin();
   RecalcPositions(n);
   PermutationEvent();
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

template<typename TrackKind>
Track *TrackList::Add(std::unique_ptr<TrackKind> &&t)
{
   Track *pTrack;
   push_back(ListOfTracks::value_type(pTrack = t.release()));

   auto n = getPrev( getEnd() );

   pTrack->SetOwner(mSelf, n);
   pTrack->SetId( TrackId{ ++sCounter } );
   RecalcPositions(n);
   ResizingEvent(n);
   return back().get();
}

// Make instantiations for the linker to find
template Track *TrackList::Add<TimeTrack>(std::unique_ptr<TimeTrack> &&);
#if defined(USE_MIDI)
template Track *TrackList::Add<NoteTrack>(std::unique_ptr<NoteTrack> &&);
#endif
template Track *TrackList::Add<WaveTrack>(std::unique_ptr<WaveTrack> &&);
template Track *TrackList::Add<LabelTrack>(std::unique_ptr<LabelTrack> &&);
template Track *TrackList::Add<Track>(std::unique_ptr<Track> &&);

template<typename TrackKind>
Track *TrackList::AddToHead(std::unique_ptr<TrackKind> &&t)
{
   Track *pTrack;
   push_front(ListOfTracks::value_type(pTrack = t.release()));
   auto n = getBegin();
   pTrack->SetOwner(mSelf, n);
   pTrack->SetId( TrackId{ ++sCounter } );
   RecalcPositions(n);
   ResizingEvent(n);
   return front().get();
}

// Make instantiations for the linker to find
template Track *TrackList::AddToHead<TimeTrack>(std::unique_ptr<TimeTrack> &&);

template<typename TrackKind>
Track *TrackList::Add(std::shared_ptr<TrackKind> &&t)
{
   push_back(t);

   auto n = getPrev( getEnd() );

   t->SetOwner(mSelf, n);
   t->SetId( TrackId{ ++sCounter } );
   RecalcPositions(n);
   ResizingEvent(n);
   return back().get();
}

// Make instantiations for the linker to find
template Track *TrackList::Add<Track>(std::shared_ptr<Track> &&);
template Track *TrackList::Add<WaveTrack>(std::shared_ptr<WaveTrack> &&);

auto TrackList::Replace(Track * t, ListOfTracks::value_type &&with) ->
   ListOfTracks::value_type
{
   ListOfTracks::value_type holder;
   if (t && with) {
      auto node = t->GetNode();
      t->SetOwner({}, {});

      holder = std::move(*node.first);

      Track *pTrack = with.get();
      *node.first = std::move(with);
      pTrack->SetOwner(mSelf, node);
      pTrack->SetId( t->GetId() );
      RecalcPositions(node);

      DeletionEvent();
      ResizingEvent(node);
   }
   return holder;
}

TrackNodePointer TrackList::Remove(Track *t)
{
   auto result = getEnd();
   if (t) {
      auto node = t->GetNode();
      t->SetOwner({}, {});

      if ( !isNull( node ) ) {
         ListOfTracks::value_type holder = std::move( *node.first );

         result = getNext( node );
         erase(node.first);
         if ( !isNull( result ) )
            RecalcPositions(result);

         DeletionEvent();
      }
   }
   return result;
}

void TrackList::Clear(bool sendEvent)
{
   // Null out the back-pointers in tracks, in case there are outstanding
   // shared_ptrs to those tracks.
   for ( auto pTrack: *this )
      pTrack->SetOwner( {}, {} );
   for ( auto pTrack: mPendingUpdates )
      pTrack->SetOwner( {}, {} );

   ListOfTracks tempList;
   tempList.swap( *this );

   ListOfTracks updating;
   updating.swap( mPendingUpdates );

   mUpdaters.clear();

   if (sendEvent)
      DeletionEvent();
}

void TrackList::Select(Track * t, bool selected /* = true */ )
{
   if (t) {
      const auto node = t->GetNode();
      if ( !isNull( node ) ) {
         t->SetSelected( selected );
         if ( t->GetLinked() ) {
            auto next = getNext( node );
            if ( !isNull( next ) )
               (*next.first)->SetSelected( selected );
         }
         else {
            auto prev = getPrev( node );
            if ( !isNull( prev ) && (*prev.first)->GetLinked() )
               (*prev.first)->SetSelected( selected );
         }
      }
   }
}

/// Return a track in the list that comes after Track t
Track *TrackList::GetNext(Track * t, bool linked) const
{
   if (t) {
      auto node = t->GetNode();
      if ( !isNull( node ) ) {
         if ( linked && t->GetLinked() )
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
                !t->GetLinked() && t->GetLink() )
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
                   !(*node.first)->GetLinked() && (*node.first)->GetLink() )
                  node = prev;
            }

            return node.first->get();
         }
      }
   }

   return nullptr;
}

/// For mono track height of track
/// For stereo track combined height of both channels.
int TrackList::GetGroupHeight(Track * t) const
{
   int height = t->GetHeight();

   t = t->GetLink();
   if (t) {
      height += t->GetHeight();
   }
   return height;
}

bool TrackList::CanMoveUp(Track * t) const
{
   return GetPrev(t, true) != NULL;
}

bool TrackList::CanMoveDown(Track * t) const
{
   return GetNext(t, true) != NULL;
}

// This is used when you want to swap the track or pair of
// tracks in s1 with the track or pair of tracks in s2.
// The complication is that the tracks are stored in a single
// linked list, and pairs of tracks are marked only by a flag
// in one of the tracks.
void TrackList::SwapNodes(TrackNodePointer s1, TrackNodePointer s2)
{
   // if a null pointer is passed in, we want to know about it
   wxASSERT(!isNull(s1));
   wxASSERT(!isNull(s2));

   // Deal with first track in each team
   Track *link;
   link = (*s1.first)->GetLink();
   bool linked1 = link != nullptr;
   if (linked1 && !(*s1.first)->GetLinked()) {
      s1 = link->GetNode();
   }

   link = (*s2.first)->GetLink();
   bool linked2 = link != nullptr;
   if (linked2 && !(*s2.first)->GetLinked()) {
      s2 = link->GetNode();
   }

   // Safety check...
   if (s1 == s2)
      return;

   // Be sure s1 is the earlier iterator
   if ((*s1.first)->GetIndex() >= (*s2.first)->GetIndex()) {
      std::swap(s1, s2);
      std::swap(linked1, linked2);
   }

   // Remove tracks
   ListOfTracks::value_type save11 = std::move(*s1.first), save12{};
   s1.first = erase(s1.first);
   if (linked1) {
      wxASSERT(s1 != s2);
      save12 = std::move(*s1.first), s1.first = erase(s1.first);
   }
   const bool same = (s1 == s2);

   ListOfTracks::value_type save21 = std::move(*s2.first), save22{};
   s2.first = erase(s2.first);
   if (linked2)
      save22 = std::move(*s2.first), s2.first = erase(s2.first);

   if (same)
      // We invalidated s1!
      s1 = s2;

   // Reinsert them
   Track *pTrack;
   if (save22)
      pTrack = save22.get(),
      pTrack->SetOwner(mSelf, s1 = { insert(s1.first, std::move(save22)), this });
   pTrack = save21.get(),
   pTrack->SetOwner(mSelf, s1 = { insert(s1.first, std::move(save21)), this });

   if (save12)
      pTrack = save12.get(),
      pTrack->SetOwner(mSelf, s2 = { insert(s2.first, std::move(save12)), this });
   pTrack = save11.get(),
   pTrack->SetOwner(mSelf, s2 = { insert(s2.first, std::move(save11)), this });

   // Now correct the Index in the tracks, and other things
   RecalcPositions(s1);
   PermutationEvent();
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

size_t TrackList::size() const
{
   int cnt = 0;

   if (!empty())
      cnt = getPrev( getEnd() ).first->get()->GetIndex() + 1;

   return cnt;
}

TimeTrack *TrackList::GetTimeTrack()
{
   auto iter = std::find_if(begin(), end(),
      [] ( Track *t ) { return t->GetKind() == Track::Time; }
   );
   if (iter == end())
      return nullptr;
   else
      return static_cast<TimeTrack*>(*iter);
}

const TimeTrack *TrackList::GetTimeTrack() const
{
   return const_cast<TrackList*>(this)->GetTimeTrack();
}

unsigned TrackList::GetNumExportChannels(bool selectionOnly) const
{
   /* counters for tracks panned different places */
   int numLeft = 0;
   int numRight = 0;
   //int numMono = 0;
   /* track iteration kit */
   const Track *tr;
   TrackListConstIterator iter;

   for (tr = iter.First(this); tr != NULL; tr = iter.Next()) {

      // Want only unmuted wave tracks.
      auto wt = static_cast<const WaveTrack *>(tr);
      if ((tr->GetKind() != Track::Wave) ||
          wt->GetMute())
         continue;

      // do we only want selected ones?
      if (selectionOnly && !(tr->GetSelected())) {
         //want selected but this one is not
         continue;
      }

      // Found a left channel
      if (tr->GetChannel() == Track::LeftChannel) {
         numLeft++;
      }

      // Found a right channel
      else if (tr->GetChannel() == Track::RightChannel) {
         numRight++;
      }

      // Found a mono channel, but it may be panned
      else if (tr->GetChannel() == Track::MonoChannel) {
         float pan = ((WaveTrack*)tr)->GetPan();

         // Figure out what kind of channel it should be
         if (pan == -1.0) {   // panned hard left
            numLeft++;
         }
         else if (pan == 1.0) {  // panned hard right
            numRight++;
         }
         else if (pan == 0) { // panned dead center
            // numMono++;
         }
         else {   // panned somewhere else
            numLeft++;
            numRight++;
         }
      }
   }

   // if there is stereo content, report 2, else report 1
   if (numRight > 0 || numLeft > 0) {
      return 2;
   }

   return 1;
}

namespace {
   template<typename Array>
   Array GetWaveTracks(TrackListIterator p, const TrackListIterator end,
                       bool selectionOnly, bool includeMuted)
   {
      Array waveTrackArray;

      for (; p != end; ++p) {
         const auto &track = *p;
         auto wt = static_cast<const WaveTrack *>(&*track);
         if (track->GetKind() == Track::Wave &&
            (includeMuted || !wt->GetMute()) &&
            (track->GetSelected() || !selectionOnly)) {
            waveTrackArray.push_back( Track::Pointer< WaveTrack >( track ) );
         }
      }

      return waveTrackArray;
   }
}

WaveTrackArray TrackList::GetWaveTrackArray(bool selectionOnly, bool includeMuted)
{
   return GetWaveTracks<WaveTrackArray>(begin(), end(), selectionOnly, includeMuted);
}

WaveTrackConstArray TrackList::GetWaveTrackConstArray(bool selectionOnly, bool includeMuted) const
{
   auto list = const_cast<TrackList*>(this);
   return GetWaveTracks<WaveTrackConstArray>(
      list->begin(), list->end(), selectionOnly, includeMuted);
}

#if defined(USE_MIDI)
NoteTrackArray TrackList::GetNoteTrackArray(bool selectionOnly)
{
   NoteTrackArray noteTrackArray;

   for(const auto &track : *this) {
      if (track->GetKind() == Track::Note &&
         (track->GetSelected() || !selectionOnly)) {
         noteTrackArray.push_back( Track::Pointer<NoteTrack>(track) );
      }
   }

   return noteTrackArray;
}
#endif

int TrackList::GetHeight() const
{
   int height = 0;

   if (!empty()) {
      auto track = getPrev( getEnd() ).first->get();
      height = track->GetY() + track->GetHeight();
   }

   return height;
}

namespace {
   // Abstract the common pattern of the following three member functions
   double doubleMin(double a, double b) { return std::min(a, b); }
   double doubleMax(double a, double b) { return std::max(a, b); }
   inline double Accumulate
      (const TrackList &list,
       double (Track::*memfn)() const,
       double (*combine)(double, double))
   {
      // Default the answer to zero for empty list
      if (list.empty()) {
         return 0.0;
      }

      // Otherwise accumulate minimum or maximum of track values
      auto iter = list.begin();
      double acc = (**iter++.*memfn)();
      return std::accumulate(iter, list.end(), acc,
         [=](double acc, const Track *pTrack) {
            return combine(acc, (*pTrack.*memfn)());
      });
   }
}

double TrackList::GetMinOffset() const
{
   return Accumulate(*this, &Track::GetOffset, doubleMin);
}

double TrackList::GetStartTime() const
{
   return Accumulate(*this, &Track::GetStartTime, doubleMin);
}

double TrackList::GetEndTime() const
{
   return Accumulate(*this, &Track::GetEndTime, doubleMax);
}

std::shared_ptr<Track>
TrackList::RegisterPendingChangedTrack( Updater updater, Track *src )
{
   std::shared_ptr<Track> pTrack;
   if (src)
      // convert from unique_ptr to shared_ptr
      pTrack.reset( src->Duplicate().release() );

   if (pTrack) {
      mUpdaters.push_back( updater );
      mPendingUpdates.push_back( pTrack );
      auto n = mPendingUpdates.end();
      --n;
      pTrack->SetOwner(mSelf, {n, &mPendingUpdates});
   }

   return pTrack;
}

void TrackList::RegisterPendingNewTrack( const std::shared_ptr<Track> &pTrack )
{
   auto copy = pTrack;
   Add<Track>( std::move( copy ) );
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
         pendingTrack->DoSetY(src->GetY());
         pendingTrack->DoSetHeight(src->GetHeight());
         pendingTrack->DoSetMinimized(src->GetMinimized());
         pendingTrack->DoSetLinked(src->GetLinked());
      }
      ++pUpdater;
   }
}

void TrackList::ClearPendingTracks( ListOfTracks *pAdded )
// NOFAIL-GUARANTEE
{
   for (const auto &pTrack: mPendingUpdates)
      pTrack->SetOwner( {}, {} );
   mPendingUpdates.clear();
   mUpdaters.clear();

   if (pAdded)
      pAdded->clear();

   for (auto it = ListOfTracks::begin(), stop = ListOfTracks::end();
        it != stop;) {
      if (it->get()->GetId() == TrackId{}) {
         if (pAdded)
            pAdded->push_back( *it );
         it = erase( it );
      }
      else
         ++it;
   }

   if (!empty())
      RecalcPositions(getBegin());
}

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

   // Remaining steps must be NOFAIL-GUARANTEE so that this function
   // gives STRONG-GUARANTEE

   std::vector< std::shared_ptr<Track> > reinstated;

   for (auto &pendingTrack : updates) {
      if (pendingTrack) {
         auto src = FindById( pendingTrack->GetId() );
         if (src)
            this->Replace(src, std::move(pendingTrack)), result = true;
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
         this->Add(std::move(pendingTrack)), result = true;

   // Put the pending added tracks back into the list, preserving their
   // positions.
   bool inserted = false;
   ListOfTracks::iterator first;
   for (auto &pendingTrack : additions) {
      if (pendingTrack) {
         auto iter = ListOfTracks::begin();
         std::advance( iter, pendingTrack->GetIndex() );
         iter = ListOfTracks::insert( iter, pendingTrack );
         pendingTrack->SetOwner( mSelf, {iter, this} );
         pendingTrack->SetId( TrackId{ ++sCounter } );
         if (!inserted) {
            first = iter;
            inserted = true;
         }
      }
   }
   if (inserted) {
      RecalcPositions({first, this});
      result = true;
   }

   return result;
}

std::shared_ptr<Track> TrackList::FindPendingChangedTrack(TrackId id) const
{
   // Linear search.  Tracks in a project are usually very few.
   auto it = std::find_if( mPendingUpdates.begin(), mPendingUpdates.end(),
      [=](const ListOfTracks::value_type &ptr){ return ptr->GetId() == id; } );
   if (it == mPendingUpdates.end())
      return {};
   return *it;
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
