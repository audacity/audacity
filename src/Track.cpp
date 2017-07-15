/**********************************************************************

  Audacity: A Digital Audio Editor

  Track.cpp

  Dominic Mazzoni

*******************************************************************//**

\class Track
\brief Fundamental data object of Audacity, placed in the TrackPanel.
Classes derived form it include the WaveTrack, NoteTrack, LabelTrack
and TimeTrack.

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
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   mYv = mHeight;
   mHeightv = mHeight;
   mPerY = 0.5;
   mVirtualStereo = false;
#endif

   mMinimized = false;

   mOffset = 0.0;

   mChannel = MonoChannel;
}

Track::Track(const Track &orig)
: vrulerSize( orig.vrulerSize )
{
   mY = 0;
   mIndex = 0;
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   mPerY = 0.5;
#endif
   Init(orig);
   mOffset = orig.mOffset;
}

// Copy all the track properties except the actual contents
void Track::Init(const Track &orig)
{
   mDefaultName = orig.mDefaultName;
   mName = orig.mName;

   mDirManager = orig.mDirManager;

   mSelected = orig.mSelected;
   mLinked = orig.mLinked;
   mHeight = orig.mHeight;
   mMinimized = orig.mMinimized;
   mChannel = orig.mChannel;
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   mVirtualStereo = orig.mVirtualStereo;
   mHeightv = orig.mHeightv;
   mYv = orig.mYv;
#endif
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
   wxASSERT(mList.lock() == NULL || this == mNode->get());
   return mNode;
}

void Track::SetOwner
(const std::weak_ptr<TrackList> &list, TrackNodePointer node)
{
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

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
int Track::GetY(bool vStereo) const
{
   if(vStereo && mChannel == Track::MonoChannel) return mYv;
   return mY;
}

void Track::SetY(int y, bool vStereo)
{
   if(vStereo && mChannel == Track::MonoChannel) mYv = y;
   else mY = y;
}

int Track::GetHeight(bool vStereo) const
{
   if (mMinimized) {
      return GetMinimizedHeight();
   }

   if(vStereo && mChannel == Track::MonoChannel) return mHeightv;
   return mHeight;
}

void Track::SetHeight(int h, bool vStereo)
{

   if(vStereo && mChannel == Track::MonoChannel) mHeightv = h;
   else mHeight = h;

   if (mList) {
      mList->RecalcPositions(mNode);
      mList->ResizedEvent(mNode);
   }
}

#else // EXPERIMENTAL_OUTPUT_DISPLAY

int Track::GetY() const
{
   return mY;
}

void Track::SetY(int y)
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
   mHeight = h;
   auto pList = mList.lock();
   if (pList) {
      pList->RecalcPositions(mNode);
      pList->ResizingEvent(mNode);
   }
}
#endif // EXPERIMENTAL_OUTPUT_DISPLAY

bool Track::GetMinimized() const
{
   return mMinimized;
}

void Track::SetMinimized(bool isMinimized)
{
   auto pList = mList.lock();
   mMinimized = isMinimized;
   if (pList) {
      pList->RecalcPositions(mNode);
      pList->ResizingEvent(mNode);
   }
}

void Track::SetLinked(bool l)
{
   auto pList = mList.lock();
   mLinked = l;
   if (pList) {
      pList->RecalcPositions(mNode);
      pList->ResizingEvent(mNode);
   }
}

Track *Track::GetLink() const
{
   auto pList = mList.lock();
   if (!pList)
      return nullptr;

   if (!pList->isNull(mNode)) {
      if (mLinked) {
         auto next = mNode;
         ++next;
         if (!pList->isNull(next)) {
            return next->get();
         }
      }

      if (pList->hasPrev(mNode)) {
         auto prev = mNode;
         --prev;
         auto track = prev->get();
         if (track && track->GetLinked()) {
            return track;
         }
      }
   }

   return nullptr;
}

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
void Track::ReorderList(bool resize)
{
   if (mList) {
      mList->RecalcPositions(mNode);
      if(resize)
         mList->ResizedEvent(mNode);
   }
}
#endif

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
TrackListIterator::TrackListIterator(TrackList * val)
   : l(val)
   , cur{}
{
   if (l)
      cur = l->begin();
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
   return cur->get();
}

Track *TrackListIterator::First(TrackList * val)
{
   if (val != NULL) {
      l = val;
   }

   if (l == NULL) {
      return NULL;
   }

   cur = l->begin();

   if (!l->isNull(cur)) {
      return cur->get();
   }

   return NULL;
}

Track *TrackListIterator::Last(bool skiplinked)
{
   if (l == NULL) {
      return NULL;
   }

   cur = l->end();
   if (l->hasPrev(cur))
      --cur;
   else
      return NULL;

   // With skiplinked set, we won't return the second channel of a linked pair
   if (skiplinked &&
       l->hasPrev(cur) &&
       !(*cur)->GetLinked() &&
       (*cur)->GetLink())
      --cur;

   return cur->get();
}

Track *TrackListIterator::Next(bool skipLinked)
{
#ifdef DEBUG_TLI // if we are debugging this bit
   wxASSERT_MSG((!cur || (*l).Contains((*cur).t)), wxT("cur invalid at start of Next(). List changed since iterator created?"));   // check that cur is in the list
#endif

   if (!l || l->isNull(cur))
      return nullptr;

   if (skipLinked &&
       (*cur)->GetLinked()) {
      ++cur;
   }

   #ifdef DEBUG_TLI // if we are debugging this bit
   wxASSERT_MSG((!cur || (*l).Contains((*cur).t)), wxT("cur invalid after skipping linked tracks."));   // check that cur is in the list
   #endif

   if (!l->isNull(cur)) {
      ++cur;
   }

   #ifdef DEBUG_TLI // if we are debugging this bit
   wxASSERT_MSG((!cur || (*l).Contains((*cur).t)), wxT("cur invalid after moving to next track."));   // check that cur is in the list if it is not null
   #endif

   if (!l->isNull(cur)) {
      return cur->get();
   }

   return NULL;
}

Track *TrackListIterator::Prev(bool skiplinked)
{
   if (!l || l->isNull(cur))
      return nullptr;

   if (!l->hasPrev(cur)) {
      l->setNull(cur);
      return nullptr;
   }

   --cur;

   if (skiplinked && l->hasPrev(cur)) {
      auto prev = cur;
      --prev;
      if ((*prev)->GetLinked())
         cur = prev;
   }

   return cur->get();
}

Track *TrackListIterator::RemoveCurrent()
{
   if (!l || l->isNull(cur))
      return nullptr;

   cur = l->Remove(cur->get());

   #ifdef DEBUG_TLI // if we are debugging this bit
   wxASSERT_MSG((!cur || (*l).Contains((*cur).t)), wxT("cur invalid after deletion of track."));   // check that cur is in the list
   #endif

   if (!l->isNull(cur)) {
      return cur->get();
   }

   return NULL;
}

//
// TrackListCondIterator (base class for iterators that iterate over all tracks
// that meet a condition)
//

Track *TrackListCondIterator::StartWith(Track *val)
{
   Track *t = TrackListIterator::StartWith(val);

   if (t && !this->Condition(t))
      return NULL;

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
      return NULL;

   if ( ! IsGoodNextTrack(t) ) {
      l->setNull(cur);
      return NULL;
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
      return NULL;

   const bool isLabel = ( t->GetKind() == Track::Label );
   const bool isSyncLockable = IsSyncLockableNonLabelTrack( t );

   if ( !( isLabel || isSyncLockable ) ) {
      l->setNull(cur);
      return NULL;
   }

   if ( !mInLabelSection && isLabel ) {
      l->setNull(cur);
      return NULL;
   }

   mInLabelSection = isLabel;

   return t;
}

Track *SyncLockedTracksIterator::Last(bool skiplinked)
{
   if (!l || l->isNull(cur))
      return NULL;

   Track *t = cur->get();

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
// should use TrackList::Connect() and TrackList::Disconnect().
//
DEFINE_EVENT_TYPE(EVT_TRACKLIST_PERMUTED);
DEFINE_EVENT_TYPE(EVT_TRACKLIST_RESIZING);
DEFINE_EVENT_TYPE(EVT_TRACKLIST_DELETION);

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

TrackList& TrackList::operator= (const TrackList &that)
{
   if (this != &that) {
      this->Clear();
      DoAssign(that);
   }
   return *this;
}

TrackList &TrackList::operator= (TrackList &&that)
{
   if (this != &that) {
      this->Clear();
      Swap(that);
   }
   return *this;
}

void TrackList::DoAssign(const TrackList &that)
{
   TrackListConstIterator it(&that);
   for (const Track *track = it.First(); track; track = it.Next())
      Add(track->Duplicate());
}

void TrackList::Swap(TrackList &that)
{
   ListOfTracks::swap(that);
   for (auto it = begin(), last = end(); it != last; ++it)
      (*it)->SetOwner(this->mSelf, it);
   for (auto it = that.begin(), last = that.end(); it != last; ++it)
      (*it)->SetOwner(that.mSelf, it);
}

TrackList::~TrackList()
{
   Clear(false);
}

void TrackList::RecalcPositions(TrackNodePointer node)
{
   if (isNull(node)) {
      return;
   }
   Track *t;
   int i = 0;
   int y = 0;

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   int cnt = 0;
   if (hasPrev(node)) {
      auto prev = node;
      --prev;
      t = prev->get();
      i = t->GetIndex() + 1;
      if(MONO_WAVE_PAN(t))
         y = t->GetY(true) + t->GetHeight(true);
      else
         y = t->GetY() + t->GetHeight();
   }

   for (auto n = node; n != end(); ++n) {
      t = n->get();
      if(MONO_WAVE_PAN(t))
         cnt++;

      if(cnt != 2){
         t->SetIndex(i++);
         t->SetY(y);
         y += t->GetHeight();
      }
      if(cnt != 0){
         t->SetY(y,true);
         y += t->GetHeight(true);
      }
      cnt = 0;
   }
#else // EXPERIMENTAL_OUTPUT_DISPLAY
   if (hasPrev(node)) {
      auto prev = node;
      --prev;
      t = prev->get();
      i = t->GetIndex() + 1;
      y = t->GetY() + t->GetHeight();
   }

   const auto theEnd = end();
   for (auto n = node; n != theEnd; ++n) {
      t = n->get();
      t->SetIndex(i++);
      t->SetY(y);
      y += t->GetHeight();
   }
#endif // EXPERIMENTAL_OUTPUT_DISPLAY
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
   auto e = std::make_unique<wxCommandEvent>(EVT_TRACKLIST_RESIZING);
   if (!isNull(node))
      e->SetClientData(node->get());
   // wxWidgets will own the event object
   QueueEvent(e.release());
}

void TrackList::Permute(const std::vector<TrackNodePointer> &permutation)
{
   for (const auto iter : permutation) {
      value_type track = std::move(*iter);
      erase(iter);
      Track *pTrack = track.get();
      pTrack->SetOwner(mSelf, insert(end(), std::move(track)));
   }
   auto n = begin();
   RecalcPositions(n);
   PermutationEvent();
}

template<typename TrackKind>
Track *TrackList::Add(std::unique_ptr<TrackKind> &&t)
{
   Track *pTrack;
   push_back(value_type(pTrack = t.release()));
   auto n = end();
   --n;
   pTrack->SetOwner(mSelf, n);
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
   push_front(value_type(pTrack = t.release()));
   auto n = begin();
   pTrack->SetOwner(mSelf, n);
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
   auto n = end();
   --n;
   t->SetOwner(mSelf, n);
   RecalcPositions(n);
   ResizingEvent(n);
   return back().get();
}

// Make instantiations for the linker to find
template Track *TrackList::Add<Track>(std::shared_ptr<Track> &&);
template Track *TrackList::Add<WaveTrack>(std::shared_ptr<WaveTrack> &&);

auto TrackList::Replace(Track * t, value_type &&with) -> value_type
{
   value_type holder;
   if (t && with) {
      auto node = t->GetNode();
      t->SetOwner({}, {});

      holder = std::move(*node);

      Track *pTrack = with.get();
      *node = std::move(with);
      pTrack->SetOwner(mSelf, node);
      RecalcPositions(node);

      DeletionEvent();
      ResizingEvent(node);
   }
   return holder;
}

TrackNodePointer TrackList::Remove(Track *t)
{
   TrackNodePointer result(end());
   if (t) {
      auto node = t->GetNode();
      t->SetOwner({}, {});

      if (!isNull(node)) {
         value_type holder = std::move( *node );

         result = erase(node);
         if (!isNull(result)) {
            RecalcPositions(result);
         }

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

   ListOfTracks tempList;
   tempList.swap( *this );
   if (sendEvent)
      DeletionEvent();
}

void TrackList::Select(Track * t, bool selected /* = true */ )
{
   if (t) {
      const auto node = t->GetNode();
      if (!isNull(node)) {
         t->SetSelected(selected);
         auto next = node;
         ++next;
         if (t->GetLinked() && !isNull(next)) {
            (*next)->SetSelected(selected);
         }
         else if (hasPrev(node)) {
            auto prev = node;
            --prev;
            if ((*prev)->GetLinked()) {
               (*prev)->SetSelected(selected);
            }
         }
      }
   }
}

/// Return a track in the list that comes after Track t
Track *TrackList::GetNext(Track * t, bool linked) const
{
   if (t) {
      auto node = t->GetNode();
      if (!isNull(node)) {
         if (linked && t->GetLinked()) {
            ++node;
         }

         if (!isNull(node)) {
            ++node;
         }

         if (!isNull(node)) {
            return node->get();
         }
      }
   }

   return NULL;
}

Track *TrackList::GetPrev(Track * t, bool linked) const
{
   if (t) {
      auto node = t->GetNode();
      if (!isNull(node)) {
         // linked is true and input track second in team?
         if (linked && hasPrev(node) &&
             !t->GetLinked() && t->GetLink())
               // Make it the first
            --node;

         if (hasPrev(node)) {
            // Back up once
            --node;

            // Back up twice sometimes when linked is true
            if (linked && hasPrev(node) &&
                !(*node)->GetLinked() && (*node)->GetLink())
               --node;

            return node->get();
         }
      }
   }

   return NULL;
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
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   else if(MONO_WAVE_PAN(t)){
      height += t->GetHeight(true);
   }
#endif
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
   link = (*s1)->GetLink();
   bool linked1 = link != nullptr;
   if (linked1 && !(*s1)->GetLinked()) {
      s1 = link->GetNode();
   }

   link = (*s2)->GetLink();
   bool linked2 = link != nullptr;
   if (linked2 && !(*s2)->GetLinked()) {
      s2 = link->GetNode();
   }

   // Safety check...
   if (s1 == s2)
      return;

   // Be sure s1 is the earlier iterator
   if ((*s1)->GetIndex() >= (*s2)->GetIndex()) {
      std::swap(s1, s2);
      std::swap(linked1, linked2);
   }

   // Remove tracks
   value_type save11 = std::move(*s1), save12{};
   s1 = erase(s1);
   if (linked1) {
      wxASSERT(s1 != s2);
      save12 = std::move(*s1), s1 = erase(s1);
   }
   const bool same = (s1 == s2);

   value_type save21 = std::move(*s2), save22{};
   s2 = erase(s2);
   if (linked2)
      save22 = std::move(*s2), s2 = erase(s2);

   if (same)
      // We invalidated s1!
      s1 = s2;

   // Reinsert them
   Track *pTrack;
   if (save22)
      pTrack = save22.get(), pTrack->SetOwner(mSelf, s1 = insert(s1, std::move(save22)));
   pTrack = save21.get(), pTrack->SetOwner(mSelf, s1 = insert(s1, std::move(save21)));

   if (save12)
      pTrack = save12.get(), pTrack->SetOwner(mSelf, s2 = insert(s2, std::move(save12)));
   pTrack = save11.get(), pTrack->SetOwner(mSelf, s2 = insert(s2, std::move(save11)));

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
   return std::find_if(begin(), end(),
      [=](const value_type &track) { return t == track.get(); }
   ) != end();
}

bool TrackList::IsEmpty() const
{
   return empty();
}

int TrackList::GetCount() const
{
   int cnt = 0;

   if (!empty()) {
      cnt = back()->GetIndex() + 1;
   }

   return cnt;
}

TimeTrack *TrackList::GetTimeTrack()
{
   auto iter = std::find_if(begin(), end(),
      [] (const value_type &t) { return t->GetKind() == Track::Time; }
   );
   if (iter == end())
      return nullptr;
   else
      return static_cast<TimeTrack*>(iter->get());
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
   int numMono = 0;
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
            numMono++;
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
   Array GetWaveTracks(ListOfTracks::const_iterator p, ListOfTracks::const_iterator end,
                       bool selectionOnly, bool includeMuted)
   {
      Array waveTrackArray;

      for (; p != end; ++p) {
         const auto &track = *p;
         auto wt = static_cast<const WaveTrack *>(&*track);
         if (track->GetKind() == Track::Wave &&
            (includeMuted || !wt->GetMute()) &&
            (track->GetSelected() || !selectionOnly)) {
            waveTrackArray.push_back(static_cast<WaveTrack*>(track.get()));
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
   return GetWaveTracks<WaveTrackConstArray>(begin(), end(), selectionOnly, includeMuted);
}

#if defined(USE_MIDI)
NoteTrackArray TrackList::GetNoteTrackArray(bool selectionOnly)
{
   NoteTrackArray noteTrackArray;

   for(const auto &track : *this) {
      if (track->GetKind() == Track::Note &&
         (track->GetSelected() || !selectionOnly)) {
         noteTrackArray.push_back(static_cast<NoteTrack*>(track.get()));
      }
   }

   return noteTrackArray;
}
#endif

int TrackList::GetHeight() const
{
   int height = 0;

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   if (!empty()) {
      const Track *t = rbegin()->get();
      if(MONO_WAVE_PAN(t))
         height = t->GetY(true) + t->GetHeight(true);
      else
         height = t->GetY() + t->GetHeight();
   }
#else
   if (!empty()) {
      const auto &track = back();
      height = track->GetY() + track->GetHeight();
   }
#endif
   return height;
}

namespace {
   // Abstract the common pattern of the following three member functions
   double doubleMin(double a, double b) { return std::min(a, b); }
   double doubleMax(double a, double b) { return std::max(a, b); }
   inline double Accumulate
      (const ListOfTracks &list,
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
         [=](double acc, const ListOfTracks::value_type &pTrack) {
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
