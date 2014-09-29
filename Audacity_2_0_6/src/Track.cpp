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

#include <float.h>
#include <wx/file.h>
#include <wx/textfile.h>
#include <wx/log.h>

#include "Track.h"
#include "WaveTrack.h"
#include "NoteTrack.h"
#include "LabelTrack.h"
#include "Project.h"
#include "TimeTrack.h"
#include "DirManager.h"

#ifdef _MSC_VER
//Disable truncation warnings
#pragma warning( disable : 4786 )
#endif

#ifdef __WXDEBUG__
   // if we are in a debug build of audacity
   /// Define this to do extended (slow) debuging of TrackListIterator
//   #define DEBUG_TLI
#endif

Track::Track(DirManager * projDirManager)
:  vrulerSize(36,0),
   mDirManager(projDirManager)
{
   mDirManager->Ref();

   mList      = NULL;
   mNode      = NULL;
   mSelected  = false;
   mLinked    = false;
   mMute      = false;
   mSolo      = false;

   mY = 0;
   mHeight = 150;
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
{
   mDirManager = NULL;

   mList = NULL;
   mNode = NULL;
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

   if (mDirManager != orig.mDirManager) {
      if (mDirManager) {
         mDirManager->Deref(); // MM: unreference old DirManager
      }

      // MM: Assign and ref new DirManager
      mDirManager = orig.mDirManager;
      mDirManager->Ref();
   }

   mSelected = orig.mSelected;
   mLinked = orig.mLinked;
   mMute = orig.mMute;
   mSolo = orig.mSolo;
   mHeight = orig.mHeight;
   mMinimized = orig.mMinimized;
   mChannel = orig.mChannel;
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   mVirtualStereo = orig.mVirtualStereo;
   mHeightv = orig.mHeightv;
   mYv = orig.mYv;
#endif
}

void Track::Merge(const Track &orig)
{
   mSelected = orig.mSelected;
   mMute = orig.mMute;
   mSolo = orig.mSolo;
}

Track::~Track()
{
   mDirManager->Deref();
}


const TrackListNode *Track::GetNode()
{
   return mNode;
}

// A track can only live on one list at a time, so if you're moving a
// track from one list to another, you must call SetOwner() with NULL
// pointers first and then with the real pointers.
void Track::SetOwner(TrackList *list, TrackListNode *node)
{
   // Try to detect offenders while in development.
   wxASSERT(list == NULL || mList == NULL);
   wxASSERT(node == NULL || mNode == NULL);
   wxASSERT((list != NULL && node != NULL) || (list == NULL && node == NULL));

   mList = list;
   mNode = node;
}

int Track::GetMinimizedHeight() const
{
   if (GetLink()) {
      return 20;
   }

   return 40;
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

   mList->RecalcPositions(mNode);
   mList->ResizedEvent(mNode);
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
   mList->RecalcPositions(mNode);
   mList->ResizedEvent(mNode);
}
#endif // EXPERIMENTAL_OUTPUT_DISPLAY

bool Track::GetMinimized() const
{
   return mMinimized;
}

void Track::SetMinimized(bool isMinimized)
{
   mMinimized = isMinimized;
   mList->RecalcPositions(mNode);
   mList->ResizedEvent(mNode);
}

void Track::SetLinked(bool l)
{
   mLinked = l;
   mList->RecalcPositions(mNode);
   mList->ResizedEvent(mNode);
}

Track *Track::GetLink() const
{
   if (mNode) {
      if (mNode->next && mLinked) {
         return mNode->next->t;
      }

      if (mNode->prev && mNode->prev->t->GetLinked()) {
         return mNode->prev->t;
      }
   }

   return NULL;
}

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
void Track::ReorderList(bool resize)
{
    mList->RecalcPositions(mNode);
    if(resize)
      mList->ResizedEvent(mNode);
}
#endif

bool Track::IsSyncLockSelected()
{
#ifdef EXPERIMENTAL_SYNC_LOCK
   AudacityProject *p = GetActiveProject();
   if (!p || !p->IsSyncLocked())
      return false;

   SyncLockedTracksIterator git(mList);
   Track *t = git.First(this);

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

bool Track::SyncLockAdjust(double oldT1, double newT1)
{
   if (newT1 > oldT1) {
      // Insert space within the track

      if (oldT1 > GetEndTime())
         return true;

      Track *tmp;
      bool ret;

      ret = Cut(oldT1, GetEndTime(), &tmp);
      if (!ret) return false;

      ret = Paste(newT1, tmp);
      wxASSERT(ret);

      delete tmp;
      return ret;
   }
   else if (newT1 < oldT1) {
      // Remove from the track
      return Clear(newT1, oldT1);
   }

   // fall-through: no change
   return true;
}

// TrackListIterator
TrackListIterator::TrackListIterator(TrackList * val)
{
   l = val;
   cur = NULL;
}

Track *TrackListIterator::StartWith(Track * val)
{
   if (val == NULL) {
      return First();
   }

   if (l == NULL) {
      return NULL;
   }

   cur = (TrackListNode *) val->GetNode();

   if (cur) {
      return cur->t;
   }

   return NULL;
}

Track *TrackListIterator::First(TrackList * val)
{
   if (val != NULL) {
      l = val;
   }

   if (l == NULL) {
      return NULL;
   }

   cur = l->head;

   if (cur) {
      return cur->t;
   }

   return NULL;
}

Track *TrackListIterator::Last(bool skiplinked)
{
   if (l == NULL) {
      return NULL;
   }

   cur = l->tail;

   // With skiplinked set, we won't return the second channel of a linked pair
   if (skiplinked && cur && cur->t && cur->t->GetLink() && !cur->t->GetLinked())
   {
      cur = cur->prev;
   }

   if (cur) {
      return cur->t;
   }

   return NULL;
}

Track *TrackListIterator::Next(bool skipLinked)
{
   #ifdef DEBUG_TLI // if we are debugging this bit
   wxASSERT_MSG((!cur || (*l).Contains((*cur).t)), wxT("cur invalid at start of Next(). List changed since iterator created?"));   // check that cur is in the list
   #endif

   if (skipLinked && cur && cur->t->GetLinked()) {
      cur = cur->next;
   }

   #ifdef DEBUG_TLI // if we are debugging this bit
   wxASSERT_MSG((!cur || (*l).Contains((*cur).t)), wxT("cur invalid after skipping linked tracks."));   // check that cur is in the list
   #endif

   if (cur) {
      cur = cur->next;
   }

   #ifdef DEBUG_TLI // if we are debugging this bit
   wxASSERT_MSG((!cur || (*l).Contains((*cur).t)), wxT("cur invalid after moving to next track."));   // check that cur is in the list if it is not null
   #endif

   if (cur) {
      return cur->t;
   }

   return NULL;
}

Track *TrackListIterator::Prev(bool skiplinked)
{
   if (cur)
      cur = cur->prev;

   if (skiplinked && cur && cur->prev && cur->prev->t->GetLinked())
      cur = cur->prev;

   return cur ? cur->t : NULL;
}

Track *TrackListIterator::RemoveCurrent(bool deletetrack)
{
   TrackListNode *next = cur->next;

   l->Remove(cur->t, deletetrack);

   cur = next;

   #ifdef DEBUG_TLI // if we are debugging this bit
   wxASSERT_MSG((!cur || (*l).Contains((*cur).t)), wxT("cur invalid after deletion of track."));   // check that cur is in the list
   #endif

   if (cur) {
      return cur->t;
   }

   return NULL;
}

Track *TrackListIterator::ReplaceCurrent(Track *t)
{
   Track *p = NULL;

   if (cur) {
      p = cur->t;
      p->SetOwner(NULL, NULL);

      cur->t = t;
      t->SetOwner(l, cur);
      l->RecalcPositions(cur);
      l->UpdatedEvent(cur);
      l->ResizedEvent(cur);
   }

   return p;
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
   return r.Intersects(mPanelRect);
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

Track *SyncLockedTracksIterator::First(Track * member)
{
   Track *t = NULL;

   // A sync-locked group consists of any positive number of wave tracks followed by any
   // non-negative number of label tracks. Step back through any label tracks,
   // and then through the wave tracks above them.

   while (member && member->GetKind() == Track::Label) {
      member = l->GetPrev(member);
   }

   while (member && (member->GetKind() == Track::Wave
#ifdef USE_MIDI
                  || member->GetKind() == Track::Note
#endif
                    )) {
      t = member;
      member = l->GetPrev(member);
   }

   // Make it current (if t is still NULL there are no wave tracks, so we're
   // not in a sync-locked group).
   if (t)
      cur = (TrackListNode *) t->GetNode();

   mInLabelSection = false;

   return t;
}

Track *SyncLockedTracksIterator::Next(bool skiplinked)
{
   Track *t = TrackListIterator::Next(skiplinked);

   //
   // Ways to end a sync-locked group
   //

   // End of tracks
   if (!t)
      return NULL;

   // In the label section, encounter a non-label track
   if (mInLabelSection && t->GetKind() != Track::Label) {
      cur = NULL;
      return NULL;
   }
// This code block stops a group when a NoteTrack is encountered
#ifndef USE_MIDI
   // Encounter a non-wave non-label track
   if (t->GetKind() != Track::Wave && t->GetKind() != Track::Label) {
      cur = NULL;
      return NULL;
   }
#endif
   // Otherwise, check if we're in the label section
   mInLabelSection = (t->GetKind() == Track::Label);

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

   // In wave section, encounter a label track
   if (!mInLabelSection && t->GetKind() == Track::Label) {
      cur = NULL;
      return NULL;
   }
#ifndef USE_MIDI
   // Encounter a non-wave non-label track
   if (t->GetKind() != Track::Wave && t->GetKind() != Track::Label) {
      cur = NULL;
      return NULL;
   }
#endif
   // Otherwise, check if we're in the label section
   mInLabelSection = (t->GetKind() == Track::Label);

   return t;
}

Track *SyncLockedTracksIterator::Last(bool skiplinked)
{
   if (!cur)
      return NULL;

   Track *t = cur->t;

   while (l->GetNext(t)) {
      // Check if this is the last track in the sync-locked group.
      int nextKind = l->GetNext(t)->GetKind();
      if (mInLabelSection && nextKind != Track::Label)
         break;
      if (nextKind != Track::Label && nextKind != Track::Wave
#ifdef USE_MIDI
          && nextKind != Track::Note
#endif
          )
         break;

      t = Next(skiplinked);
   }

   return t;
}


// TrackList
//
// The TrackList sends itself events whenever an update occurs to the list it
// is managing.  Any other classes that may be interested in get these updates
// should use TrackList::Connect() and TrackList::Disconnect().
//
DEFINE_EVENT_TYPE(EVT_TRACKLIST_RESIZED);
DEFINE_EVENT_TYPE(EVT_TRACKLIST_UPDATED);

TrackList::TrackList()
:  wxEvtHandler()
{
   head = NULL;
   tail = NULL;
}

TrackList::~TrackList()
{
   Clear();
}

void TrackList::RecalcPositions(const TrackListNode *node)
{
   Track *t;
   int i = 0;
   int y = 0;

   if (!node) {
      return;
   }

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   int cnt = 0;
   if (node->prev) {
      t = node->prev->t;
      i = t->GetIndex() + 1;
      if(MONO_WAVE_PAN(t))
         y = t->GetY(true) + t->GetHeight(true);
      else
         y = t->GetY() + t->GetHeight();
   }

   for (const TrackListNode *n = node; n; n = n->next) {
      t = n->t;
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
   if (node->prev) {
      t = node->prev->t;
      i = t->GetIndex() + 1;
      y = t->GetY() + t->GetHeight();
   }

   for (const TrackListNode *n = node; n; n = n->next) {
      t = n->t;
      t->SetIndex(i++);
      t->SetY(y);
      y += t->GetHeight();
   }
#endif // EXPERIMENTAL_OUTPUT_DISPLAY
}

void TrackList::UpdatedEvent(const TrackListNode *node)
{
   wxCommandEvent e(EVT_TRACKLIST_UPDATED);
   if (node) {
      e.SetClientData(node->t);
   }
   else {
      e.SetClientData(NULL);
   }
   ProcessEvent(e);
}

void TrackList::ResizedEvent(const TrackListNode *node)
{
   if (node) {
      wxCommandEvent e(EVT_TRACKLIST_RESIZED);
      e.SetClientData(node->t);
      ProcessEvent(e);
   }
}

void TrackList::Add(Track * t)
{
   TrackListNode *n = new TrackListNode;
   t->SetOwner(this, n);

   n->t = (Track *) t;
   n->prev = tail;
   n->next = NULL;

   if (tail) {
      tail->next = n;
   }
   tail = n;

   if (!head) {
      head = n;
   }

   RecalcPositions(n);
   UpdatedEvent(n);
}

void TrackList::AddToHead(Track * t)
{
   TrackListNode *n = new TrackListNode;
   t->SetOwner(this, n);

   n->t = (Track *) t;
   n->prev = NULL;
   n->next = head;

   if (head) {
      head->prev = n;
   }
   head = n;

   if (!tail) {
      tail = n;
   }

   RecalcPositions(head);
   UpdatedEvent(n);
   ResizedEvent(n);
}

void TrackList::Replace(Track * t, Track * with, bool deletetrack)
{
   if (t && with) {
      TrackListNode *node = (TrackListNode *) t->GetNode();

      t->SetOwner(NULL, NULL);
      if (deletetrack) {
         delete t;
      }

      node->t = with;
      with->SetOwner(this, node);
      RecalcPositions(node);
      UpdatedEvent(node);
      ResizedEvent(node);
   }
}

void TrackList::Remove(Track * t, bool deletetrack)
{
   if (t) {
      TrackListNode *node = (TrackListNode *) t->GetNode();

      t->SetOwner(NULL, NULL);
      if (deletetrack) {
         delete t;
      }

      if (node) {
         if (node->prev) {
            node->prev->next = node->next;
         }
         else {
            head = node->next;
         }

         if (node->next) {
            node->next->prev = node->prev;
            RecalcPositions(node->next);
         }
         else {
            tail = node->prev;
         }

         UpdatedEvent(NULL);
         ResizedEvent(node->next);

         delete node;
      }
   }
}

void TrackList::Clear(bool deleteTracks /* = false */)
{
   while (head) {
      TrackListNode *temp = head;

      head->t->SetOwner(NULL, NULL);
      if (deleteTracks) {
         delete head->t;
      }

      head = head->next;
      delete temp;
   }
   tail = NULL;

   UpdatedEvent(NULL);
}

void TrackList::Select(Track * t, bool selected /* = true */ )
{
   if (t) {
      const TrackListNode *node = t->GetNode();
      if (node) {
         t->SetSelected(selected);
         if (t->GetLinked() && node->next) {
            node->next->t->SetSelected(selected);
         }
         else if (node->prev && node->prev->t->GetLinked()) {
            node->prev->t->SetSelected(selected);
         }
      }
   }
}

Track *TrackList::GetLink(Track * t) const
{
   if (t) {
      return t->GetLink();
   }

   return NULL;
}

/// Return a track in the list that comes after Track t
Track *TrackList::GetNext(Track * t, bool linked) const
{
   if (t) {
      const TrackListNode *node = t->GetNode();
      if (node) {
         if (linked && t->GetLinked()) {
            node = node->next;
         }

         if (node) {
            node = node->next;
         }

         if (node) {
            return node->t;
         }
      }
   }

   return NULL;
}

Track *TrackList::GetPrev(Track * t, bool linked) const
{
   if (t) {
      const TrackListNode *node = t->GetNode();
      if (node) {
         if (linked) {
            // Input track second in team?
            if (!t->GetLinked() && t->GetLink()) {
               // Make it the first
               node = node->prev;
            }

            // Get the previous node
            node = node->prev;

            // Bump back to start of team
            if (node && node->t->GetLink()) {
               node = node->prev;
            }
         }
         else {
            node = node->prev;
         }

         if (node) {
            return node->t;
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

// Precondition: if either of s1 or s2 are "linked", then
// s1 and s2 must each be the FIRST node of the linked pair.
//
// This is used when you want to swap the track or pair of
// tracks in s1 with the track or pair of tracks in s2.
// The complication is that the tracks are stored in a single
// linked list, and pairs of tracks are marked only by a flag
// in one of the tracks.
void TrackList::Swap(TrackListNode * s1, TrackListNode * s2)
{
   Track *link;
   Track *source[4];
   TrackListNode *target[4];

   // if a null pointer is passed in, we want to know about it
   wxASSERT(s1);
   wxASSERT(s2);

   // Deal with firat track in each team
   link = s1->t->GetLink();
   if (!s1->t->GetLinked() && link) {
      s1 = (TrackListNode *) link->GetNode();
   }

   link = s2->t->GetLink();
   if (!s2->t->GetLinked() && link) {
      s2 = (TrackListNode *) link->GetNode();
   }

   target[0] = s1;
   source[0] = target[0]->t;
   if (source[0]->GetLinked()) {
      target[1] = target[0]->next;
      source[1] = target[1]->t;
   }
   else {
      target[1] = NULL;
      source[1] = NULL;
   }

   target[2] = s2;
   source[2] = target[2]->t;
   if (source[2]->GetLinked()) {
      target[3] = target[2]->next;
      source[3] = target[3]->t;
   }
   else {
      target[3] = NULL;
      source[3] = NULL;
   }

   int s = 2;
   for (int t = 0; t < 4; t++) {
      if (target[t]) {
         target[t]->t = source[s];
         target[t]->t->SetOwner(NULL, NULL);
         target[t]->t->SetOwner(this, target[t]);

         s = (s + 1) % 4;
         if (!source[s]) {
            s = (s + 1) % 4;
         }
      }
   }

   RecalcPositions(s1);
   UpdatedEvent(s1);
   ResizedEvent(s1);
}

bool TrackList::MoveUp(Track * t)
{
   if (t) {
      Track *p = GetPrev(t, true);
      if (p) {
         Swap((TrackListNode *)p->GetNode(), (TrackListNode *)t->GetNode());
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
         Swap((TrackListNode *)t->GetNode(), (TrackListNode *)n->GetNode());
         return true;
      }
   }

   return false;
}

bool TrackList::Contains(Track * t) const
{
   TrackListNode *p = head;
   while (p) {
      if (p->t == t) {
         return true;
      }
      p = p->next;
   }
   return false;
}

bool TrackList::IsEmpty() const
{
   return (head == NULL);
}

int TrackList::GetCount() const
{
   int cnt = 0;

   if (tail) {
      cnt = tail->t->GetIndex() + 1;
   }

   return cnt;
}

TimeTrack *TrackList::GetTimeTrack()
{
   TrackListNode *p = head;
   while (p) {
      if (p->t->GetKind() == Track::Time) {
         return (TimeTrack *)p->t;
      }
      p = p->next;
   }
   return NULL;
}

int TrackList::GetNumExportChannels(bool selectionOnly)
{
   /* counters for tracks panned different places */
   int numLeft = 0;
   int numRight = 0;
   int numMono = 0;
   /* track iteration kit */
   Track *tr;
   TrackListIterator iter;

   for (tr = iter.First(this); tr != NULL; tr = iter.Next()) {

      // Want only unmuted wave tracks.
      if ((tr->GetKind() != Track::Wave) || tr->GetMute())
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

void TrackList::GetWaveTracks(bool selectionOnly,
                              int *num, WaveTrack ***tracks)
{
   int i;
   *num = 0;

   TrackListNode *p = head;
   while (p) {
      if (p->t->GetKind() == Track::Wave && !(p->t->GetMute()) &&
          (p->t->GetSelected() || !selectionOnly)) {
         (*num)++;
      }
      p = p->next;
   }

   *tracks = new WaveTrack*[*num];
   p = head;
   i = 0;
   while (p) {
      if (p->t->GetKind() == Track::Wave && !(p->t->GetMute()) &&
          (p->t->GetSelected() || !selectionOnly)) {
         (*tracks)[i++] = (WaveTrack *)p->t;
      }
      p = p->next;
   }
}

WaveTrackArray TrackList::GetWaveTrackArray(bool selectionOnly)
{
   WaveTrackArray waveTrackArray;

   TrackListNode *p = head;
   while (p) {
      if (p->t->GetKind() == Track::Wave &&
          (p->t->GetSelected() || !selectionOnly)) {
         waveTrackArray.Add((WaveTrack*)p->t);
      }

      p = p->next;
   }

   return waveTrackArray;
}

#if defined(USE_MIDI)
NoteTrackArray TrackList::GetNoteTrackArray(bool selectionOnly)
{
   NoteTrackArray noteTrackArray;

   TrackListNode *p = head;
   while (p) {
      if (p->t->GetKind() == Track::Note &&
         (p->t->GetSelected() || !selectionOnly)) {
         noteTrackArray.Add((NoteTrack*)p->t);
      }

      p = p->next;
   }

   return noteTrackArray;
}
#endif

int TrackList::GetHeight() const
{
   int height = 0;

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   if (tail) {
      const Track *t = tail->t;
      if(MONO_WAVE_PAN(t))
         height = t->GetY(true) + t->GetHeight(true);
      else
         height = t->GetY() + t->GetHeight();
   }
#else
   if (tail) {
      const Track *t = tail->t;
      height = t->GetY() + t->GetHeight();
   }
#endif
   return height;
}

double TrackList::GetMinOffset() const
{
   const TrackListNode *node = head;
   if (!node) {
      return 0.0;
   }

   double len = node->t->GetOffset();

   while ((node = node->next)) {
      double l = node->t->GetOffset();
      if (l < len) {
         len = l;
      }
   }

   return len;
}

double TrackList::GetStartTime() const
{
   const TrackListNode *node = head;
   if (!node) {
      return 0.0;
   }

   double min = node->t->GetStartTime();

   while ((node = node->next)) {
      double l = node->t->GetStartTime();
      if (l < min) {
         min = l;
      }
   }

   return min;
}

double TrackList::GetEndTime() const
{
   const TrackListNode *node = head;
   if (!node) {
      return 0.0;
   }

   double max = node->t->GetEndTime();

   while ((node = node->next)) {
      double l = node->t->GetEndTime();
      if (l > max) {
         max = l;
      }
   }

   return max;
}

