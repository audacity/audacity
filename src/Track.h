/**********************************************************************

  Audacity: A Digital Audio Editor

  Track.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_TRACK__
#define __AUDACITY_TRACK__

#include "Audacity.h"

#include "MemoryX.h"
#include <vector>
#include <list>
#include <functional>
#include <wx/event.h>
#include <wx/gdicmn.h>
#include <wx/longlong.h>
#include <wx/string.h>

#include "Experimental.h"
#include "SampleFormat.h"
#include "tracks/ui/CommonTrackPanelCell.h"
#include "xml/XMLTagHandler.h"

#ifdef __WXMSW__
#pragma warning(disable:4284)
#endif

class wxTextFile;
class DirManager;
class Track;
class LabelTrack;
class TimeTrack;
class TrackControls;
class TrackVRulerControls;
class TrackPanelResizerCell;
class WaveTrack;
class NoteTrack;
class AudacityProject;
class ZoomInfo;

class SelectHandle;
class TimeShiftHandle;

WX_DEFINE_USER_EXPORTED_ARRAY(Track*, TrackArray, class AUDACITY_DLL_API);
using WaveTrackArray = std::vector < std::shared_ptr< WaveTrack > > ;
using WaveTrackConstArray = std::vector < std::shared_ptr < const WaveTrack > >;

using NoteTrackConstArray = std::vector < std::shared_ptr< const NoteTrack > >;

#if defined(USE_MIDI)
class NoteTrack;
#endif

class TrackList;

using ListOfTracks = std::list< std::shared_ptr< Track > >;

using TrackNodePointer =
std::pair< ListOfTracks::iterator, ListOfTracks* >;

inline bool operator == (const TrackNodePointer &a, const TrackNodePointer &b)
{ return a.second == b.second && a.first == b.first; }

inline bool operator != (const TrackNodePointer &a, const TrackNodePointer &b)
{ return !(a == b); }

class ViewInfo;

// This is an in-session identifier of track objects across undo states
// It does not persist between sessions
// Default constructed value is not equal to the id of any track that has ever
// been added to a TrackList, or (directly or transitively) copied from such
// (A pending additional track that is not yet applied is not considered added)
// TrackIds are assigned uniquely across projects
class TrackId
{
public:
   TrackId() : mValue(-1) {}
   explicit TrackId (long value) : mValue(value) {}

   bool operator == (const TrackId &other) const
   { return mValue == other.mValue; }

   bool operator != (const TrackId &other) const
   { return mValue != other.mValue; }

   // Define this in case you want to key a std::map on TrackId
   // The operator does not mean anything else
   bool operator <  (const TrackId &other) const
   { return mValue <  other.mValue; }

private:
   long mValue;
};

class AUDACITY_DLL_API Track /* not final */
   : public CommonTrackPanelCell, public XMLTagHandler
{
   friend class TrackList;
   friend class TrackListIterator;
   friend class SyncLockedTracksIterator;

 // To be TrackDisplay
 private:
   TrackId mId;

 protected:
   std::weak_ptr<TrackList> mList;
   TrackNodePointer mNode{};
   int            mIndex;
   int            mY;
   int            mHeight;
   wxString       mName;
   wxString       mDefaultName;

   bool           mSelected;

   bool           mLinked;
   bool           mMinimized;

 public:

   TrackId GetId() const { return mId; }
 private:
   void SetId( TrackId id ) { mId = id; }
 public:

   // Given a bare pointer, find a shared_ptr.  But this is not possible for
   // a track not owned by any project, so the result can be null.
   template<typename Subclass = Track>
   inline static std::shared_ptr<Subclass> Pointer( Track *t )
   {
      if (t) {
         auto pList = t->mList.lock();
         if (pList)
            return std::static_pointer_cast<Subclass>(*t->mNode.first);
      }
      return {};
   }

   template<typename Subclass = const Track>
   inline static std::shared_ptr<Subclass> Pointer( const Track *t )
   {
      if (t) {
         auto pList = t->mList.lock();
         if (pList) {
            std::shared_ptr<const Track> p{ *t->mNode.first };
            // Let you change the type, but not cast away the const
            return std::static_pointer_cast<Subclass>(p);
         }
      }
      return {};
   }

   // Cause certain overriding tool modes (Zoom; future ones?) to behave
   // uniformly in all tracks, disregarding track contents.
   // Do not further override this...
   std::vector<UIHandlePtr> HitTest
      (const TrackPanelMouseState &, const AudacityProject *pProject)
      final override;

 public:

   // Rather override this for subclasses:
   virtual std::vector<UIHandlePtr> DetailedHitTest
      (const TrackPanelMouseState &,
       const AudacityProject *pProject, int currentTool, bool bMultiTool)
      = 0;

   mutable wxSize vrulerSize;

   // Return another, associated TrackPanelCell object that implements the
   // drop-down, close and minimize buttons, etc.
   std::shared_ptr<TrackPanelCell> GetTrackControl();

   // Return another, associated TrackPanelCell object that implements the
   // mouse actions for the vertical ruler
   std::shared_ptr<TrackPanelCell> GetVRulerControl();

   // Return another, associated TrackPanelCell object that implements the
   // click and drag to resize
   std::shared_ptr<TrackPanelCell> GetResizer();

   // This just returns a constant and can be overriden by subclasses
   // to specify a different height for the case that the track is minimized.
   virtual int GetMinimizedHeight() const;
   int GetActualHeight() const { return mHeight; }

   int GetIndex() const;
   void SetIndex(int index);

   int GetY() const;
private:
   // Always maintain a strictly contiguous layout of tracks.
   // So client code is not permitted to modify this attribute directly.
   void SetY(int y);
   // No need yet to make this virtual
   void DoSetY(int y);
public:

   int GetHeight() const;
   void SetHeight(int h);
protected:
   virtual void DoSetHeight(int h);
public:

   bool GetMinimized() const;
   void SetMinimized(bool isMinimized);
protected:
   virtual void DoSetMinimized(bool isMinimized);
public:

   Track *GetLink() const;

 private:
   TrackNodePointer GetNode() const;
   void SetOwner
      (const std::weak_ptr<TrackList> &list, TrackNodePointer node);

 // Keep in Track

 protected:
   int                 mChannel;
   double              mOffset;

   mutable std::shared_ptr<DirManager> mDirManager;

 public:

   enum
   {
      LeftChannel = 0,
      RightChannel = 1,
      MonoChannel = 2
   };

   enum TrackKindEnum
   {
      None,
      Wave,
#if defined(USE_MIDI)
      Note,
#endif
      Label,
      Time,
      All
   };

   enum : unsigned { DefaultHeight = 150 };

   Track(const std::shared_ptr<DirManager> &projDirManager);
   Track(const Track &orig);

   virtual ~ Track();

   void Init(const Track &orig);

   using Holder = std::unique_ptr<Track>;
   virtual Holder Duplicate() const = 0;

   // Called when this track is merged to stereo with another, and should
   // take on some paramaters of its partner.
   virtual void Merge(const Track &orig);

   wxString GetName() const { return mName; }
   void SetName( const wxString &n ) { mName = n; }
   wxString GetDefaultName() const { return mDefaultName; }
   void SetDefaultName( const wxString &n ) { mDefaultName = n; }

   bool GetSelected() const { return mSelected; }
   virtual void SetSelected(bool s);

   bool GetLinked  () const { return mLinked;   }
   void SetLinked  (bool l);
private:
   // No need yet to make this virtual
   void DoSetLinked(bool l);
public:

   virtual int GetChannel() const { return mChannel;};
   virtual double GetOffset() const = 0;

   void Offset(double t) { SetOffset(GetOffset() + t); }
   virtual void SetOffset (double o) { mOffset = o; }

   void SetChannel(int    c) { mChannel = c; }
   virtual void SetPan( float ){ ;}
   virtual void SetPanFromChannelType(){ ;};

   // AS: Note that the dirManager is mutable.  This is
   // mostly to support "Duplicate" of const objects,
   // but in general, mucking with the dir manager is
   // separate from the Track.
   const std::shared_ptr<DirManager> &GetDirManager() const { return mDirManager; }

   // Create a NEW track and modify this track
   // Return non-NULL or else throw
   // May assume precondition: t0 <= t1
   virtual Holder Cut(double WXUNUSED(t0), double WXUNUSED(t1)) = 0;

   // Create a NEW track and don't modify this track
   // Return non-NULL or else throw
   // Note that subclasses may want to distinguish tracks stored in a clipboard
   // from those stored in a project
   // May assume precondition: t0 <= t1
   virtual Holder Copy
      (double WXUNUSED(t0), double WXUNUSED(t1), bool forClipboard = true) const = 0;

   // May assume precondition: t0 <= t1
   virtual void Clear(double WXUNUSED(t0), double WXUNUSED(t1)) = 0;

   virtual void Paste(double WXUNUSED(t), const Track * WXUNUSED(src)) = 0;

   // This can be used to adjust a sync-lock selected track when the selection
   // is replaced by one of a different length.
   virtual void SyncLockAdjust(double oldT1, double newT1);

   // May assume precondition: t0 <= t1
   virtual void Silence(double WXUNUSED(t0), double WXUNUSED(t1)) = 0;

   // May assume precondition: t0 <= t1
   virtual void InsertSilence(double WXUNUSED(t), double WXUNUSED(len)) = 0;

   virtual int GetKind() const { return None; }

   // XMLTagHandler callback methods -- NEW virtual for writing
   virtual void WriteXML(XMLWriter &xmlFile) const = 0;

   // Returns true if an error was encountered while trying to
   // open the track from XML
   virtual bool GetErrorOpening() { return false; }

   virtual double GetStartTime() const = 0;
   virtual double GetEndTime() const = 0;

   // Checks if sync-lock is on and any track in its sync-lock group is selected.
   bool IsSyncLockSelected() const;

protected:
   std::shared_ptr<Track> FindTrack() override;

   // These are called to create controls on demand:
   virtual std::shared_ptr<TrackControls> GetControls() = 0;
   virtual std::shared_ptr<TrackVRulerControls> GetVRulerControls() = 0;

   // These hold the controls:
   std::shared_ptr<TrackControls> mpControls;
   std::shared_ptr<TrackVRulerControls> mpVRulerContols;
   std::shared_ptr<TrackPanelResizerCell> mpResizer;

   std::weak_ptr<SelectHandle> mSelectHandle;
   std::weak_ptr<TimeShiftHandle> mTimeShiftHandle;
};

class AUDACITY_DLL_API AudioTrack /* not final */ : public Track
{
public:
   AudioTrack(const std::shared_ptr<DirManager> &projDirManager)
      : Track{ projDirManager } {}
   AudioTrack(const Track &orig) : Track{ orig } {}

   // Serialize, not with tags of its own, but as attributes within a tag.
   void WriteXMLAttributes(XMLWriter &WXUNUSED(xmlFile)) const {}

   // Return true iff the attribute is recognized.
   bool HandleXMLAttribute(const wxChar * /*attr*/, const wxChar * /*value*/)
   { return false; }
};

class AUDACITY_DLL_API PlayableTrack /* not final */ : public AudioTrack
{
public:
   PlayableTrack(const std::shared_ptr<DirManager> &projDirManager)
      : AudioTrack{ projDirManager } {}
   PlayableTrack(const Track &orig) : AudioTrack{ orig } {}

   bool GetMute    () const { return mMute;     }
   bool GetSolo    () const { return mSolo;     }
   void SetMute    (bool m) { mMute     = m; }
   void SetSolo    (bool s) { mSolo     = s; }

   void Init( const PlayableTrack &init );
   void Merge( const Track &init ) override;

   // Serialize, not with tags of its own, but as attributes within a tag.
   void WriteXMLAttributes(XMLWriter &xmlFile) const;

   // Return true iff the attribute is recognized.
   bool HandleXMLAttribute(const wxChar *attr, const wxChar *value);

protected:
   bool                mMute { false };
   bool                mSolo { false };
};

class AUDACITY_DLL_API TrackListIterator /* not final */
: public std::iterator< std::forward_iterator_tag, Track *const >
{
 public:
   // The default-constructed value can serve as the end iterator for
   // traversal over any track list.
   TrackListIterator() {}
   explicit TrackListIterator(TrackList * val);
   explicit TrackListIterator(TrackList * val, TrackNodePointer p);
   TrackListIterator(const TrackListIterator&) = default;
   TrackListIterator& operator=(const TrackListIterator&) = default;
   virtual ~TrackListIterator() {}

   // Iterate functions
   virtual Track *First(TrackList * val = nullptr);
   virtual Track *StartWith(Track * val);
   virtual Track *Next(bool skiplinked = false);
   virtual Track *Prev(bool skiplinked = false);
   virtual Track *Last(bool skiplinked = false);

   Track *RemoveCurrent(); // deletes track, returns next

   // Provide minimal STL forward-iterator idiom:

   // unlike Next, this is non-mutating.
   // An end iterator may be safely dereferenced, returning nullptr.
   Track *operator * () const;

   TrackListIterator &operator++ () { (void) Next(); return *this; }
   TrackListIterator operator++ (int)
   { auto copy = *this; operator++(); return copy; }

   bool operator == (const TrackListIterator &other) const;
   bool operator != (const TrackListIterator &other) const
   { return !(*this == other); }

 protected:
   friend TrackList;

   TrackList *l {};
   TrackNodePointer cur{};
};

class AUDACITY_DLL_API TrackListConstIterator
: public std::iterator< std::forward_iterator_tag, const Track *const >
{
public:
   // The default-constructed value can serve as the end iterator for
   // traversal over any track list.
   TrackListConstIterator() {}
   explicit TrackListConstIterator(
      const TrackList * val, TrackNodePointer p)
      : mIter(const_cast<TrackList*>(val), p)
   {}
   explicit TrackListConstIterator(
      const TrackList * val)
      : mIter(const_cast<TrackList*>(val))
   {}
   TrackListConstIterator(const TrackListConstIterator&) = default;
   TrackListConstIterator& operator=(const TrackListConstIterator&) = default;
   ~TrackListConstIterator() {}

   // Iterate functions
   const Track *First(const TrackList * val = NULL)
   { return mIter.First(const_cast<TrackList*>(val)); }
   const Track *StartWith(const Track * val)
   { return mIter.StartWith(const_cast<Track*>(val)); }
   const Track *Next(bool skiplinked = false)
   { return mIter.Next(skiplinked); }
   const Track *Prev(bool skiplinked = false)
   { return mIter.Prev(skiplinked); }
   const Track *Last(bool skiplinked = false)
   { return mIter.Last(skiplinked); }

   // Provide minimal STL forward-iterator idiom:

   // unlike Next, this is non-mutating.
   // An end iterator may be safely dereferenced, returning nullptr.
   const Track *operator * () const { return *mIter; }

   TrackListConstIterator &operator++ () { (void) Next(); return *this; }
   TrackListConstIterator operator++ (int)
   { auto copy = *this; operator++(); return copy; }

   bool operator == (const TrackListConstIterator &other) const
   { return mIter == other.mIter; }
   bool operator != (const TrackListConstIterator &other) const
   { return !(*this == other); }

private:
   TrackListIterator mIter;
};

// TrackListCondIterator (base class for iterators that iterate over all tracks)
// that meet a condition)
class AUDACITY_DLL_API TrackListCondIterator /* not final */ : public TrackListIterator
{
   public:
      TrackListCondIterator(TrackList *val = NULL)
         :  TrackListIterator(val) {}
      virtual ~TrackListCondIterator() {}

      // Iteration functions
      Track *First(TrackList *val = NULL) override;
      Track *StartWith(Track *val) override;
      Track *Next(bool skiplinked = false) override;
      Track *Prev(bool skiplinked = false) override;
      Track *Last(bool skiplinked = false) override;

   protected:
      // NEW virtual
      virtual bool Condition(Track *t) = 0;
};

//
// TrackListOfKindIterator
//
// Based on TrackListIterator and returns only tracks of the specified type.
//
class AUDACITY_DLL_API TrackListOfKindIterator /* not final */ : public TrackListCondIterator
{
 public:
   TrackListOfKindIterator(int kind, TrackList * val = NULL);
   virtual ~TrackListOfKindIterator() {}

 protected:
   virtual bool Condition(Track *t) override;

 private:
   int kind;
};

//
// SelectedTrackListOfKindIterator
//
// Based on TrackListOfKindIterator and returns only tracks selected.
//
class AUDACITY_DLL_API SelectedTrackListOfKindIterator final : public TrackListOfKindIterator
{
 public:
    SelectedTrackListOfKindIterator(int kind, TrackList * val = NULL) : TrackListOfKindIterator(kind, val) {}
   virtual ~SelectedTrackListOfKindIterator() {}

 protected:
   bool Condition(Track *t) override;
};

//
// VisibleTrackIterator
//
// Based on TrackListIterator returns only the currently visible tracks.
//
class AUDACITY_DLL_API VisibleTrackIterator final : public TrackListCondIterator
{
 public:
   VisibleTrackIterator(AudacityProject *project);
   virtual ~VisibleTrackIterator() {}

 protected:
   bool Condition(Track *t) override;

 private:
   AudacityProject *mProject;
   wxRect mPanelRect;
};


// SyncLockedTracksIterator returns only tracks belonging to the sync-locked tracks
// in which the starting track is a member.
class AUDACITY_DLL_API SyncLockedTracksIterator final : public TrackListIterator
{
 public:
   SyncLockedTracksIterator(TrackList * val);
   virtual ~SyncLockedTracksIterator() {}

   // Iterate functions
   Track *StartWith(Track *member) override;
   Track *Next(bool skiplinked = false) override;
   Track *Prev(bool skiplinked = false) override;
   Track *Last(bool skiplinked = false) override;

 private:
   bool IsGoodNextTrack(const Track *t) const;
   bool mInLabelSection;
};


/** \brief TrackList is a flat linked list of tracks supporting Add,  Remove,
 * Clear, and Contains, plus serialization of the list of tracks.
 */

struct TrackListEvent : public wxCommandEvent
{
   TrackListEvent(wxEventType commandType = wxEVT_NULL, int winid = 0)
   : wxCommandEvent{ commandType, winid } {}

   TrackListEvent( const TrackListEvent& ) = default;

   wxEvent *Clone() const override { return new TrackListEvent(*this); }

   std::weak_ptr<Track> mpTrack;
};

// Posted when tracks are reordered but otherwise unchanged.
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API,
                         EVT_TRACKLIST_PERMUTED, wxCommandEvent);

// Posted when some track was added or changed its height.
// Cast to TrackListEvent and examine mpTrack to retrieve it.
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API,
                         EVT_TRACKLIST_RESIZING, wxCommandEvent);

// Posted when a track has been deleted from a tracklist.
// Also posted when one track replaces another
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API,
                         EVT_TRACKLIST_DELETION, wxCommandEvent);

class TrackList final : public wxEvtHandler, public ListOfTracks
{
   // privatize this, make you use Swap instead:
   using ListOfTracks::swap;

   // Create an empty TrackList
   TrackList();

   // Disallow copy
   TrackList(const TrackList &that) = delete;
   TrackList &operator= (const TrackList&) = delete;

   // Allow move
   TrackList(TrackList &&that) : TrackList() { Swap(that); }
   TrackList& operator= (TrackList&&);

   void clear() = delete;

 public:
   // Create an empty TrackList
   static std::shared_ptr<TrackList> Create();

   // Move is defined in terms of Swap
   void Swap(TrackList &that);

   // Destructor
   virtual ~TrackList();

   // Hide the inherited begin() and end()
   using iterator = TrackListIterator;
   using const_iterator = TrackListConstIterator;
   using value_type = Track *;
   iterator begin() { return iterator{
      this, { ListOfTracks::begin(), this } }; }
   iterator end() { return {}; }
   const_iterator begin() const { return const_iterator{ this }; }
   const_iterator end() const { return {}; }
   const_iterator cbegin() const { return begin(); }
   const_iterator cend() const { return end(); }

   friend class Track;
   friend class TrackListIterator;
   friend class SyncLockedTracksIterator;

   /// For use in sorting:  assume each iterator points into this list, no duplications
   void Permute(const std::vector<TrackNodePointer> &permutation);

   Track *FindById( TrackId id );

   /// Add a Track, giving it a fresh id
   template<typename TrackKind>
   Track *Add(std::unique_ptr<TrackKind> &&t);

   /// Add a Track, giving it a fresh id
   template<typename TrackKind>
   Track *AddToHead(std::unique_ptr<TrackKind> &&t);

   /// Add a Track, giving it a fresh id
   template<typename TrackKind>
   Track *Add(std::shared_ptr<TrackKind> &&t);

   /// Replace first track with second track, give back a holder
   /// Give the replacement the same id as the replaced
   ListOfTracks::value_type Replace(Track * t, ListOfTracks::value_type &&with);

   /// Remove this Track or all children of this TrackList.
   /// Return an iterator to what followed the removed track.
   TrackNodePointer Remove(Track *t);

   /// Make the list empty
   void Clear(bool sendEvent = true);

   /** Select a track, and if it is linked to another track, select it, too. */
   void Select(Track * t, bool selected = true);

   Track *GetPrev(Track * t, bool linked = false) const;

   /** Return a track in the list that comes after Track t
     * @param t a track in the list
     * @param linked if true, skips over linked tracks, if false returns the next track even if it is a linked track
    **/
   Track *GetNext(Track * t, bool linked = false) const;
   int GetGroupHeight(Track * t) const;

   bool CanMoveUp(Track * t) const;
   bool CanMoveDown(Track * t) const;

   bool MoveUp(Track * t);
   bool MoveDown(Track * t);
   bool Move(Track * t, bool up) { return up ? MoveUp(t) : MoveDown(t); }

   TimeTrack *GetTimeTrack();
   const TimeTrack *GetTimeTrack() const;

   /** \brief Find out how many channels this track list mixes to
   *
   * This is used in exports of the tracks to work out whether to export in
   * Mono, Stereo etc. @param selectionOnly Whether to consider the entire track
   * list or only the selected members of it
   */
   unsigned GetNumExportChannels(bool selectionOnly) const;

   WaveTrackArray GetWaveTrackArray(bool selectionOnly, bool includeMuted = true);
   WaveTrackConstArray GetWaveTrackConstArray(bool selectionOnly, bool includeMuted = true) const;

#if defined(USE_MIDI)
   NoteTrackConstArray GetNoteTrackConstArray(bool selectionOnly) const;
#endif

   /// Mainly a test function. Uses a linear search, so could be slow.
   bool Contains(const Track * t) const;

   // Return non-null only if the weak pointer is not, and the track is
   // owned by this list; constant time.
   template <typename Subclass>
   std::shared_ptr<Subclass> Lock(const std::weak_ptr<Subclass> &wTrack)
   {
      auto pTrack = wTrack.lock();
      if (pTrack) {
         auto pList = pTrack->mList.lock();
         if (pTrack && this == pList.get())
            return pTrack;
      }
      return {};
   }

   bool empty() const;
   size_t size() const;

   double GetStartTime() const;
   double GetEndTime() const;

   double GetMinOffset() const;
   int GetHeight() const;

#if LEGACY_PROJECT_FILE_SUPPORT
   // File I/O
   bool Load(wxTextFile * in, DirManager * dirManager) override;
   bool Save(wxTextFile * out, bool overwrite) override;
#endif

private:
   bool isNull(TrackNodePointer p) const
   { return (p.second == this && p.first == ListOfTracks::end())
      || (p.second == &mPendingUpdates && p.first == mPendingUpdates.end()); }
   TrackNodePointer getEnd() const
   { return { const_cast<TrackList*>(this)->ListOfTracks::end(),
              const_cast<TrackList*>(this)}; }
   TrackNodePointer getBegin() const
   { return { const_cast<TrackList*>(this)->ListOfTracks::begin(),
              const_cast<TrackList*>(this)}; }

   // Move an iterator to the next node, if any; else stay at end
   TrackNodePointer getNext(TrackNodePointer p) const
   {
      if ( isNull(p) )
         return p;
      auto q = p;
      ++q.first;
      return q;
   }

   // Move an iterator to the previous node, if any; else wrap to end
   TrackNodePointer getPrev(TrackNodePointer p) const
   {
      if (p == getBegin())
         return getEnd();
      else {
         auto q = p;
         --q.first;
         return q;
      }
   }

   void RecalcPositions(TrackNodePointer node);
   void PermutationEvent();
   void DeletionEvent();
   void ResizingEvent(TrackNodePointer node);

   void SwapNodes(TrackNodePointer s1, TrackNodePointer s2);

   std::weak_ptr<TrackList> mSelf;

   // Nondecreasing during the session.
   // Nonpersistent.
   // Used to assign ids to added tracks.
   static long sCounter;

public:
   using Updater = std::function< void(Track &dest, const Track &src) >;
   // Start a deferred update of the project.
   // The return value is a duplicate of the given track.
   // While ApplyPendingTracks or ClearPendingTracks is not yet called,
   // there may be other direct changes to the project that push undo history.
   // Meanwhile the returned object can accumulate other changes for a deferred
   // push, and temporarily shadow the actual project track for display purposes.
   // The Updater function, if not null, merges state (from the actual project
   // into the pending track) which is not meant to be overridden by the
   // accumulated pending changes.
   // To keep the display consistent, the Y and Height values, minimized state,
   // and Linked state must be copied, and this will be done even if the
   // Updater does not do it.
   // Pending track will have the same TrackId as the actual.
   // Pending changed tracks will not occur in iterations.
   std::shared_ptr<Track> RegisterPendingChangedTrack(
      Updater updater,
      Track *src
   );

   // Like the previous, but for a NEW track, not a replacement track.  Caller
   // supplies the track, and there are no updates.
   // Pending track will have an unassigned TrackId.
   // Pending changed tracks WILL occur in iterations, always after actual
   // tracks, and in the sequence that they were added.  They can be
   // distinguished from actual tracks by TrackId.
   void RegisterPendingNewTrack( const std::shared_ptr<Track> &pTrack );

   // Invoke the updaters of pending tracks.  Pass any exceptions from the
   // updater functions.
   void UpdatePendingTracks();

   // Forget pending track additions and changes;
   // if requested, give back the pending added tracks.
   void ClearPendingTracks( ListOfTracks *pAdded = nullptr );

   // Change the state of the project.
   // Strong guarantee for project state in case of exceptions.
   // Will always clear the pending updates.
   // Return true if the state of the track list really did change.
   bool ApplyPendingTracks();

   // Find anything registered with RegisterPendingChangedTrack and not yet
   // cleared or applied
   std::shared_ptr<Track> FindPendingChangedTrack(TrackId id) const;

   bool HasPendingTracks() const;

private:
   // Need to put pending tracks into a list so that GetLink() works
   ListOfTracks mPendingUpdates;
   // This is in correspondence with mPendingUpdates
   std::vector< Updater > mUpdaters;
};

class AUDACITY_DLL_API TrackFactory
{
 private:
   TrackFactory(const std::shared_ptr<DirManager> &dirManager, const ZoomInfo *zoomInfo):
      mDirManager(dirManager)
      , mZoomInfo(zoomInfo)
   {
   }

   const std::shared_ptr<DirManager> mDirManager;
   const ZoomInfo *const mZoomInfo;
   friend class AudacityProject;
   friend class BenchmarkDialog;

 public:
   // These methods are defined in WaveTrack.cpp, NoteTrack.cpp,
   // LabelTrack.cpp, and TimeTrack.cpp respectively
   std::unique_ptr<WaveTrack> DuplicateWaveTrack(const WaveTrack &orig);
   std::unique_ptr<WaveTrack> NewWaveTrack(sampleFormat format = (sampleFormat)0,
                           double rate = 0);
   std::unique_ptr<LabelTrack> NewLabelTrack();
   std::unique_ptr<TimeTrack> NewTimeTrack();
#if defined(USE_MIDI)
   std::unique_ptr<NoteTrack> NewNoteTrack();
#endif
};

// global functions
struct TransportTracks;
TransportTracks GetAllPlaybackTracks(const TrackList &trackList, bool selectedOnly, bool useMidi = false);

#endif
