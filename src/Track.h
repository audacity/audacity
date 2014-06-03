/**********************************************************************

  Audacity: A Digital Audio Editor

  Track.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_TRACK__
#define __AUDACITY_TRACK__

#include "Audacity.h"

#include <wx/dynarray.h>
#include <wx/event.h>
#include <wx/gdicmn.h>
#include <wx/longlong.h>
#include <wx/string.h>

#include "Experimental.h"
#include "SampleFormat.h"
#include "xml/XMLTagHandler.h"

#ifdef __WXMSW__
#pragma warning(disable:4284)
#endif

class wxTextFile;
class DirManager;
class UndoStack;
class LabelTrack;
class TimeTrack;
class WaveTrack;
class AudacityProject;

WX_DEFINE_USER_EXPORTED_ARRAY(WaveTrack*, WaveTrackArray, class AUDACITY_DLL_API);

#if defined(USE_MIDI)
class NoteTrack;
WX_DEFINE_USER_EXPORTED_ARRAY(NoteTrack*, NoteTrackArray, class AUDACITY_DLL_API);
#endif

class TrackList;
struct TrackListNode;

class AUDACITY_DLL_API Track: public XMLTagHandler
{

 // To be TrackDisplay
 protected:
   TrackList     *mList;
   TrackListNode *mNode;
   int            mIndex;
   int            mY;
   int            mHeight;
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   int            mYv;   //For mono a virtual Y value is necessary.
   int            mHeightv; // For mono a virtual height value is necessary.
   float          mPerY; //mY as a percent of mYv + mY
   bool           mVirtualStereo;
#endif
   wxString       mName;
   wxString       mDefaultName;

   bool           mSelected;

   bool           mLinked;
   bool           mMinimized;

 public:
   wxSize vrulerSize;

   // This just returns a constant and can be overriden by subclasses
   // to specify a different height for the case that the track is minimized.
   virtual int GetMinimizedHeight() const;
   int GetActualHeight() { return mHeight; };

   int GetIndex() const;
   void SetIndex(int index);
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   int GetY(bool vStereo = false) const;
   void SetY(int y, bool vStereo = false);
   int GetHeight(bool vStereo = false) const;
   void SetHeight(int h, bool vStereo = false);
#else
   int GetY() const;
   void SetY(int y);
   int GetHeight() const;
   void SetHeight(int h);
#endif
   bool GetMinimized() const;
   void SetMinimized(bool isMinimized);
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   float GetVirtualTrackPercentage() { return mPerY;}
   void SetVirtualTrackPercentage(float val) { mPerY = val;}
   bool GetVirtualStereo() { return mVirtualStereo;}
   void SetVirtualStereo(bool vStereo) { mVirtualStereo = vStereo;}
#endif
   Track *GetLink() const;

   const TrackListNode *GetNode();
   void SetOwner(TrackList *list, TrackListNode *node);

 // Keep in Track

 protected:
   int                 mChannel;
   double              mOffset;
   bool                mMute;
   bool                mSolo;

   mutable DirManager *mDirManager;

 public:
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   void ReorderList(bool resize = true);
#endif

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

   Track(DirManager * projDirManager);
   Track(const Track &orig);

   virtual ~ Track();

   void Init(const Track &orig);
   virtual Track *Duplicate() = 0;

   // Called when this track is merged to stereo with another, and should
   // take on some paramaters of its partner.
   virtual void Merge(const Track &orig);

   wxString GetName() const { return mName; }
   void SetName( wxString n ) { mName = n; }
   wxString GetDefaultName() const { return mDefaultName; }
   void SetDefaultName( wxString n ) { mDefaultName = n; }

   bool GetSelected() const { return mSelected; }
   bool GetMute    () const { return mMute;     }
   bool GetLinked  () const { return mLinked;   }
   bool GetSolo    () const { return mSolo;     }

   void SetSelected(bool s) { mSelected = s; }
   void SetMute    (bool m) { mMute     = m; }
   void SetLinked  (bool l);
   void SetSolo    (bool s) { mSolo     = s; }

   int    GetChannel() const { return mChannel; }
   virtual double GetOffset () { return mOffset; }

   void Offset(double t) { SetOffset(GetOffset() + t); }
   virtual void SetOffset (double o) { mOffset = o; }

   void SetChannel(int    c) { mChannel = c; }

   // AS: Note that the dirManager is mutable.  This is
   // mostly to support "Duplicate" of const objects,
   // but in general, mucking with the dir manager is
   // separate from the Track.
   DirManager* GetDirManager() const { return mDirManager; }

   virtual bool Cut  (double WXUNUSED(t0), double WXUNUSED(t1), Track ** WXUNUSED(dest)) {return false;}
   virtual bool Copy (double WXUNUSED(t0), double WXUNUSED(t1), Track ** WXUNUSED(dest)) {return false;}
   virtual bool Clear(double WXUNUSED(t0), double WXUNUSED(t1)) {return false;}
   virtual bool Paste(double WXUNUSED(t), Track * WXUNUSED(src)) {return false;}

   // This can be used to adjust a sync-lock selected track when the selection
   // is replaced by one of a different length.
   virtual bool SyncLockAdjust(double oldT1, double newT1);

   virtual bool Silence(double WXUNUSED(t0), double WXUNUSED(t1)) {return false;}
   virtual bool InsertSilence(double WXUNUSED(t), double WXUNUSED(len)) {return false;}

   virtual int GetKind() const { return None; }

   // XMLTagHandler callback methods

   virtual bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) = 0;
   virtual XMLTagHandler *HandleXMLChild(const wxChar *tag) = 0;
   virtual void WriteXML(XMLWriter &xmlFile) = 0;

   // Returns true if an error was encountered while trying to
   // open the track from XML
   virtual bool GetErrorOpening() { return false; }

   virtual double GetStartTime() { return 0.0; }
   virtual double GetEndTime() { return 0.0; }

   // Checks if sync-lock is on and any track in its sync-lock group is selected.
   bool IsSyncLockSelected();
};

struct TrackListNode
{
   Track *t;
   TrackListNode *next;
   TrackListNode *prev;
};

class AUDACITY_DLL_API TrackListIterator
{
 public:
   TrackListIterator(TrackList * val = NULL);
   virtual ~TrackListIterator() {};

   // Iterate functions
   virtual Track *First(TrackList * val = NULL);
   virtual Track *StartWith(Track * val);
   virtual Track *Next(bool skiplinked = false);
   virtual Track *Prev(bool skiplinked = false);
   virtual Track *Last(bool skiplinked = false);

   Track *ReplaceCurrent(Track *t);                // returns original
   Track *RemoveCurrent(bool deletetrack = false); // returns next

 protected:
   TrackList *l;
   TrackListNode *cur;
};

// TrackListCondIterator (base class for iterators that iterate over all tracks)
// that meet a condition)
class AUDACITY_DLL_API TrackListCondIterator: public TrackListIterator
{
   public:
      TrackListCondIterator(TrackList *val = NULL)
         :  TrackListIterator(val) {};
      virtual ~TrackListCondIterator() {};

      // Iteration functions
      Track *First(TrackList *val = NULL);
      Track *StartWith(Track *val);
      Track *Next(bool skiplinked = false);
      Track *Prev(bool skiplinked = false);
      Track *Last(bool skiplinked = false);

   protected:
      virtual bool Condition(Track *t) = 0;
};

//
// TrackListOfKindIterator
//
// Based on TrackListIterator and returns only tracks of the specified type.
//
class AUDACITY_DLL_API TrackListOfKindIterator: public TrackListCondIterator
{
 public:
   TrackListOfKindIterator(int kind, TrackList * val = NULL);
   virtual ~TrackListOfKindIterator() {};

 protected:
   virtual bool Condition(Track *t);

 private:
   int kind;
};

//
// SelectedTrackListOfKindIterator
//
// Based on TrackListOfKindIterator and returns only tracks selected.
//
class AUDACITY_DLL_API SelectedTrackListOfKindIterator: public TrackListOfKindIterator
{
 public:
    SelectedTrackListOfKindIterator(int kind, TrackList * val = NULL) : TrackListOfKindIterator(kind, val) {};
   virtual ~SelectedTrackListOfKindIterator() {};

 protected:
   bool Condition(Track *t);
};

//
// VisibleTrackIterator
//
// Based on TrackListIterator returns only the currently visible tracks.
//
class AUDACITY_DLL_API VisibleTrackIterator: public TrackListCondIterator
{
 public:
   VisibleTrackIterator(AudacityProject *project);
   virtual ~VisibleTrackIterator() {};

 protected:
   bool Condition(Track *t);

 private:
   AudacityProject *mProject;
   wxRect mPanelRect;
};


// SyncLockedTracksIterator returns only tracks belonging to the sync-locked tracks
// in which the starting track is a member.
class AUDACITY_DLL_API SyncLockedTracksIterator : public TrackListIterator
{
 public:
   SyncLockedTracksIterator(TrackList * val);
   virtual ~SyncLockedTracksIterator() {};

   // Iterate functions
   Track *First(Track *member);
   Track *Next(bool skiplinked = false);
   Track *Prev(bool skiplinked = false);
   Track *Last(bool skiplinked = false);

 private:
   bool mInLabelSection;
};


/** \brief TrackList is a flat linked list of tracks supporting Add,  Remove,
 * Clear, and Contains, plus serialization of the list of tracks.
 */

// Posted when the horizontal positions within tracks have beed updated.  The
// wxCommandEvent::GetClientData() method can be used to retrieve the first
// track that was updated.  All positions following that track will have been
// updated as well.
DECLARE_EXPORTED_EVENT_TYPE(AUDACITY_DLL_API, EVT_TRACKLIST_RESIZED, -1);

// Posted when tracks have been added or deleted from a tracklist.  The pointer
// wxCommandEvent::GetClientData() returns will be NULL for deletions or the
// track that was added.
DECLARE_EXPORTED_EVENT_TYPE(AUDACITY_DLL_API, EVT_TRACKLIST_UPDATED, -1);

class AUDACITY_DLL_API TrackList:public wxEvtHandler
{
 public:
   // Create an empty TrackList
   TrackList();

   // Destructor
   virtual ~TrackList();

   friend class Track;
   friend class TrackListIterator;

   /// Add this Track or all children of this TrackList.
   void Add(Track * t);
   void AddToHead(Track * t);

   /// Replace first track with second track
   void Replace(Track * t, Track * with, bool deletetrack = false);

   /// Remove this Track or all children of this TrackList.
   void Remove(Track * t, bool deletetrack = false);

   /// Make the list empty
   void Clear(bool deleteTracks = false);

   /** Select a track, and if it is linked to another track, select it, too. */
   void Select(Track * t, bool selected = true);

   /** If this track is linked to another track (the track immediately before or
   * after it), return its partner. Otherwise return null. */
   Track *GetLink(Track * t) const;

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

   /** \brief Find out how many channels this track list mixes to
   *
   * This is used in exports of the tracks to work out whether to export in
   * Mono, Stereo etc. @param selectionOnly Whether to consider the entire track
   * list or only the selected members of it
   */
   int GetNumExportChannels(bool selectionOnly);

   WaveTrackArray GetWaveTrackArray(bool selectionOnly);
   /** Consider this function depricated in favor of GetWaveTrackArray */
   void GetWaveTracks(bool selectionOnly, int *num, WaveTrack ***tracks);

#if defined(USE_MIDI)
   NoteTrackArray GetNoteTrackArray(bool selectionOnly);
#endif

   /// Mainly a test function. Uses a linear search, so could be slow.
   bool Contains(Track * t) const;

   bool IsEmpty() const;
   int GetCount() const;

   double GetStartTime() const;
   double GetEndTime() const;

   double GetMinOffset() const;
   int GetHeight() const;

#if LEGACY_PROJECT_FILE_SUPPORT
   // File I/O
   virtual bool Load(wxTextFile * in, DirManager * dirManager);
   virtual bool Save(wxTextFile * out, bool overwrite);
#endif

 private:
   void RecalcPositions(const TrackListNode *node);
   void UpdatedEvent(const TrackListNode *node);
   void ResizedEvent(const TrackListNode *node);

   void Swap(TrackListNode * s1, TrackListNode * s2);

   TrackListNode *head;
   TrackListNode *tail;
};

class AUDACITY_DLL_API TrackFactory
{
 private:
   TrackFactory(DirManager *dirManager):
      mDirManager(dirManager)
   {
   }

   DirManager *mDirManager;
   friend class AudacityProject;
   friend class BenchmarkDialog;

 public:
   // These methods are defined in WaveTrack.cpp, NoteTrack.cpp,
   // LabelTrack.cpp, and TimeTrack.cpp respectively
   WaveTrack* DuplicateWaveTrack(WaveTrack &orig);
   WaveTrack *NewWaveTrack(sampleFormat format = (sampleFormat)0,
                           double rate = 0);
   LabelTrack *NewLabelTrack();
   TimeTrack *NewTimeTrack();
#if defined(USE_MIDI)
   NoteTrack *NewNoteTrack();
#endif
};

#endif
