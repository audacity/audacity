/**********************************************************************

  Audacity: A Digital Audio Editor

  Track.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_TRACK__
#define __AUDACITY_TRACK__

#include "Audacity.h" // for USE_* macros

#include "Experimental.h"

#include "MemoryX.h"
#include <vector>
#include <list>
#include <functional>
#include <wx/gdicmn.h>
#include <wx/longlong.h>

#include "SampleFormat.h"
#include "tracks/ui/CommonTrackPanelCell.h"
#include "xml/XMLTagHandler.h"

#ifdef __WXMSW__
#pragma warning(disable:4284)
#endif

class wxTextFile;
class DirManager;
class Track;
class AudioTrack;
class PlayableTrack;
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

using TrackArray = std::vector< Track* >;
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

enum class TrackKind
{
   None,
   Wave,
#if defined(USE_MIDI)
   Note,
#endif
   Label,
   Time,
   Audio,
   Playable,
   All
};

/// Compile-time function on enum values.
/// It knows all inheritance relations among Track subclasses
/// even where the track types are only forward declared.
constexpr bool CompatibleTrackKinds( TrackKind desired, TrackKind actual )
{
   return
      (desired == actual)
      ||
      (desired == TrackKind::All)
      ||
      (desired == TrackKind::Audio    && actual == TrackKind::Wave)
#ifdef USE_MIDI
      ||
      (desired == TrackKind::Audio    && actual == TrackKind::Note)
#endif
      ||
      (desired == TrackKind::Playable && actual == TrackKind::Wave)
#ifdef EXPERIMENTAL_MIDI_OUT
      ||
      (desired == TrackKind::Playable && actual == TrackKind::Note)
#endif
   ;
}

/// \brief Metaprogramming in TrackTyper lets track_cast work even when the track
/// subclasses are visible only as incomplete types
namespace TrackTyper {
   template<typename, TrackKind> struct Pair;
   using List = std::tuple<
     Pair<Track,         TrackKind::All>,
     Pair<AudioTrack,    TrackKind::Audio>,
     Pair<PlayableTrack, TrackKind::Playable>,
     Pair<LabelTrack,    TrackKind::Label>,
     Pair<NoteTrack,     TrackKind::Note>,
     Pair<TimeTrack,     TrackKind::Time>,
     Pair<WaveTrack,     TrackKind::Wave>
     // New classes can be added easily to this list
   >;
   template<typename...> struct Lookup;
   template<typename TrackType, TrackKind Here, typename... Rest>
      struct Lookup< TrackType, std::tuple< Pair<TrackType, Here>, Rest... > > {
         static constexpr TrackKind value() {
            return Here;
         }
      };
   template<typename TrackType, typename NotHere, typename... Rest>
      struct Lookup< TrackType, std::tuple< NotHere, Rest... > > {
         static constexpr TrackKind value() {
            return Lookup< TrackType, std::tuple< Rest... > >::value();
         }
      };
};

template<typename TrackType> constexpr TrackKind track_kind ()
{
   using namespace TrackTyper;
   return Lookup< typename std::remove_const<TrackType>::type, List >::value();
}

// forward declarations, so we can make them friends
template<typename T>
   typename std::enable_if< std::is_pointer<T>::value, T >::type
      track_cast(Track *track);

template<typename T>
   typename std::enable_if<
      std::is_pointer<T>::value &&
         std::is_const< typename std::remove_pointer< T >::type >::value,
      T
   >::type
      track_cast(const Track *track);

class ViewInfo;

/// This is an in-session identifier of track objects across undo states
///
/// It does not persist between sessions
/// Default constructed value is not equal to the id of any track that has ever
/// been added to a TrackList, or (directly or transitively) copied from such
/// (A pending additional track that is not yet applied is not considered added)
/// TrackIds are assigned uniquely across projects
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
   , public std::enable_shared_from_this<Track> // see SharedPointer()
{
   friend class TrackList;

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

 private:
   bool           mSelected;

 protected:
   bool           mLinked;
   bool           mMinimized;

 public:

   enum ChannelType
   {
      LeftChannel = 0,
      RightChannel = 1,
      MonoChannel = 2
   };
   
   TrackId GetId() const { return mId; }
 private:
   void SetId( TrackId id ) { mId = id; }
 public:

   // Given a bare pointer, find a shared_ptr.  Undefined results if the track
   // is not yet managed by a shared_ptr.  Undefined results if the track is
   // not really of the subclass.  (That is, trusts the caller and uses static
   // not dynamic casting.)
   template<typename Subclass = Track>
   inline std::shared_ptr<Subclass> SharedPointer()
   {
      // shared_from_this is injected into class scope by base class
      // std::enable_shared_from_this<Track>
      return std::static_pointer_cast<Subclass>( shared_from_this() );
   }

   template<typename Subclass = const Track>
   inline auto SharedPointer() const -> typename
      std::enable_if<
         std::is_const<Subclass>::value, std::shared_ptr<Subclass>
      >::type
   {
      // shared_from_this is injected into class scope by base class
      // std::enable_shared_from_this<Track>
      return std::static_pointer_cast<Subclass>( shared_from_this() );
   }

   // Static overloads of SharedPointer for when the pointer may be null
   template<typename Subclass = Track>
   static inline std::shared_ptr<Subclass> SharedPointer( Track *pTrack )
   { return pTrack ? pTrack->SharedPointer<Subclass>() : nullptr; }

   template<typename Subclass = const Track>
   static inline std::shared_ptr<Subclass> SharedPointer( const Track *pTrack )
   { return pTrack ? pTrack->SharedPointer<Subclass>() : nullptr; }

   // Find anything registered with TrackList::RegisterPendingChangedTrack and
   // not yet cleared or applied; if no such exists, return this track
   std::shared_ptr<const Track> SubstitutePendingChangedTrack() const;

   // If this track is a pending changed track, return the corresponding
   // original; else return this track
   std::shared_ptr<const Track> SubstituteOriginalTrack() const;

   // Cause certain overriding tool modes (Zoom; future ones?) to behave
   // uniformly in all tracks, disregarding track contents.
   // Do not further override this...
   std::vector<UIHandlePtr> HitTest
      (const TrackPanelMouseState &, const AudacityProject *pProject)
      final override;

   // Delegates the handling to the related TCP cell
   std::shared_ptr<TrackPanelCell> ContextMenuDelegate() override;

 public:

   // Rather override this for subclasses:
   virtual std::vector<UIHandlePtr> DetailedHitTest
      (const TrackPanelMouseState &,
       const AudacityProject *pProject, int currentTool, bool bMultiTool)
      = 0;

   mutable wxSize vrulerSize;

   // Return another, associated TrackPanelCell object that implements the
   // drop-down, close and minimize buttons, etc.
   std::shared_ptr<TrackControls> GetTrackControl();
   std::shared_ptr<const TrackControls> GetTrackControl() const;

   // Return another, associated TrackPanelCell object that implements the
   // mouse actions for the vertical ruler
   std::shared_ptr<TrackVRulerControls> GetVRulerControl();
   std::shared_ptr<const TrackVRulerControls> GetVRulerControl() const;

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
   static void FinishCopy (const Track *n, Track *dest);

   // For use when loading a file.  Return true if ok, else make repair
   bool LinkConsistencyCheck();

   bool HasOwner() const { return static_cast<bool>(GetOwner());}

private:
   std::shared_ptr<TrackList> GetOwner() const { return mList.lock(); }

   Track *GetLink() const;
   bool GetLinked  () const { return mLinked; }

   friend WaveTrack; // WaveTrack needs to call SetLinked when reloading project
   void SetLinked  (bool l);

   void SetChannel(ChannelType c) { mChannel = c; }
private:
   // No need yet to make this virtual
   void DoSetLinked(bool l);

   TrackNodePointer GetNode() const;
   void SetOwner
      (const std::weak_ptr<TrackList> &list, TrackNodePointer node);

 // Keep in Track

 protected:
   ChannelType         mChannel;
   double              mOffset;

   mutable std::shared_ptr<DirManager> mDirManager;

 public:

   enum : unsigned { DefaultHeight = 150 };

   Track(const std::shared_ptr<DirManager> &projDirManager);
   Track(const Track &orig);

   virtual ~ Track();

   void Init(const Track &orig);

   using Holder = std::shared_ptr<Track>;
   virtual Holder Duplicate() const = 0;

   // Called when this track is merged to stereo with another, and should
   // take on some paramaters of its partner.
   virtual void Merge(const Track &orig);

   wxString GetName() const { return mName; }
   void SetName( const wxString &n );
   wxString GetDefaultName() const { return mDefaultName; }
   void SetDefaultName( const wxString &n ) { mDefaultName = n; }

   bool GetSelected() const { return mSelected; }

   virtual void SetSelected(bool s);

public:

   virtual ChannelType GetChannel() const { return mChannel;}
   virtual double GetOffset() const = 0;

   void Offset(double t) { SetOffset(GetOffset() + t); }
   virtual void SetOffset (double o) { mOffset = o; }

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

private:
   virtual TrackKind GetKind() const { return TrackKind::None; }

   template<typename T>
      friend typename std::enable_if< std::is_pointer<T>::value, T >::type
         track_cast(Track *track);
   template<typename T>
      friend typename std::enable_if<
         std::is_pointer<T>::value &&
            std::is_const< typename std::remove_pointer< T >::type >::value,
         T
      >::type
         track_cast(const Track *track);

public:
   bool SameKindAs(const Track &track) const
      { return GetKind() == track.GetKind(); }

   template < typename R = void >
   using Continuation = std::function< R() >;
   using Fallthrough = Continuation<>;
   
private:

   // Variadic template specialized below
   template< typename ...Params >
   struct Executor;

   // This specialization grounds the recursion.
   template< typename R, typename ConcreteType >
   struct Executor< R, ConcreteType >
   {
      enum : unsigned { SetUsed = 0 };
      // No functions matched, so do nothing.
      R operator () (const void *) { return R{}; }
   };

   // And another specialization is needed for void return.
   template< typename ConcreteType >
   struct Executor< void, ConcreteType >
   {
      enum : unsigned { SetUsed = 0 };
      // No functions matched, so do nothing.
      void operator () (const void *) { }
   };

   // This struct groups some helpers needed to define the recursive cases of
   // Executor.
   struct Dispatcher {
      // This implements the specialization of Executor
      // for the first recursive case.
      template< typename R, typename ConcreteType,
                typename Function, typename ...Functions >
      struct inapplicable
      {
         using Tail = Executor< R, ConcreteType, Functions... >;
         enum : unsigned { SetUsed = Tail::SetUsed << 1 };

         // Ignore the first, inapplicable function and try others.
         R operator ()
            (const Track *pTrack,
             const Function &, const Functions &...functions)
         { return Tail{}( pTrack, functions... ); }
      };

      // This implements the specialization of Executor
      // for the second recursive case.
      template< typename R, typename BaseClass, typename ConcreteType,
                typename Function, typename ...Functions >
      struct applicable1
      {
         enum : unsigned { SetUsed = 1u };

         // Ignore the remaining functions and call the first only.
         R operator ()
            (const Track *pTrack,
             const Function &function, const Functions &...)
         { return function( (BaseClass *)pTrack ); }
      };

      // This implements the specialization of Executor
      // for the third recursive case.
      template< typename R, typename BaseClass, typename ConcreteType,
                typename Function, typename ...Functions >
      struct applicable2
      {
         using Tail = Executor< R, ConcreteType, Functions... >;
         enum : unsigned { SetUsed = (Tail::SetUsed << 1) | 1u };

         // Call the first function, which may request dispatch to the further
         // functions by invoking a continuation.
         R operator ()
            (const Track *pTrack, const Function &function,
             const Functions &...functions)
         {
            auto continuation = Continuation<R>{ [&] {
               return Tail{}( pTrack, functions... );
            } };
            return function( (BaseClass *)pTrack, continuation );
         }
      };

      // This variadic template chooses among the implementations above.
      template< typename ... > struct Switch;

      // Ground the recursion.
      template< typename R, typename ConcreteType >
      struct Switch< R, ConcreteType >
      {
         // No BaseClass of ConcreteType is acceptable to Function.
         template< typename Function, typename ...Functions >
            static auto test()
               -> inapplicable< R, ConcreteType, Function, Functions... >;
      };

      // Recursive case.
      template< typename R, typename ConcreteType,
                typename BaseClass, typename ...BaseClasses >
      struct Switch< R, ConcreteType, BaseClass, BaseClasses... >
      {
         using Retry = Switch< R, ConcreteType, BaseClasses... >;

         // If ConcreteType is not compatible with BaseClass, or if
         // Function does not accept BaseClass, try other BaseClasses.
         template< typename Function, typename ...Functions >
            static auto test( const void * )
               -> decltype( Retry::template test< Function, Functions... >() );

         // If BaseClass is a base of ConcreteType and Function can take it,
         // then overload resolution chooses this.
         // If not, then the sfinae rule makes this overload unavailable.
         template< typename Function, typename ...Functions >
            static auto test( std::true_type * )
               -> decltype(
                  (void) std::declval<Function>()
                     ( (BaseClass*)nullptr ),
                  applicable1< R, BaseClass, ConcreteType,
                               Function, Functions... >{}
               );

         // If BaseClass is a base of ConcreteType and Function can take it,
         // with a second argument for a continuation,
         // then overload resolution chooses this.
         // If not, then the sfinae rule makes this overload unavailable.
         template< typename Function, typename ...Functions >
            static auto test( std::true_type * )
               -> decltype(
                  (void) std::declval<Function>()
                     ( (BaseClass*)nullptr,
                       std::declval< Continuation<R> >() ),
                  applicable2< R, BaseClass, ConcreteType,
                               Function, Functions... >{}
               );

         static constexpr bool Compatible = CompatibleTrackKinds(
            track_kind<BaseClass>(), track_kind<ConcreteType>() );
         template< typename Function, typename ...Functions >
            static auto test()
               -> decltype(
                  test< Function, Functions... >(
                     (std::integral_constant<bool, Compatible>*)nullptr) );
      };
   };

   // This specialization is the recursive case for non-const tracks.
   template< typename R, typename ConcreteType,
             typename Function, typename ...Functions >
   struct Executor< R, ConcreteType, Function, Functions... >
      : decltype(
         Dispatcher::Switch< R, ConcreteType,
            Track, AudioTrack, PlayableTrack,
            WaveTrack, LabelTrack, TimeTrack,
            NoteTrack >
               ::template test<Function, Functions... >())
   {};

   // This specialization is the recursive case for const tracks.
   template< typename R, typename ConcreteType,
             typename Function, typename ...Functions >
   struct Executor< R, const ConcreteType, Function, Functions... >
      : decltype(
         Dispatcher::Switch< R, ConcreteType,
            const Track, const AudioTrack, const PlayableTrack,
            const WaveTrack, const LabelTrack, const TimeTrack,
            const NoteTrack >
               ::template test<Function, Functions... >())
   {};

public:

   // A variadic function taking any number of function objects, each taking
   // a pointer to Track or a subclass, maybe const-qualified, and maybe a
   // second argument which is a fall-through continuation.
   // Each of the function objects (and supplied continuations) returns R.
   // Call the first in the sequence that accepts the actual type of the track.
   // If no function accepts the track, do nothing and return R{}
   // if R is not void.
   // If one of the functions invokes the call-through, then the next following
   // applicable funtion is called.
   template< typename R = void, typename ...Functions >
   R TypeSwitch(const Functions &...functions)
   {
      using WaveExecutor =
         Executor< R, WaveTrack,  Functions... >;
      using NoteExecutor =
         Executor< R, NoteTrack,  Functions... >;
      using LabelExecutor =
         Executor< R, LabelTrack, Functions... >;
      using TimeExecutor =
         Executor< R, TimeTrack,  Functions... >;
      using DefaultExecutor =
         Executor< R, Track >;
      enum { All = sizeof...( functions ) };

      static_assert(
         (1u << All) - 1u ==
            (WaveExecutor::SetUsed |
             NoteExecutor::SetUsed |
             LabelExecutor::SetUsed |
             TimeExecutor::SetUsed),
         "Uncallable case in Track::TypeSwitch"
      );

      switch (GetKind()) {
         case TrackKind::Wave:
            return WaveExecutor{} (this,  functions...);
#if defined(USE_MIDI)
         case TrackKind::Note:
            return NoteExecutor{} (this,  functions...);
#endif
         case TrackKind::Label:
            return LabelExecutor{}(this, functions...);
         case TrackKind::Time:
            return TimeExecutor{} (this,  functions...);
         default:
            return DefaultExecutor{} (this);
      }
   }

   // This is the overload of TypeSwitch (see above) for const tracks, taking
   // callable arguments that only accept arguments that are pointers to const
   template< typename R = void, typename ...Functions >
   R TypeSwitch(const Functions &...functions) const
   {
      using WaveExecutor =
         Executor< R, const WaveTrack,  Functions... >;
      using NoteExecutor =
         Executor< R, const NoteTrack,  Functions... >;
      using LabelExecutor =
         Executor< R, const LabelTrack, Functions... >;
      using TimeExecutor =
         Executor< R, const TimeTrack,  Functions... >;
      using DefaultExecutor =
         Executor< R, const Track >;
      enum { All = sizeof...( functions ) };

      static_assert(
         (1u << All) - 1u ==
            (WaveExecutor::SetUsed |
             NoteExecutor::SetUsed |
             LabelExecutor::SetUsed |
             TimeExecutor::SetUsed),
         "Uncallable case in Track::TypeSwitch"
      );

      switch (GetKind()) {
         case TrackKind::Wave:
            return WaveExecutor{} (this,  functions...);
#if defined(USE_MIDI)
         case TrackKind::Note:
            return NoteExecutor{} (this,  functions...);
#endif
         case TrackKind::Label:
            return LabelExecutor{}(this, functions...);
         case TrackKind::Time:
            return TimeExecutor{} (this,  functions...);
         default:
            return DefaultExecutor{} (this);
      }
   }

   // XMLTagHandler callback methods -- NEW virtual for writing
   virtual void WriteXML(XMLWriter &xmlFile) const = 0;

   // Returns true if an error was encountered while trying to
   // open the track from XML
   virtual bool GetErrorOpening() { return false; }

   virtual double GetStartTime() const = 0;
   virtual double GetEndTime() const = 0;

   // Checks if sync-lock is on and any track in its sync-lock group is selected.
   bool IsSyncLockSelected() const;

   // Send an event to listeners when state of the track changes
   // To do: define values for the argument to distinguish different parts
   // of the state, perhaps with wxNewId
   void Notify( int code = -1 );

   // An always-true predicate useful for defining iterators
   bool Any() const;

   // Frequently useful operands for + and -
   bool IsSelected() const;
   bool IsSelectedOrSyncLockSelected() const;
   bool IsLeader() const;
   bool IsSelectedLeader() const;

protected:
   std::shared_ptr<Track> DoFindTrack() override;

   // These are called to create controls on demand:
   virtual std::shared_ptr<TrackControls> DoGetControls() = 0;
   virtual std::shared_ptr<TrackVRulerControls> DoGetVRulerControls() = 0;

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
   void SetMute    (bool m);
   void SetSolo    (bool s);

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

// Functions to encapsulate the checked down-casting of track pointers,
// eliminating possibility of error -- and not quietly casting away const
// typical usage:
// if (auto wt = track_cast<WaveTrack*>(track)) { ... }
template<typename T>
   inline typename std::enable_if< std::is_pointer<T>::value, T >::type
      track_cast(Track *track)
{
   using BareType = typename std::remove_pointer< T >::type;
   if (track &&
       CompatibleTrackKinds( track_kind<BareType>(), track->GetKind() ))
      return reinterpret_cast<T>(track);
   else
      return nullptr;
}

// Overload for const pointers can cast only to other const pointer types
template<typename T>
   inline typename std::enable_if<
      std::is_pointer<T>::value &&
         std::is_const< typename std::remove_pointer< T >::type >::value,
      T
   >::type
      track_cast(const Track *track)
{
   using BareType = typename std::remove_pointer< T >::type;
   if (track &&
       CompatibleTrackKinds( track_kind<BareType>(), track->GetKind() ))
      return reinterpret_cast<T>(track);
   else
      return nullptr;
}

template < typename TrackType > struct TrackIterRange;

// new track iterators can eliminate the need to cast the result
template <
   typename TrackType // Track or a subclass, maybe const-qualified
> class TrackIter
   : public ValueIterator< TrackType *, std::bidirectional_iterator_tag >
{
public:
   // Type of predicate taking pointer to const TrackType
   // TODO C++14:  simplify away ::type
   using FunctionType = std::function< bool(
      typename std::add_pointer<
         typename std::add_const<
            typename std::remove_pointer<
               TrackType
            >::type
         >::type
      >::type
   ) >;

   template<typename Predicate = FunctionType>
   TrackIter( TrackNodePointer begin, TrackNodePointer iter,
              TrackNodePointer end, const Predicate &pred = {} )
      : mBegin( begin ), mIter( iter ), mEnd( end ), mPred( pred )
   {
      // Establish the class invariant
      if (this->mIter != this->mEnd && !this->valid())
         this->operator ++ ();
   }

   // Return an iterator that replaces the predicate, advancing to the first
   // position at or after the old position that satisfies the new predicate,
   // or to the end.
   template < typename Predicate2 >
      TrackIter Filter( const Predicate2 &pred2 ) const
   {
      return { this->mBegin, this->mIter, this->mEnd, pred2 };
   }

   // Return an iterator that refines the subclass (and not removing const),
   // advancing to the first position at or after the old position that
   // satisfies the type constraint, or to the end
   template < typename TrackType2 >
      auto Filter() const
         -> typename std::enable_if<
            std::is_base_of< TrackType, TrackType2 >::value &&
               (!std::is_const<TrackType>::value ||
                 std::is_const<TrackType2>::value),
            TrackIter< TrackType2 >
         >::type
   {
      return { this->mBegin, this->mIter, this->mEnd, this->mPred };
   }

   const FunctionType &GetPredicate() const
   { return this->mPred; }

   // Unlike with STL iterators, this class gives well defined behavior when
   // you increment an end iterator: you get the same.
   TrackIter &operator ++ ()
   {
      // Maintain the class invariant
      if (this->mIter != this->mEnd) do
         ++this->mIter.first;
      while (this->mIter != this->mEnd && !this->valid() );
      return *this;
   }

   TrackIter operator ++ (int)
   {
      TrackIter result { *this };
      this-> operator ++ ();
      return result;
   }

   // Unlike with STL iterators, this class gives well defined behavior when
   // you decrement past the beginning of a range: you wrap and get an end
   // iterator.
   TrackIter &operator -- ()
   {
      // Maintain the class invariant
      do {
         if (this->mIter == this->mBegin)
            // Go circularly
            this->mIter = this->mEnd;
         else
            --this->mIter.first;
      } while (this->mIter != this->mEnd && !this->valid() );
      return *this;
   }

   TrackIter operator -- (int)
   {
      TrackIter result { *this };
      this->operator -- ();
      return result;
   }

   // Unlike with STL iterators, this class gives well defined behavior when
   // you dereference an end iterator: you get a null pointer.
   TrackType *operator * () const
   {
      if (this->mIter == this->mEnd)
         return nullptr;
      else
         // Other methods guarantee that the cast is correct
         // (provided no operations on the TrackList invalidated
         // underlying iterators or replaced the tracks there)
         return static_cast< TrackType * >( &**this->mIter.first );
   }

   // This might be called operator + ,
   // but that might wrongly suggest constant time when the iterator is not
   // random access.
   TrackIter advance( long amount ) const
   {
      auto copy = *this;
      std::advance( copy, amount );
      return copy;
   }

   friend inline bool operator == (TrackIter a, TrackIter b)
   {
      // Assume the predicate is not stateful.  Just compare the iterators.
      return
         a.mIter == b.mIter
         // Assume this too:
         // && a.mBegin == b.mBegin && a.mEnd == b.mEnd
      ;
   }

   friend inline bool operator != (TrackIter a, TrackIter b)
   {
      return !(a == b);
   }

private:
   bool valid() const
   {
      // assume mIter != mEnd
      const auto pTrack = track_cast< TrackType * >( &**this->mIter.first );
      if (!pTrack)
         return false;
      return !this->mPred || this->mPred( pTrack );
   }

   // This friendship is needed in TrackIterRange::StartingWith and
   // TrackIterRange::EndingAfter()
   friend TrackIterRange< TrackType >;

   // The class invariant is that mIter == mEnd, or else, mIter != mEnd and
   // **mIter is of the appropriate subclass and mPred(&**mIter) is true.
   TrackNodePointer mBegin, mIter, mEnd;
   FunctionType mPred;
};

template <
   typename TrackType // Track or a subclass, maybe const-qualified
> struct TrackIterRange
   : public IteratorRange< TrackIter< TrackType > >
{
   TrackIterRange
      ( const TrackIter< TrackType > &begin,
        const TrackIter< TrackType > &end )
         : IteratorRange< TrackIter< TrackType > >
            ( begin, end )
   {}

   // Conjoin the filter predicate with another predicate
   // Read + as "and"
   template< typename Predicate2 >
      TrackIterRange operator + ( const Predicate2 &pred2 ) const
   {
      const auto &pred1 = this->first.GetPredicate();
      using Function = typename TrackIter<TrackType>::FunctionType;
      const auto &newPred = pred1
         ? Function{ [=] (typename Function::argument_type track) {
            return pred1(track) && pred2(track);
         } }
         : Function{ pred2 };
      return {
         this->first.Filter( newPred ),
         this->second.Filter( newPred )
      };
   }

   // Specify the added conjunct as a pointer to member function
   // Read + as "and"
   template< typename R, typename C >
      TrackIterRange operator + ( R ( C ::* pmf ) () const ) const
   {
      return this->operator + ( std::mem_fn( pmf ) );
   }

   // Conjoin the filter predicate with the negation of another predicate
   // Read - as "and not"
   template< typename Predicate2 >
      TrackIterRange operator - ( const Predicate2 &pred2 ) const
   {
      using ArgumentType =
         typename TrackIterRange::iterator::FunctionType::argument_type;
      auto neg = [=] (ArgumentType track) { return !pred2( track ); };
      return this->operator + ( neg );
   }

   // Specify the negated conjunct as a pointer to member function
   // Read - as "and not"
   template< typename R, typename C >
      TrackIterRange operator - ( R ( C ::* pmf ) () const ) const
   {
      return this->operator + ( std::not1( std::mem_fn( pmf ) ) );
   }

   template< typename TrackType2 >
      TrackIterRange< TrackType2 > Filter() const
   {
      return {
         this-> first.template Filter< TrackType2 >(),
         this->second.template Filter< TrackType2 >()
      };
   }

   TrackIterRange StartingWith( const Track *pTrack ) const
   {
      auto newBegin = this->find( pTrack );
      // More careful construction is needed so that the independent
      // increment and decrement of each iterator in the NEW pair
      // has the expected behavior at boundaries of the range
      return {
         { newBegin.mIter, newBegin.mIter,    this->second.mEnd,
           this->first.GetPredicate() },
         { newBegin.mIter, this->second.mEnd, this->second.mEnd,
           this->second.GetPredicate() }
      };
   }

   TrackIterRange EndingAfter( const Track *pTrack ) const
   {
      const auto newEnd = this->reversal().find( pTrack ).base();
      // More careful construction is needed so that the independent
      // increment and decrement of each iterator in the NEW pair
      // has the expected behavior at boundaries of the range
      return {
         { this->first.mBegin, this->first.mIter, newEnd.mIter,
           this->first.GetPredicate() },
         { this->first.mBegin, newEnd.mIter,      newEnd.mIter,
           this->second.GetPredicate() }
      };
   }

   // Exclude one given track
   TrackIterRange Excluding ( const TrackType *pExcluded ) const
   {
      return this->operator - (
         [=](const Track *pTrack){ return pExcluded == pTrack; } );
   }

   // See Track::TypeSwitch
   template< typename ...Functions >
   void Visit(const Functions &...functions)
   {
      for (auto track : *this)
         track->TypeSwitch(functions...);
   }

   // See Track::TypeSwitch
   // Visit until flag is false, or no more tracks
   template< typename Flag, typename ...Functions >
   void VisitWhile(Flag &flag, const Functions &...functions)
   {
      if ( flag ) for (auto track : *this) {
         track->TypeSwitch(functions...);
         if (!flag)
            break;
      }
   }
};


/** \brief TrackList is a flat linked list of tracks supporting Add,  Remove,
 * Clear, and Contains, plus serialization of the list of tracks.
 */

struct TrackListEvent : public wxCommandEvent
{
   explicit
   TrackListEvent(
      wxEventType commandType,
      const std::weak_ptr<Track> &pTrack = {}, int code = -1)
   : wxCommandEvent{ commandType }
   , mpTrack{ pTrack }
   , mCode{ code }
   {}

   TrackListEvent( const TrackListEvent& ) = default;

   wxEvent *Clone() const override { return new TrackListEvent(*this); }

   std::weak_ptr<Track> mpTrack;
   int mCode;
};

// Posted when the set of selected tracks changes.
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API,
                         EVT_TRACKLIST_SELECTION_CHANGE, TrackListEvent);

// Posted when certain fields of a track change.
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API,
                         EVT_TRACKLIST_TRACK_DATA_CHANGE, TrackListEvent);

// Posted when tracks are reordered but otherwise unchanged.
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API,
                         EVT_TRACKLIST_PERMUTED, TrackListEvent);

// Posted when some track changed its height.
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API,
                         EVT_TRACKLIST_RESIZING, TrackListEvent);

// Posted when a track has been added to a tracklist.
// Also posted when one track replaces another
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API,
                         EVT_TRACKLIST_ADDITION, TrackListEvent);

// Posted when a track has been deleted from a tracklist.
// Also posted when one track replaces another
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API,
                         EVT_TRACKLIST_DELETION, TrackListEvent);

class TrackList final : public wxEvtHandler, public ListOfTracks
   , public std::enable_shared_from_this<TrackList>
{
   // privatize this, make you use Add instead:
   using ListOfTracks::push_back;

   // privatize this, make you use Swap instead:
   using ListOfTracks::swap;

   // Disallow copy
   TrackList(const TrackList &that) = delete;
   TrackList &operator= (const TrackList&) = delete;

   // Allow move
   TrackList(TrackList &&that) : TrackList() { Swap(that); }
   TrackList& operator= (TrackList&&);

   void clear() = delete;

 public:
   // Create an empty TrackList
   // Don't call directly -- use Create() instead
   TrackList();

   // Create an empty TrackList
   static std::shared_ptr<TrackList> Create();

   // Move is defined in terms of Swap
   void Swap(TrackList &that);

   // Destructor
   virtual ~TrackList();

   // Iteration

   // Hide the inherited begin() and end()
   using iterator = TrackIter<Track>;
   using const_iterator = TrackIter<const Track>;
   using value_type = Track *;
   iterator begin() { return Any().begin(); }
   iterator end() { return Any().end(); }
   const_iterator begin() const { return Any().begin(); }
   const_iterator end() const { return Any().end(); }
   const_iterator cbegin() const { return begin(); }
   const_iterator cend() const { return end(); }

   // Turn a pointer into an iterator (constant time).
   template < typename TrackType = Track >
      auto Find(Track *pTrack)
         -> TrackIter< TrackType >
   {
      if (!pTrack || pTrack->GetOwner().get() != this)
         return EndIterator<TrackType>();
      else
         return MakeTrackIterator<TrackType>( pTrack->GetNode() );
   }

   // Turn a pointer into an iterator (constant time).
   template < typename TrackType = const Track >
      auto Find(const Track *pTrack) const
         -> typename std::enable_if< std::is_const<TrackType>::value,
            TrackIter< TrackType >
         >::type
   {
      if (!pTrack || pTrack->GetOwner().get() != this)
         return EndIterator<TrackType>();
      else
         return MakeTrackIterator<TrackType>( pTrack->GetNode() );
   }

   // If the track is not an audio track, or not one of a group of channels,
   // return the track itself; else return the first channel of its group --
   // in either case as an iterator that will only visit other leader tracks.
   // (Generalizing away from the assumption of at most stereo)
   TrackIter< Track > FindLeader( Track *pTrack );

   TrackIter< const Track >
      FindLeader( const Track *pTrack ) const
   {
      return const_cast<TrackList*>(this)->
         FindLeader( const_cast<Track*>(pTrack) ).Filter< const Track >();
   }


   template < typename TrackType = Track >
      auto Any()
         -> TrackIterRange< TrackType >
   {
      return Tracks< TrackType >();
   }

   template < typename TrackType = const Track >
      auto Any() const
         -> typename std::enable_if< std::is_const<TrackType>::value,
            TrackIterRange< TrackType >
         >::type
   {
      return Tracks< TrackType >();
   }

   // Abbreviating some frequently used cases
   template < typename TrackType = Track >
      auto Selected()
         -> TrackIterRange< TrackType >
   {
      return Tracks< TrackType >( &Track::IsSelected );
   }

   template < typename TrackType = const Track >
      auto Selected() const
         -> typename std::enable_if< std::is_const<TrackType>::value,
            TrackIterRange< TrackType >
         >::type
   {
      return Tracks< TrackType >( &Track::IsSelected );
   }


   template < typename TrackType = Track >
      auto Leaders()
         -> TrackIterRange< TrackType >
   {
      return Tracks< TrackType >( &Track::IsLeader );
   }

   template < typename TrackType = const Track >
      auto Leaders() const
         -> typename std::enable_if< std::is_const<TrackType>::value,
            TrackIterRange< TrackType >
         >::type
   {
      return Tracks< TrackType >( &Track::IsLeader );
   }


   template < typename TrackType = Track >
      auto SelectedLeaders()
         -> TrackIterRange< TrackType >
   {
      return Tracks< TrackType >( &Track::IsSelectedLeader );
   }

   template < typename TrackType = const Track >
      auto SelectedLeaders() const
         -> typename std::enable_if< std::is_const<TrackType>::value,
            TrackIterRange< TrackType >
         >::type
   {
      return Tracks< TrackType >( &Track::IsSelectedLeader );
   }


   template<typename TrackType>
      static auto SingletonRange( TrackType *pTrack )
         -> TrackIterRange< TrackType >
   {
      return pTrack->GetOwner()->template Any<TrackType>()
         .StartingWith( pTrack ).EndingAfter( pTrack );
   }


   static TrackIterRange< Track >
      SyncLockGroup( Track *pTrack );

   static TrackIterRange< const Track >
      SyncLockGroup( const Track *pTrack )
   {
      return SyncLockGroup(const_cast<Track*>(pTrack)).Filter<const Track>();
   }

private:
   Track *DoAddToHead(const std::shared_ptr<Track> &t);
   Track *DoAdd(const std::shared_ptr<Track> &t);

   template< typename TrackType, typename InTrackType >
      static TrackIterRange< TrackType >
         Channels_( TrackIter< InTrackType > iter1 )
   {
      // Assume iterator filters leader tracks
      if (*iter1) {
         return {
            iter1.Filter( &Track::Any )
               .template Filter<TrackType>(),
            (++iter1).Filter( &Track::Any )
               .template Filter<TrackType>()
         };
      }
      else
         // empty range
         return {
            iter1.template Filter<TrackType>(),
            iter1.template Filter<TrackType>()
         };
   }

public:
   // Find an iterator range of channels including the given track.
   template< typename TrackType >
      static auto Channels( TrackType *pTrack )
         -> TrackIterRange< TrackType >
   {
      return Channels_<TrackType>( pTrack->GetOwner()->FindLeader(pTrack) );
   }

   friend class Track;

   /// For use in sorting:  assume each iterator points into this list, no duplications
   void Permute(const std::vector<TrackNodePointer> &permutation);

   Track *FindById( TrackId id );

   /// Add a Track, giving it a fresh id
   template<typename TrackKind>
      TrackKind *AddToHead( const std::shared_ptr< TrackKind > &t )
         { return static_cast< TrackKind* >( DoAddToHead( t ) ); }

   template<typename TrackKind>
      TrackKind *Add( const std::shared_ptr< TrackKind > &t )
         { return static_cast< TrackKind* >( DoAdd( t ) ); }

   /** \brief Define a group of channels starting at the given track
   *
   * @param track and (groupSize - 1) following tracks must be in this
   * list.  They will be disassociated from any groups they already belong to.
   * @param groupSize must be at least 1.
   * @param resetChannels if true, disassociated channels will be marked Mono.
   */
   void GroupChannels(
      Track &track, size_t groupSize, bool resetChannels = true );

   /// Replace first track with second track, give back a holder
   /// Give the replacement the same id as the replaced
   ListOfTracks::value_type Replace(
      Track * t, const ListOfTracks::value_type &with);

   /// Remove this Track or all children of this TrackList.
   /// Return an iterator to what followed the removed track.
   TrackNodePointer Remove(Track *t);

   /// Make the list empty
   void Clear(bool sendEvent = true);

   int GetGroupHeight(const Track * t) const;

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

   // Visit all tracks satisfying a predicate, mutative access
   template <
      typename TrackType = Track,
      typename Pred =
         typename TrackIterRange< TrackType >::iterator::FunctionType
   >
      auto Tracks( const Pred &pred = {} )
         -> TrackIterRange< TrackType >
   {
      auto b = getBegin(), e = getEnd();
      return { { b, b, e, pred }, { b, e, e, pred } };
   }

   // Visit all tracks satisfying a predicate, const access
   template <
      typename TrackType = const Track,
      typename Pred =
         typename TrackIterRange< TrackType >::iterator::FunctionType
   >
      auto Tracks( const Pred &pred = {} ) const
         -> typename std::enable_if< std::is_const<TrackType>::value,
            TrackIterRange< TrackType >
         >::type
   {
      auto b = const_cast<TrackList*>(this)->getBegin();
      auto e = const_cast<TrackList*>(this)->getEnd();
      return { { b, b, e, pred }, { b, e, e, pred } };
   }

   Track *GetPrev(Track * t, bool linked = false) const;
   Track *GetNext(Track * t, bool linked = false) const;
   
   std::pair<Track *, Track *> FindSyncLockGroup(Track *pMember) const;

   template < typename TrackType >
      TrackIter< TrackType >
         MakeTrackIterator( TrackNodePointer iter ) const
   {
      auto b = const_cast<TrackList*>(this)->getBegin();
      auto e = const_cast<TrackList*>(this)->getEnd();
      return { b, iter, e };
   }

   template < typename TrackType >
      TrackIter< TrackType >
         EndIterator() const
   {
      auto e = const_cast<TrackList*>(this)->getEnd();
      return { e, e, e };
   }

   TrackIterRange< Track > EmptyRange() const;

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
   void SelectionEvent( const std::shared_ptr<Track> &pTrack );
   void PermutationEvent();
   void DataEvent( const std::shared_ptr<Track> &pTrack, int code );
   void DeletionEvent();
   void AdditionEvent(TrackNodePointer node);
   void ResizingEvent(TrackNodePointer node);

   void SwapNodes(TrackNodePointer s1, TrackNodePointer s2);

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
   std::shared_ptr<WaveTrack> DuplicateWaveTrack(const WaveTrack &orig);
   std::shared_ptr<WaveTrack> NewWaveTrack(sampleFormat format = (sampleFormat)0,
                           double rate = 0);
   std::shared_ptr<LabelTrack> NewLabelTrack();
   std::shared_ptr<TimeTrack> NewTimeTrack();
#if defined(USE_MIDI)
   std::shared_ptr<NoteTrack> NewNoteTrack();
#endif
};

// global functions
struct TransportTracks;
TransportTracks GetAllPlaybackTracks(TrackList &trackList, bool selectedOnly, bool useMidi = false);

#endif
