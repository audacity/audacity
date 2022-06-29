/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file Track.h
  @brief declares abstract base class Track, TrackList, and iterators over TrackList

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_TRACK__
#define __AUDACITY_TRACK__

#include <atomic>
#include <utility>
#include <vector>
#include <list>
#include <functional>
#include <wx/longlong.h>

#include "ClientData.h"
#include "Observer.h"
// TrackAttachment needs to be a complete type for the Windows build, though
// not the others, so there is a nested include here:
#include "TrackAttachment.h"
#include "XMLTagHandler.h"

#ifdef __WXMSW__
#pragma warning(disable:4284)
#endif

class wxTextFile;
class Track;
class ProjectSettings;
class AudacityProject;

using TrackArray = std::vector< Track* >;

class TrackList;

using ListOfTracks = std::list< std::shared_ptr< Track > >;

//! Pairs a std::list iterator and a pointer to a list, for comparison purposes
/*! Compare owning lists first, and only if same, then the iterators;
 else MSVC debug runtime complains. */
using TrackNodePointer =
std::pair< ListOfTracks::iterator, ListOfTracks* >;

inline bool operator == (const TrackNodePointer &a, const TrackNodePointer &b)
{ return a.second == b.second && a.first == b.first; }

inline bool operator != (const TrackNodePointer &a, const TrackNodePointer &b)
{ return !(a == b); }

namespace {

//! Empty class which will have subclasses
struct TrackTypeCountTag{};

/*! Declared but undefined function, whose overloads will define a compile-time
  function from integers to known sub-types of Track
 */
auto enumerateTrackTypes(TrackTypeCountTag, ...) -> void;

//! What type is associated with `U` (at the point of instantiation for Tag)?
template<unsigned U, typename Tag> using EnumeratedTrackType =
   std::remove_reference_t< decltype( enumerateTrackTypes( Tag{},
      std::integral_constant<unsigned, U>{} ) ) >;

//! Embedded `value` member counts track types so far declared in the compilation unit
/*!
 @tparam Tag a distinct subclass of TrackTypeCountTag for one point of instantiation of the template
 */
template<typename Tag>
class CountTrackTypes {
   template<unsigned U> struct Stop{
      static constexpr unsigned value = U; };
   template<unsigned U> struct Count
      : std::conditional_t<
         std::is_void_v<EnumeratedTrackType<U, Tag>>,
         Stop<U>,
         Count<U + 1>
      >
   {};
public:
   static constexpr unsigned value = Count<0>::value;
};

//! Embedded `type` member is the tuple of track types so far declared in the compilation unit
/*!
 Each track subtype occurs earlier than its base classes in the tuple of types

 @tparam Tag a distinct subclass of TrackTypeCountTag for one point of instantiation of the template
 */
template<typename Tag>
class CollectTrackTypes {
   template<typename... Types> struct Stop{
      using type = std::tuple<Types...>; };
   template<unsigned U, typename... Types> struct Accumulate;
   template<unsigned U, typename Type, typename... Types> struct AccumulateType
      : std::conditional_t< std::is_void_v<Type>,
         Stop<Types...>,
         Accumulate<U + 1, Type, Types...>
      >
   {};
   template<unsigned U, typename... Types> struct Accumulate
      : AccumulateType<U, EnumeratedTrackType<U, Tag>, Types...>
   {};
public:
   using type = typename Accumulate<0>::type;
};

//! Implements the ENUMERATE_TRACK_TYPE macro
template<typename T>
struct TrackTypeCounter {
   struct Tag : TrackTypeCountTag {};
   static constexpr unsigned value = CountTrackTypes<Tag>::value;
};

}

//! This macro should be called immediately after each definition of a track subtype
/*! It must occur at file scope, not within any other namespace */
#define ENUMERATE_TRACK_TYPE(T) namespace { auto enumerateTrackTypes(\
   TrackTypeCountTag, \
   std::integral_constant<unsigned, TrackTypeCounter<T>::value>) -> T&; }

// forward declarations, so we can make them friends
template<typename T>
   std::enable_if_t< std::is_pointer_v<T>, T >
      track_cast(Track *track);

template<typename T>
   std::enable_if_t<
      std::is_pointer_v<T> &&
         std::is_const_v< std::remove_pointer_t< T > >,
      T
   >
      track_cast(const Track *track);

//! An in-session identifier of track objects across undo states.  It does not persist between sessions
/*!
    Default constructed value is not equal to the id of any track that has ever
    been added to a TrackList, or (directly or transitively) copied from such.
    (A track added by TrackList::RegisterPendingNewTrack() that is not yet applied is not
    considered added.)
 
    TrackIds are assigned uniquely across projects. */
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

//! Optional extra information about an interval, appropriate to a subtype of Track
struct TRACK_API TrackIntervalData {
   virtual ~TrackIntervalData();
};

//! A start and an end time, and non-mutative access to optional extra information
/*! @invariant `Start() <= End()` */
class ConstTrackInterval {
public:

   /*! @pre `start <= end` */
   ConstTrackInterval( double start, double end,
      std::unique_ptr<TrackIntervalData> pExtra = {} )
   : start{ start }, end{ end }, pExtra{ std::move( pExtra ) }
   {
      wxASSERT( start <= end );
   }

   ConstTrackInterval( ConstTrackInterval&& ) = default;
   ConstTrackInterval &operator=( ConstTrackInterval&& ) = default;

   double Start() const { return start; }
   double End() const { return end; }
   const TrackIntervalData *Extra() const { return pExtra.get(); }

private:
   double start, end;
protected:
   // TODO C++17: use std::any instead
   std::unique_ptr< TrackIntervalData > pExtra;
};

//! A start and an end time, and mutative access to optional extra information
/*! @invariant `Start() <= End()` */
class TrackInterval : public ConstTrackInterval {
public:
   using ConstTrackInterval::ConstTrackInterval;

   TrackInterval(TrackInterval&&) = default;
   TrackInterval &operator= (TrackInterval&&) = default;

   TrackIntervalData *Extra() const { return pExtra.get(); }
};

//! Template generated base class for Track lets it host opaque UI related objects
using AttachedTrackObjects = ClientData::Site<
   Track, TrackAttachment, ClientData::ShallowCopying, std::shared_ptr
>;

//! Abstract base class for an object holding data associated with points on a time axis
class TRACK_API Track /* not final */
   : public XMLTagHandler
   , public AttachedTrackObjects
   , public std::enable_shared_from_this<Track> // see SharedPointer()
{
protected:
   //! Empty argument passed to some public constructors
   /*!
    Passed to function templates like make_shared, which don't need to be
    friends; but construction of the argument is controlled by the class
    */
   struct ProtectedCreationArg{};
public:

   //! For two tracks describes the type of the linkage
   enum class LinkType : int {
       None = 0, //< No linkage
       Group = 2, //< Tracks are grouped together
       Aligned, //< Tracks are grouped and changes should be synchronized
   };

   struct ChannelGroupData;

   //! Hosting of objects attached by higher level code
   using ChannelGroupAttachments = ClientData::Site<
      ChannelGroupData, ClientData::Cloneable<>, ClientData::DeepCopying
   >;

   // Structure describing data common to channels of a group of tracks
   // Should be deep-copyable (think twice before adding shared pointers!)
   struct ChannelGroupData : ChannelGroupAttachments {
      LinkType mLinkType{ LinkType::None };
   };

private:

   friend class TrackList;

 private:
   TrackId mId; //!< Identifies the track only in-session, not persistently

   std::unique_ptr<ChannelGroupData> mpGroupData;

 protected:
   std::weak_ptr<TrackList> mList; //!< Back pointer to owning TrackList
   //! Holds iterator to self, so that TrackList::Find can be constant-time
   /*! mNode's pointer to std::list might not be this TrackList, if it's a pending update track */
   TrackNodePointer mNode{};
   int            mIndex; //!< 0-based position of this track in its TrackList
   wxString       mName;

 private:
   bool           mSelected;

 public:

   //! Alias for my base type
   using AttachedObjects = ::AttachedTrackObjects;

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
   inline auto SharedPointer() const ->
      std::enable_if_t<
         std::is_const_v<Subclass>, std::shared_ptr<Subclass>
      >
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
   std::shared_ptr<Track> SubstitutePendingChangedTrack();
   std::shared_ptr<const Track> SubstitutePendingChangedTrack() const;

   // If this track is a pending changed track, return the corresponding
   // original; else return this track
   std::shared_ptr<const Track> SubstituteOriginalTrack() const;

   using IntervalData = TrackIntervalData;
   using Interval = TrackInterval;
   using Intervals = std::vector< Interval >;
   using ConstInterval = ConstTrackInterval;
   using ConstIntervals = std::vector< ConstInterval >;

   //! Names of a track type for various purposes.
   /*! Some of the distinctions exist only for historical reasons. */
   struct TypeNames {
      wxString info; //!< short, like "wave", in macro output, not internationalized
      wxString property; //!< short, like "wave", as a Lisp symbol property, not internationalized
      TranslatableString name; //!< long, like "Wave Track"
   };
   struct TypeInfo {
      TypeNames names;
      bool concrete = false;
      const TypeInfo *pBaseInfo = nullptr;

      bool IsBaseOf(const TypeInfo &other) const
      {
         for (auto pInfo = &other;
              pInfo; pInfo = pInfo->pBaseInfo)
            if (this == pInfo)
               return true;
         return false;
      }
   };
   virtual const TypeInfo &GetTypeInfo() const = 0;
   static const TypeInfo &ClassTypeInfo();
   virtual const TypeNames &GetTypeNames() const
   { return GetTypeInfo().names; }

   //! Whether this track type implements cut-copy-paste; by default, true
   virtual bool SupportsBasicEditing() const;

   using Holder = std::shared_ptr<Track>;

   //! Find or create the destination track for a paste, maybe in a different project
   /*! @return A smart pointer to the track; its `use_count()` can tell whether it is new */
   virtual Holder PasteInto( AudacityProject & ) const = 0;

   //! Report times on the track where important intervals begin and end, for UI to snap to
   /*!
   Some intervals may be empty, and no ordering of the intervals is assumed.
   */
   virtual ConstIntervals GetIntervals() const;

   /*! @copydoc GetIntervals()
   This overload exposes the extra data of the intervals as non-const
    */
   virtual Intervals GetIntervals();

 public:
   mutable std::pair<int, int> vrulerSize;

   int GetIndex() const;
   void SetIndex(int index);

public:
   static void FinishCopy (const Track *n, Track *dest);

   //! Check consistency of channel groups, and maybe fix it
   /*!
    @param doFix whether to make any changes to correct inconsistencies
    @param completeList whether to assume that the TrackList containing this
    is completely loaded; if false, skip some of the checks
    @return true if no inconsistencies were found
    */
   virtual bool LinkConsistencyFix(bool doFix = true, bool completeList = true);

   //! Do the non-mutating part of consistency fix only and return status
   bool LinkConsistencyCheck(bool completeList)
   { return const_cast<Track*>(this)->LinkConsistencyFix(false, completeList); }

   bool HasOwner() const { return static_cast<bool>(GetOwner());}

   std::shared_ptr<TrackList> GetOwner() const { return mList.lock(); }

   LinkType GetLinkType() const noexcept;
   //! Returns true if the leader track has link type LinkType::Aligned
   bool IsAlignedWithLeader() const;

   ChannelGroupData &GetGroupData();
   const ChannelGroupData &GetGroupData() const;

protected:
   
   /*!
    @param completeList only influences debug build consistency checking
    */
   void SetLinkType(LinkType linkType, bool completeList = true);
   void SetChannel(ChannelType c) noexcept;

private:
   ChannelGroupData &MakeGroupData();
   /*!
    @param completeList only influences debug build consistency checking
    */
   void DoSetLinkType(LinkType linkType, bool completeList = true);

   Track* GetLinkedTrack() const;
   //! Returns true for leaders of multichannel groups
   bool HasLinkedTrack() const noexcept;
   
   //! Retrieve mNode with debug checks
   TrackNodePointer GetNode() const;
   //! Update mNode when Track is added to TrackList, or removed from it
   void SetOwner
      (const std::weak_ptr<TrackList> &list, TrackNodePointer node);

 // Keep in Track

 protected:
   ChannelType         mChannel;
   double              mOffset;

 public:

   Track();
   Track(const Track &orig, ProtectedCreationArg&&);
   Track& operator =(const Track &orig) = delete;

   virtual ~ Track();

   void Init(const Track &orig);

   // public nonvirtual duplication function that invokes Clone():
   virtual Holder Duplicate() const;

   // Called when this track is merged to stereo with another, and should
   // take on some parameters of its partner.
   virtual void Merge(const Track &orig);

   wxString GetName() const { return mName; }
   void SetName( const wxString &n );

   bool GetSelected() const { return mSelected; }

   virtual void SetSelected(bool s);

   // The argument tells whether the last undo history state should be
   // updated for the appearance change
   void EnsureVisible( bool modifyState = false );

public:

   virtual ChannelType GetChannel() const { return mChannel;}
   virtual double GetOffset() const = 0;

   void Offset(double t) { SetOffset(GetOffset() + t); }
   virtual void SetOffset (double o) { mOffset = o; }

   virtual void SetPan( float ){ ;}
   virtual void SetPanFromChannelType(){ ;};

   // Create a NEW track and modify this track
   // Return non-NULL or else throw
   // May assume precondition: t0 <= t1
   virtual Holder Cut(double WXUNUSED(t0), double WXUNUSED(t1)) = 0;

   // Create a NEW track and don't modify this track
   // Return non-NULL or else throw
   // Note that subclasses may want to distinguish tracks stored in a clipboard
   // from those stored in a project
   // May assume precondition: t0 <= t1
   // Should invoke Track::Init
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
   // Subclass responsibility implements only a part of Duplicate(), copying
   // the track data proper (not associated data such as for groups and views):
   virtual Holder Clone() const = 0;

   template<typename T>
      friend std::enable_if_t< std::is_pointer_v<T>, T >
         track_cast(Track *track);
   template<typename T>
      friend std::enable_if_t<
         std::is_pointer_v<T> &&
            std::is_const_v< std::remove_pointer_t< T > >,
         T
      >
         track_cast(const Track *track);

public:
   bool SameKindAs(const Track &track) const
      { return &GetTypeInfo() == &track.GetTypeInfo(); }

   //! Type of arguments passed as optional second parameter to TypeSwitch() cases
   template < typename R = void >
   using Continuation = std::function< R() >;
   //! Type of arguments passed as optional second parameter to TypeSwitch<void>() cases
   using Fallthrough = Continuation<>;
   
private:
   //! Variadic template implements metafunction with specializations, to dispatch Track::TypeSwitch
   template< typename ...Params >
   struct Executor{};

   //! Helper for recursive case of metafunction implementing Track::TypeSwitch
   /*! Mutually recursive (in compile time) with template Track::Executor. */
   struct Dispatcher {
      //! First, recursive case of metafunction, defers generation of operator ()
      template< typename Tag, typename R, typename ArgumentType,
                typename Function, typename ...Functions >
      struct inapplicable
      {
         using QualifiedTrackType =
            std::conditional_t< std::is_const_v<ArgumentType>,
               const Track, Track >;
   
         //! The template specialization to recur with
         using Tail = Executor< Tag, R, ArgumentType, Functions... >;
         //! Constant used in a compile-time check
         enum : unsigned { SetUsed = Tail::SetUsed << 1 };

         //! Ignore the first, inapplicable function and try others.
         R operator ()
            (QualifiedTrackType *pTrack,
             const Function &, const Functions &...functions) const
         { return Tail{}( pTrack, functions... ); }
      };

      //! Second, nonrecursive case of metafunction, generates operator () that calls function without fallthrough
      template< typename R, typename BaseClass, typename ArgumentType,
                typename Function >
      struct applicable1
      {
         using QualifiedTrackType =
            std::conditional_t< std::is_const_v<ArgumentType>,
               const Track, Track >;
         using QualifiedBaseClass =
            std::conditional_t< std::is_const_v<ArgumentType>,
               const BaseClass, BaseClass >;
   
         //! Constant used in a compile-time check
         enum : unsigned { SetUsed = 1u };

         //! Ignore the remaining functions and call the first only.
         R operator ()
            (QualifiedTrackType *pTrack, const Function &function, ...) const
         { return function( static_cast<QualifiedBaseClass *>(pTrack) ); }
      };

      //! Third, recursive case of metafunction, generates operator () that calls function with fallthrough
      template< typename Tag,
                typename R, typename BaseClass, typename ArgumentType,
                typename Function, typename ...Functions >
      struct applicable2
      {
         using QualifiedTrackType =
            std::conditional_t< std::is_const_v<ArgumentType>,
               const Track, Track >;
         using QualifiedBaseClass =
            std::conditional_t< std::is_const_v<ArgumentType>,
               const BaseClass, BaseClass >;
   
         //! The template specialization to recur with
         using Tail = Executor< Tag, R, ArgumentType, Functions... >;
         //! Constant used in a compile-time check
         enum : unsigned { SetUsed = (Tail::SetUsed << 1) | 1u };

         //! Call the first function, which may request dispatch to the further functions by invoking a continuation.
         R operator ()
            (QualifiedTrackType *pTrack, const Function &function,
             const Functions &...functions) const
         {
            auto continuation = Continuation<R>{ [&] {
               return Tail{}( pTrack, functions... );
            } };
            return function( static_cast<QualifiedBaseClass *>(pTrack),
               continuation );
         }
      };

      //! Variadic template implements metafunction with specializations, to choose among implementations of operator ()
      template< typename ... > struct Switch {};

      //! Base case, no more base classes of ArgumentType
      /*! Computes a type as the return type of undefined member test() */
      template< typename Tag, typename R, typename ArgumentType >
      struct Switch< Tag, R, ArgumentType, std::tuple<> >
      {
         //! No BaseClass of ArgumentType is acceptable to Function.
         template< typename Function, typename ...Functions >
            static auto test()
               -> inapplicable< Tag, R, ArgumentType, Function, Functions... >;
      };

      //! Recursive case, tries to match function with one base class of ArgumentType
      /*! Computes a type as the return type of undefined member test() */
      template< typename Tag, typename R, typename ArgumentType,
                typename BaseClass, typename ...BaseClasses >
      struct Switch< Tag, R, ArgumentType, std::tuple<BaseClass, BaseClasses...> >
      {
         using QualifiedBaseClass =
            std::conditional_t< std::is_const_v<ArgumentType>,
               const BaseClass, BaseClass >;

         //! Recur to this type to try the next base class
         using Retry =
            Switch< Tag, R, ArgumentType, std::tuple<BaseClasses...> >;

         //! Catch-all overload of undefined function used in decltype only
         /*! If ArgumentType is not compatible with BaseClass, or if
          Function does not accept QualifiedBaseClass*, try other BaseClasses. */
         template< typename Function, typename ...Functions >
            static auto test( const void * )
               -> decltype( Retry::template test< Function, Functions... >() );

         //! overload when upcast of ArgumentType* works, with sfinae'd return type
         /*! If BaseClass is a base of ArgumentType and Function can take a pointer to it,
           then overload resolution chooses this.
           If not, then the sfinae rule makes this overload unavailable. */
         template< typename Function, typename ...Functions >
            static auto test( std::true_type * )
               -> decltype(
                  (void) std::declval<Function>()
                     ( (QualifiedBaseClass*)nullptr ),
                  applicable1< R, BaseClass, ArgumentType, Function>{}
               );

         //! overload when upcast of ArgumentType* works, with sfinae'd return type
         /*! If BaseClass is a base of ArgumentType and Function can take a pointer to it,
           with a second argument for a continuation,
           then overload resolution chooses this.
           If not, then the sfinae rule makes this overload unavailable. */
         template< typename Function, typename ...Functions >
            static auto test( std::true_type * )
               -> decltype(
                  (void) std::declval<Function>()
                     ( (QualifiedBaseClass*)nullptr,
                       std::declval< Continuation<R> >() ),
                  applicable2< Tag, R, BaseClass, ArgumentType,
                               Function, Functions... >{}
               );

         //! Whether upcast of ArgumentType* to first BaseClass* works
         static constexpr bool Compatible =
            std::is_base_of_v<BaseClass, ArgumentType>;
         //! undefined function used in decltype only to compute a type, using other overloads
         template< typename Function, typename ...Functions >
            static auto test()
               -> decltype(
                  test< Function, Functions... >(
                     (std::integral_constant<bool, Compatible>*)nullptr) );
      };
   };

   //! Base case of metafunction implementing Track::TypeSwitch
   template< typename Tag, typename R, typename ArgumentType >
   struct Executor< Tag, R, ArgumentType >
   {
      using NominalType = ArgumentType;
      //! Constant used in a compile-time check
      enum : unsigned { SetUsed = 0 };
      //! No functions matched, so do nothing
      R operator () (const void *, ...)
      {
         if constexpr (std::is_void_v<R>)
            return;
         else
            return R{};
      }
   };

   //! Implements Track::TypeSwitch, its operator() invokes the first function that can accept ArgumentType*
   /*! Mutually recursive (in compile time) with template Track::Dispatcher. */
   template< typename Tag, typename R, typename ArgumentType,
             typename Function, typename ...Functions >
   struct Executor< Tag, R, ArgumentType, Function, Functions... >
      : decltype(
         Dispatcher::Switch< Tag, R, ArgumentType,
            typename CollectTrackTypes<Tag>::type >
               ::template test<Function, Functions... >())
   {
      using NominalType = ArgumentType;
   };

public:

   template<typename TrackType>
   static void checkTrackType()
   {
      static_assert(
         std::is_same_v<Track, TrackType> ||
         std::is_same_v<const Track, TrackType>, "Error" );
   }
   template<typename R, typename TrackType, typename... Functions>
   static R CallExecutor(R*, std::tuple<>*, TrackType&, const Functions&...)
   {
      // This overload is needed so that the other overload of CallExecutor
      // compiles, but it should never be reached at run-time, because an
      // Executor generated for (const) Track should have been the catch-all.
      wxASSERT(false);
      checkTrackType<TrackType>();
      if constexpr (std::is_void_v<R>)
         return;
      else
         return R{};
   }
   template<
      typename R, typename TrackType, typename... Functions,
      typename Executor, typename... Executors>
   static R CallExecutor(
      R*, std::tuple<Executor, Executors...>*, TrackType &track,
      const Functions &...functions)
   {
      checkTrackType<TrackType>();
      const auto &info = Executor::NominalType::ClassTypeInfo();
      // Dynamic type test of track
      // Assumes Executor classes are sequenced with more specific accepted
      // types earlier
      if ( info.IsBaseOf(track.GetTypeInfo()) )
         // Dispatch to an Executor that knows which of functions applies
         return Executor{}(&track, functions...);
      else
         // Recur, with fewer candidate Executors and all of functions
         return CallExecutor( (R*)nullptr,
            (std::tuple<Executors...>*)nullptr, track, functions...);
   }

   template<typename ...Executors>
   static constexpr unsigned UsedCases(std::tuple<Executors...>*)
   {
      return (Executors::SetUsed | ...); // My very first fold expression :-)
   }

   template<
      typename Tag,
      bool IsConst,
      typename R,
      typename ...TrackTypes,
      typename ...Functions
   >
   static R DoTypeSwitch(
      std::conditional_t<IsConst, const Track, Track> &track,
      std::tuple<TrackTypes...>*,
      const Functions &...functions )
   {
      // Generate Executor classes, for each of TrackTypes,
      // each zero-sized and with an operator () that calls the correct
      // one of functions, assuming the track is of the corresponding type
      using Executors = std::tuple< Executor<
         Tag, R,
         std::conditional_t<IsConst, const TrackTypes, TrackTypes>,
         Functions...
      >... >;
      // Don't even construct the tuple of zero-sized types, just point
      constexpr Executors *executors = nullptr;

      // Compile time reachability check of the given functions
      enum { All = sizeof...( functions ) };
      static_assert( (1u << All) - 1u == UsedCases(executors),
         "Uncallable case in Track::TypeSwitch");

      // Do dynamic dispatch to one of the Executors
      return CallExecutor((R *)nullptr, executors, track, functions...);
   }

   //! Use this function rather than testing track type explicitly and making down-casts.
   /*!
   A variadic function taking any number of function objects, each taking
   a pointer to Track or a subclass, maybe const-qualified, and maybe a
   second argument which is a fall-through continuation.
   
   Each of the function objects (and supplied continuations) returns R (or a type convertible to R).
   Calls the first in the sequence that accepts the actual type of the track.
   
   If no function accepts the track, do nothing and return R{}
   if R is not void.
   
   If one of the functions invokes the fall-through, then the next following
   applicable function is called.

   @tparam R Return type of this function and each function argument
   @tparam Functions callable types deduced from arguments
   @param functions typically lambdas, taking a pointer to a track subclass, and optionally a fall-through call-back
    */
   template<
      typename R = void,
      typename ...Functions
   >
   R TypeSwitch(const Functions &...functions)
   {
      struct Tag : TrackTypeCountTag {};
      // Collect all concrete and abstract track types known at compile time
      using TrackTypes = typename CollectTrackTypes<Tag>::type;
      TrackTypes *const trackTypes = nullptr;
      // Generate a function that dispatches dynamically on track type
      return DoTypeSwitch<Tag, false, R>(*this, trackTypes, functions...);
   }

   /*! @copydoc Track::TypeSwitch */
   /*! This is the overload for const tracks, only taking
   callable arguments that accept first arguments that are pointers to const. */
   template<
      typename R = void,
      typename ...Functions
   >
   R TypeSwitch(const Functions &...functions) const
   {
      struct Tag : TrackTypeCountTag {};
      // Collect all concrete and abstract track types known at compile time
      using TrackTypes = typename CollectTrackTypes<Tag>::type;
      TrackTypes *const trackTypes = nullptr;
      // Generate a function that dispatches dynamically on track type
      return DoTypeSwitch<Tag, true, R>(*this, trackTypes, functions...);
   }

   // XMLTagHandler callback methods -- NEW virtual for writing
   virtual void WriteXML(XMLWriter &xmlFile) const = 0;

   // Returns true if an error was encountered while trying to
   // open the track from XML
   virtual bool GetErrorOpening() { return false; }

   virtual double GetStartTime() const = 0;
   virtual double GetEndTime() const = 0;

   // Send a notification to subscribers when state of the track changes
   // To do: define values for the argument to distinguish different parts
   // of the state
   void Notify( int code = -1 );

   // An always-true predicate useful for defining iterators
   bool Any() const;

   // Frequently useful operands for + and -
   bool IsSelected() const;
   bool IsLeader() const;
   bool IsSelectedLeader() const;

   // Cause this track and following ones in its TrackList to adjust
   void AdjustPositions();

   // Serialize, not with tags of its own, but as attributes within a tag.
   void WriteCommonXMLAttributes(
      XMLWriter &xmlFile, bool includeNameAndSelected = true) const;

   // Return true iff the attribute is recognized.
   bool HandleCommonXMLAttribute(const std::string_view& attr, const XMLAttributeValueView& valueView);
};

ENUMERATE_TRACK_TYPE(Track);

//! Track subclass holding data representing sound (as notes, or samples, or ...)
class TRACK_API AudioTrack /* not final */ : public Track
{
public:
   AudioTrack();
   AudioTrack(const Track &orig, ProtectedCreationArg &&a);

   static const TypeInfo &ClassTypeInfo();

   // Serialize, not with tags of its own, but as attributes within a tag.
   void WriteXMLAttributes(XMLWriter &WXUNUSED(xmlFile)) const {}

   // Return true iff the attribute is recognized.
   bool HandleXMLAttribute(const std::string_view & /*attr*/, const XMLAttributeValueView &/*value*/)
   { return false; }
};

ENUMERATE_TRACK_TYPE(AudioTrack);

//! AudioTrack subclass that can also be audibly replayed by the program
class TRACK_API PlayableTrack /* not final */ : public AudioTrack
{
public:
   PlayableTrack();
   PlayableTrack(const PlayableTrack &orig, ProtectedCreationArg&&);

   static const TypeInfo &ClassTypeInfo();

   bool GetMute    () const { return DoGetMute();     }
   bool GetSolo    () const { return DoGetSolo();     }
   bool GetNotMute () const { return !DoGetMute();     }
   bool GetNotSolo () const { return !DoGetSolo();     }
   void SetMute    (bool m);
   void SetSolo    (bool s);

   void Init( const PlayableTrack &init );
   void Merge( const Track &init ) override;

   // Serialize, not with tags of its own, but as attributes within a tag.
   void WriteXMLAttributes(XMLWriter &xmlFile) const;

   // Return true iff the attribute is recognized.
   bool HandleXMLAttribute(const std::string_view &attr, const XMLAttributeValueView &value);

protected:
   // These just abbreviate load and store with relaxed memory ordering
   bool DoGetMute() const;
   void DoSetMute(bool value);
   bool DoGetSolo() const;
   void DoSetSolo(bool value);

   //! Atomic because it may be read by worker threads in playback
   std::atomic<bool>  mMute { false };
   //! Atomic because it may be read by worker threads in playback
   std::atomic<bool>  mSolo { false };
};

ENUMERATE_TRACK_TYPE(PlayableTrack);


//! Encapsulate the checked down-casting of track pointers
/*! Eliminates possibility of error -- and not quietly casting away const
 
Typical usage:
```
if (auto wt = track_cast<const WaveTrack*>(track)) { ... }
```
 */
template<typename T>
   inline std::enable_if_t< std::is_pointer_v<T>, T >
      track_cast(Track *track)
{
   using BareType = std::remove_pointer_t< T >;
   if (track &&
       BareType::ClassTypeInfo().IsBaseOf(track->GetTypeInfo() ))
      return reinterpret_cast<T>(track);
   else
      return nullptr;
}

/*! @copydoc track_cast(Track*)
This overload for const pointers can cast only to other const pointer types. */
template<typename T>
   inline std::enable_if_t<
      std::is_pointer_v<T> && std::is_const_v< std::remove_pointer_t< T > >,
      T
   >
      track_cast(const Track *track)
{
   using BareType = std::remove_pointer_t< T >;
   if (track &&
       BareType::ClassTypeInfo().IsBaseOf(track->GetTypeInfo() ))
      return reinterpret_cast<T>(track);
   else
      return nullptr;
}

template < typename TrackType > struct TrackIterRange;

//! Iterator over only members of a TrackList of the specified subtype, optionally filtered by a predicate; past-end value dereferenceable, to nullptr
/*! Does not suffer invalidation when an underlying std::list iterator is deleted, provided that is not
    equal to its current position or to the beginning or end iterator.
 
    The filtering predicate is tested only when the iterator is constructed or advanced.
 
    @tparam TrackType Track or a subclass, maybe const-qualified
*/
template <
   typename TrackType
> class TrackIter
   : public ValueIterator< TrackType *, std::bidirectional_iterator_tag >
{
public:
   //! Type of predicate taking pointer to const TrackType
   using FunctionType = std::function< bool(
      std::add_pointer_t< std::add_const_t< std::remove_pointer_t<TrackType> > >
   ) >;

   //! Constructor, usually not called directly except by methods of TrackList
   TrackIter(
         TrackNodePointer begin, //!< Remember lower bound
         TrackNodePointer iter, //!< The actual pointer
         TrackNodePointer end, //!< Remember upper bound
         FunctionType pred = {} //!< %Optional filter
   )
      : mBegin( begin ), mIter( iter ), mEnd( end )
      , mPred( std::move(pred) )
   {
      // Establish the class invariant
      if (this->mIter != this->mEnd && !this->valid())
         this->operator ++ ();
   }

   //! Return an iterator that replaces the predicate
   /*! Advance to the first position at or after the old position that satisfies the new predicate,
   or to the end */
   template < typename Predicate2 >
   TrackIter Filter( const Predicate2 &pred2 ) const
   {
      return { this->mBegin, this->mIter, this->mEnd, pred2 };
   }

   //! Return an iterator for a subclass of TrackType (and not removing const) with same predicate
   /*! Advance to the first position at or after the old position that
   satisfies the type constraint, or to the end */
   template < typename TrackType2 >
      auto Filter() const
         -> std::enable_if_t<
            std::is_base_of_v< TrackType, TrackType2 > &&
               (!std::is_const_v<TrackType> ||
                 std::is_const_v<TrackType2>),
            TrackIter< TrackType2 >
         >
   {
      return { this->mBegin, this->mIter, this->mEnd, this->mPred };
   }

   const FunctionType &GetPredicate() const
   { return this->mPred; }

   //! Safe to call even when at the end
   /*! In that case *this remains unchanged. */
   TrackIter &operator ++ ()
   {
      // Maintain the class invariant
      if (this->mIter != this->mEnd) do
         ++this->mIter.first;
      while (this->mIter != this->mEnd && !this->valid() );
      return *this;
   }

   //! @copydoc operator++
   TrackIter operator ++ (int)
   {
      TrackIter result { *this };
      this-> operator ++ ();
      return result;
   }

   //! Safe to call even when at the beginning
   /*! In that case *this wraps to the end. */
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

   //! @copydoc operator--
   TrackIter operator -- (int)
   {
      TrackIter result { *this };
      this->operator -- ();
      return result;
   }

   //! Safe to call even when at the end
   /*! In that case you get nullptr. */
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

   //! This might be called operator + , but it's not constant-time as with a random access iterator
   TrackIter advance(
      long amount //!< may be negative
   ) const
   {
      auto copy = *this;
      std::advance( copy, amount );
      return copy;
   }

   //! Compares only current positions, assuming same beginnings and ends
   friend inline bool operator == (TrackIter a, TrackIter b)
   {
      // Assume the predicate is not stateful.  Just compare the iterators.
      return
         a.mIter == b.mIter
         // Assume this too:
         // && a.mBegin == b.mBegin && a.mEnd == b.mEnd
      ;
   }

   //! @copydoc operator==
   friend inline bool operator != (TrackIter a, TrackIter b)
   {
      return !(a == b);
   }

private:
   /*! @pre underlying iterators are still valid, and mPred, if not null, is well behaved */
   /*! @invariant mIter == mEnd, or else, mIter != mEnd,
   and **mIter is of the appropriate subclass, and mPred is null or mPred(&**mIter) is true. */

   //! Test satisfaction of the invariant, while initializing, incrementing, or decrementing
   bool valid() const
   {
      // assume mIter != mEnd
      const auto pTrack = track_cast< TrackType * >( &**this->mIter.first );
      if (!pTrack)
         return false;
      return !this->mPred || this->mPred( pTrack );
   }

   //! This friendship is needed in TrackIterRange::StartingWith and TrackIterRange::EndingAfter()
   friend TrackIterRange< TrackType >;

   TrackNodePointer
      mBegin, //!< Allows end of reverse iteration to be detected without comparison to other TrackIter
      mIter, //!< Current position
      mEnd; //!< Allows end of iteration to be detected without comparison to other TrackIter
   FunctionType mPred; //!< %Optional filter
};

//! Range between two @ref TrackIter "TrackIters", usable in range-for statements, and with Visit member functions
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
         { newBegin.mIter, this->second.mIter, this->second.mEnd,
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

   //! See Track::TypeSwitch
   template< typename ...Functions >
   void Visit(const Functions &...functions)
   {
      for (auto track : *this)
         track->TypeSwitch(functions...);
   }

   //! See Track::TypeSwitch
   /*! Visit until flag is false, or no more tracks */
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


//! Notification of changes in individual tracks of TrackList, or of TrackList's composition
struct TrackListEvent
{
   enum Type {
      //! Posted when the set of selected tracks changes.
      SELECTION_CHANGE,

      //! Posted when certain fields of a track change.
      TRACK_DATA_CHANGE,

      //! Posted when a track needs to be scrolled into view.
      TRACK_REQUEST_VISIBLE,

      //! Posted when tracks are reordered but otherwise unchanged.
      /*! mpTrack points to the moved track that is earliest in the New ordering. */
      PERMUTED,

      //! Posted when some track changed its height.
      RESIZING,

      //! Posted when a track has been added to a tracklist.  Also posted when one track replaces another
      ADDITION,

      //! Posted when a track has been deleted from a tracklist. Also posted when one track replaces another
      /*! mpTrack points to the first track after the deletion, if there is one. */
      DELETION,
   };

   TrackListEvent( Type type,
      const std::weak_ptr<Track> &pTrack = {}, int extra = -1)
      : mType{ type }
      , mpTrack{ pTrack }
      , mExtra{ extra }
   {}

   TrackListEvent( const TrackListEvent& ) = default;

   const Type mType;
   const std::weak_ptr<Track> mpTrack;
   const int mExtra;
};

/*! @brief A flat linked list of tracks supporting Add,  Remove,
 * Clear, and Contains, serialization of the list of tracks, event notifications
 */
class TRACK_API TrackList final
   : public Observer::Publisher<TrackListEvent>
   , public ListOfTracks
   , public std::enable_shared_from_this<TrackList>
   , public ClientData::Base
{
   // privatize this, make you use Add instead:
   using ListOfTracks::push_back;

   // privatize this, make you use Swap instead:
   using ListOfTracks::swap;

   // Disallow copy
   TrackList(const TrackList &that) = delete;
   TrackList &operator= (const TrackList&) = delete;

   // No need for move, disallow it
   TrackList(TrackList &&that) = delete;
   TrackList& operator= (TrackList&&) = delete;

   void clear() = delete;

 public:
   static TrackList &Get( AudacityProject &project );
   static const TrackList &Get( const AudacityProject &project );
 
   // Create an empty TrackList
   // Don't call directly -- use Create() instead
   explicit TrackList( AudacityProject *pOwner );

   // Create an empty TrackList
   static std::shared_ptr<TrackList> Create( AudacityProject *pOwner );

   // Move is defined in terms of Swap
   void Swap(TrackList &that);

   // Destructor
   virtual ~TrackList();

   // Find the owning project, which may be null
   AudacityProject *GetOwner() { return mOwner; }
   const AudacityProject *GetOwner() const { return mOwner; }

   /**
    * \brief Returns string that contains baseTrackName,
    * but is guaranteed to be unique among other tracks in that list.
    * \param baseTrackName String to be put into the template
    * \return Formatted string: "[baseTrackName] [N]"
    */
   wxString MakeUniqueTrackName(const wxString& baseTrackName) const;

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

   //! Turn a pointer into a TrackIter (constant time); get end iterator if this does not own the track
   template < typename TrackType = Track >
      auto Find(Track *pTrack)
         -> TrackIter< TrackType >
   {
      if (!pTrack || pTrack->GetOwner().get() != this)
         return EndIterator<TrackType>();
      else
         return MakeTrackIterator<TrackType>( pTrack->GetNode() );
   }

   //! @copydoc Find
   /*! const overload will only produce iterators over const TrackType */
   template < typename TrackType = const Track >
      auto Find(const Track *pTrack) const
         -> std::enable_if_t< std::is_const_v<TrackType>,
            TrackIter< TrackType >
         >
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
         -> std::enable_if_t< std::is_const_v<TrackType>,
            TrackIterRange< TrackType >
         >
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
         -> std::enable_if_t< std::is_const_v<TrackType>,
            TrackIterRange< TrackType >
         >
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
         -> std::enable_if_t< std::is_const_v<TrackType>,
            TrackIterRange< TrackType >
         >
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
         -> std::enable_if_t< std::is_const_v<TrackType>,
            TrackIterRange< TrackType >
         >
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

   //! If the given track is one of a pair of channels, swap them
   /*! @return success */
   static bool SwapChannels(Track &track);

   friend class Track;

   //! For use in sorting:  assume each iterator points into this list, no duplications
   void Permute(const std::vector<TrackNodePointer> &permutation);

   Track *FindById( TrackId id );

   /// Add a Track, giving it a fresh id
   template<typename TrackKind>
      TrackKind *AddToHead( const std::shared_ptr< TrackKind > &t )
         { return static_cast< TrackKind* >( DoAddToHead( t ) ); }

   template<typename TrackKind>
      TrackKind *Add( const std::shared_ptr< TrackKind > &t )
         { return static_cast< TrackKind* >( DoAdd( t ) ); }
   
   //! Removes linkage if track belongs to a group
   void UnlinkChannels(Track& track);
   /** \brief Converts channels to a multichannel track. 
   * @param first and the following must be in this list. Tracks should
   * not be a part of another group (not linked)
   * @param nChannels number of channels, for now only 2 channels supported
   * @param aligned if true, the link type will be set to Track::LinkType::Aligned,
   * or Track::LinkType::Group otherwise
   * @returns true on success, false if some prerequisites do not met
   */
   bool MakeMultiChannelTrack(Track& first, int nChannels, bool aligned);

   /// Replace first track with second track, give back a holder
   /// Give the replacement the same id as the replaced
   ListOfTracks::value_type Replace(
      Track * t, const ListOfTracks::value_type &with);

   //! Remove the Track and return an iterator to what followed it.
   TrackNodePointer Remove(Track *t);

   /// Make the list empty
   void Clear(bool sendEvent = true);

   bool CanMoveUp(Track * t) const;
   bool CanMoveDown(Track * t) const;

   bool MoveUp(Track * t);
   bool MoveDown(Track * t);
   bool Move(Track * t, bool up) { return up ? MoveUp(t) : MoveDown(t); }

   //! Mainly a test function. Uses a linear search, so could be slow.
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
         -> std::enable_if_t< std::is_const_v<TrackType>,
            TrackIterRange< TrackType >
         >
   {
      auto b = const_cast<TrackList*>(this)->getBegin();
      auto e = const_cast<TrackList*>(this)->getEnd();
      return { { b, b, e, pred }, { b, e, e, pred } };
   }

   Track *GetPrev(Track * t, bool linked = false) const;
   Track *GetNext(Track * t, bool linked = false) const;
   
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

   //! Move an iterator to the next node, if any; else stay at end
   TrackNodePointer getNext(TrackNodePointer p) const
   {
      if ( isNull(p) )
         return p;
      auto q = p;
      ++q.first;
      return q;
   }

   //! Move an iterator to the previous node, if any; else wrap to end
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
   void QueueEvent(TrackListEvent event);
   void SelectionEvent( const std::shared_ptr<Track> &pTrack );
   void PermutationEvent(TrackNodePointer node);
   void DataEvent( const std::shared_ptr<Track> &pTrack, int code );
   void EnsureVisibleEvent(
      const std::shared_ptr<Track> &pTrack, bool modifyState );
   void DeletionEvent(TrackNodePointer node = {});
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
   AudacityProject *mOwner;

   //! Shadow tracks holding append-recording in progress; need to put them into a list so that GetLink() works
   /*! Beware, they are in a disjoint iteration sequence from ordinary tracks */
   ListOfTracks mPendingUpdates;
   //! This is in correspondence with mPendingUpdates
   std::vector< Updater > mUpdaters;
};

#endif
