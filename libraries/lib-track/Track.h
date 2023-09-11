/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file Track.h
  @brief declares abstract base class Track, TrackList, and iterators over TrackList

  Dominic Mazzoni

**********************************************************************/
#ifndef __AUDACITY_TRACK__
#define __AUDACITY_TRACK__

#include <algorithm>
#include <atomic>
#include <utility>
#include <list>
#include <optional>
#include <functional>
#include <wx/longlong.h>

#include "Channel.h"
#include "Observer.h"
// TrackAttachment needs to be a complete type for the Windows build, though
// not the others, so there is a nested include here:
#include "TrackAttachment.h"
#include "TypeEnumerator.h"
#include "TypeSwitch.h"
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
using TrackListHolder = std::shared_ptr<TrackList>;
struct UndoStackElem;

using ListOfTracks = std::list< std::shared_ptr< Track > >;

//! Pairs a std::list iterator and a pointer to a list, for comparison purposes
/*! Compare owning lists first, and only if same, then the iterators;
 else MSVC debug runtime complains. */
using TrackNodePointer =
std::pair< ListOfTracks::iterator, ListOfTracks* >;

using ProgressReporter = std::function<void(double)>;

inline bool operator == (const TrackNodePointer &a, const TrackNodePointer &b)
{ return a.second == b.second && a.first == b.first; }

inline bool operator != (const TrackNodePointer &a, const TrackNodePointer &b)
{ return !(a == b); }

//! Empty class which will have subclasses
BEGIN_TYPE_ENUMERATION(TrackTypeTag)

//! This macro should be called immediately after the definition of each Track
//! subclass
/*! It must occur at file scope, not within any other namespace */
#define ENUMERATE_TRACK_TYPE(T) ENUMERATE_TYPE(TrackTypeTag, T)

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

//! Template generated base class for Track lets it host opaque UI related objects
using AttachedTrackObjects = ClientData::Site<
   Track, TrackAttachment, ClientData::ShallowCopying, std::shared_ptr
>;

//! Abstract base class for an object holding data associated with points on a time axis
class TRACK_API Track /* not final */
   : public XMLTagHandler
   , public AttachedTrackObjects
   , public std::enable_shared_from_this<Track> // see SharedPointer()
   , public ChannelGroup
{
protected:
   //! Empty argument passed to some public constructors
   /*!
    Passed to function templates like make_shared, which don't need to be
    friends; but construction of the argument is controlled by the class
    */
   struct ProtectedCreationArg{};

private:

   friend class TrackList;

 private:
   TrackId mId; //!< Identifies the track only in-session, not persistently

 protected:
   std::weak_ptr<TrackList> mList; //!< Back pointer to owning TrackList
   //! Holds iterator to self, so that TrackList::Find can be constant-time
   /*! mNode's pointer to std::list might not be this TrackList, if it's a pending update track */
   TrackNodePointer mNode{};
   int            mIndex; //!< 0-based position of this track in its TrackList

 public:

   //! Alias for my base type
   using AttachedObjects = ::AttachedTrackObjects;

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

   //! Find or create the destination track for a paste, maybe in a different
   //! project
   /*!
    @pre `IsLeader()`
    @param list to which any newly created tracks are added; but left unchanged
       if an existing track is found in the project instead
    @return A smart pointer to a leader track
    */
   virtual Holder PasteInto(AudacityProject &project, TrackList &list)
      const = 0;

public:
   //! Check consistency of channel groups, and maybe fix it
   /*!
    @param doFix whether to make any changes to correct inconsistencies

    @pre `!doFix || IsLeader()`
    @return true if no inconsistencies were found
    */
   virtual bool LinkConsistencyFix(bool doFix = true);

   //! Do the non-mutating part of consistency fix only and return status
   bool LinkConsistencyCheck()
   { return const_cast<Track*>(this)->LinkConsistencyFix(false); }

   bool HasOwner() const { return static_cast<bool>(GetOwner());}

   std::shared_ptr<TrackList> GetOwner() const { return mList.lock(); }
   inline TrackList* GetHolder() const;

   LinkType GetLinkType() const noexcept;

   ChannelGroupData &GetGroupData();
   //! May make group data on demand, but consider that logically const
   const ChannelGroupData &GetGroupData() const;

protected:

   /*!
    @param completeList only influences debug build consistency checking
    */
   void SetLinkType(LinkType linkType, bool completeList = true);

private:
   int GetIndex() const;
   void SetIndex(int index);

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

 public:

   Track();
   Track(const Track &orig, ProtectedCreationArg&&);
   Track& operator =(const Track &orig) = delete;

   virtual ~ Track();

   void Init(const Track &orig);

   //! public nonvirtual duplication function that invokes Clone()
   /*!
    @pre `IsLeader()`
    @post result: `NChannels() == result->NChannels()`
    */
   virtual TrackListHolder Duplicate() const;

   //! Name is always the same for all channels of a group
   const wxString &GetName() const;
   void SetName( const wxString &n );

   //! Selectedness is always the same for all channels of a group
   bool GetSelected() const;
   virtual void SetSelected(bool s);

   /*!
    The owning TrackList emits a TRACK_REQUEST_VISIBLE event with the leader of
    this track
    The argument tells whether the last undo history state should be
    updated for the appearance change
    */
   void EnsureVisible(bool modifyState = false);

public:

   //! method to set project tempo on track
   /*!
    @pre `IsLeader()`
    */
   void OnProjectTempoChange(double newTempo);

   //! Create tracks and modify this track
   /*!
    @return non-NULL or else throw
    May assume precondition: t0 <= t1
    @pre `IsLeader()`
    @post result: `result->NChannels() == NChannels()`
    */
   virtual TrackListHolder Cut(double t0, double t1) = 0;

   //! Create new tracks and don't modify this track
   /*!
    @return non-NULL or else throw
    Note that subclasses may want to distinguish tracks stored in a clipboard
    from those stored in a project
    May assume precondition: t0 <= t1
    Should invoke Track::Init
    @pre `IsLeader`
    @post result: `result->NChannels() == NChannels()`
   */
   virtual TrackListHolder Copy(double t0, double t1, bool forClipboard = true)
      const = 0;

   /*!
    May assume precondition: t0 <= t1
    @pre `IsLeader()`
    */
   virtual void Clear(double t0, double t1) = 0;

   //! Weak precondition allows overrides to replicate one channel into many
   /*!
    @pre `IsLeader()`
    @pre `SameKindAs(src)`
    @pre `src.NChannels() == 1 || src.NChannels() == NChannels()`
    */
   virtual void Paste(double t, const Track &src) = 0;

   /*!
    Non-virtual overload that passes the first track of a given list
    @pre `IsLeader()`
    @pre `SameKindAs(**src.begin()).NChannels()`
    @pre `NChannels == (**src.begin()).NChannels()`
    */
   void Paste(double t, const TrackList &src);

   //! This can be used to adjust a sync-lock selected track when the selection
   //! is replaced by one of a different length.
   /*!
    @pre `IsLeader()`
    */
   virtual void SyncLockAdjust(double oldT1, double newT1);

   // May assume precondition: t0 <= t1
   /*!
    @pre `IsLeader()`
    */
   virtual void
   Silence(double t0, double t1, ProgressReporter reportProgress = {}) = 0;

   /*!
    May assume precondition: t0 <= t1
    @pre `IsLeader()`
    */
   virtual void InsertSilence(double t, double len) = 0;

private:
   //! Subclass responsibility implements only a part of Duplicate(), copying
   //! the track data proper (not associated data such as for groups and views)
   /*!
    @param unstretchInterval If set, this time interval's stretching must be applied.
    @pre `!unstretchInterval.has_value() ||
       unstretchInterval->first < unstretchInterval->second`
    @pre `IsLeader()`
    @post result: `NChannels() == result->NChannels()`
    */
   virtual TrackListHolder Clone() const = 0;

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

   /*!
    Do a TypeSwitch on this track, among all subtypes enumerated up to the point
    of the call
    */
   template<typename R = void, typename ...Functions>
   R TypeSwitch(const Functions &...functions)
   {
      struct Here : TrackTypeTag {};
      // List more derived classes later
      using TrackTypes =
         typename TypeEnumerator::CollectTypes<TrackTypeTag, Here>::type;
      return TypeSwitch::VDispatch<R, TrackTypes>(*this, functions...);
   }

   /*! @copydoc Track::TypeSwitch */
   template<typename R = void, typename ...Functions>
   R TypeSwitch(const Functions &...functions) const
   {
      struct Here : TrackTypeTag {};
      // List more derived classes later
      using namespace TypeList;
      using TrackTypes = Map_t<Fn<std::add_const_t>,
         typename TypeEnumerator::CollectTypes<TrackTypeTag, Here>::type>;
      return TypeSwitch::VDispatch<R, TrackTypes>(*this, functions...);
   }

   // XMLTagHandler callback methods -- NEW virtual for writing
   virtual void WriteXML(XMLWriter &xmlFile) const = 0;

   //! Returns nonempty if an error was encountered while trying to
   //! open the track from XML
   /*!
    May assume consistency of stereo channel grouping and examine other channels
    @pre `IsLeader()`
    */
   virtual std::optional<TranslatableString> GetErrorOpening() const;

   // Send a notification to subscribers when state of the track changes
   // To do: define values for the argument to distinguish different parts
   // of the state
   void Notify(bool allChannels, int code = -1);

   // An always-true predicate useful for defining iterators
   bool Any() const;

   // Frequently useful operands for + and -
   bool IsSelected() const;
   bool IsLeader() const override;
   bool IsSelectedLeader() const;

   // Cause this track and following ones in its TrackList to adjust
   void AdjustPositions();

   // Serialize, not with tags of its own, but as attributes within a tag.
   void WriteCommonXMLAttributes(
      XMLWriter &xmlFile, bool includeNameAndSelected = true) const;

   // Return true iff the attribute is recognized.
   bool HandleCommonXMLAttribute(const std::string_view& attr, const XMLAttributeValueView& valueView);

protected:
   const std::optional<double>& GetProjectTempo() const;

   /*!
    @pre `IsLeader()`
    */
   virtual void DoOnProjectTempoChange(
      const std::optional<double>& oldTempo, double newTempo) = 0;
};

ENUMERATE_TRACK_TYPE(Track);

//! Generates overrides of channel-related functions
template<typename Base = Track>
class UniqueChannelTrack
   : public Base
   , public Channel
{
public:
   using Base::Base;
   size_t NChannels() const override { return 1; }
   std::shared_ptr<Channel> DoGetChannel(size_t iChannel) override
   {
      if (iChannel == 0) {
         // Use aliasing constructor of std::shared_ptr
         Channel *alias = this;
         return { this->shared_from_this(), alias };
      }
      return {};
   }
protected:
   ChannelGroup &DoGetChannelGroup() const override {
      const Track &track = *this;
      return const_cast<Track&>(track);
   }
};

//! Holds multiple objects as a single attachment to Track
class TRACK_API ChannelAttachmentsBase : public TrackAttachment
{
public:
   using Factory =
      std::function<std::shared_ptr<TrackAttachment>(Track &, size_t)>;

   ChannelAttachmentsBase(Track &track, Factory factory);
   ~ChannelAttachmentsBase() override;

   // Override all the TrackAttachment virtuals and pass through to each
   void CopyTo(Track &track) const override;
   void Reparent(const std::shared_ptr<Track> &parent) override;
   void WriteXMLAttributes(XMLWriter &writer) const override;
   bool HandleXMLAttribute(
      const std::string_view& attr, const XMLAttributeValueView& valueView)
   override;

protected:
   /*!
    @pre `iChannel < track.NChannels()`
    */
   static TrackAttachment &Get(
      const AttachedTrackObjects::RegisteredFactory &key,
      Track &track, size_t iChannel);
   /*!
    @pre `!pTrack || iChannel < pTrack->NChannels()`
    */
   static TrackAttachment *Find(
      const AttachedTrackObjects::RegisteredFactory &key,
      Track *pTrack, size_t iChannel);

private:
   const Factory mFactory;
   std::vector<std::shared_ptr<TrackAttachment>> mAttachments;
};

//! Holds multiple objects of the parameter type as a single attachment to Track
template<typename Attachment>
class ChannelAttachments : public ChannelAttachmentsBase
{
   static_assert(std::is_base_of_v<TrackAttachment, Attachment>);
public:
   ~ChannelAttachments() override = default;

   /*!
    @pre `iChannel < track.NChannels()`
    */
   static Attachment &Get(
      const AttachedTrackObjects::RegisteredFactory &key,
      Track &track, size_t iChannel)
   {
      return static_cast<Attachment&>(
         ChannelAttachmentsBase::Get(key, track, iChannel));
   }
   /*!
    @pre `!pTrack || iChannel < pTrack->NChannels()`
    */
   static Attachment *Find(
      const AttachedTrackObjects::RegisteredFactory &key,
      Track *pTrack, size_t iChannel)
   {
      return static_cast<Attachment*>(
         ChannelAttachmentsBase::Find(key, pTrack, iChannel));
   }

   //! Type-erasing constructor
   /*!
    @tparam F returns a shared pointer to Attachment (or some subtype of it)

    @pre `f` never returns null

    `f` may assume the precondition that the given channel index is less than
    the given track's number of channels
    */
   template<typename F,
      typename sfinae = std::enable_if_t<std::is_convertible_v<
         std::invoke_result_t<F, Track&, size_t>, std::shared_ptr<Attachment>
      >>
   >
   explicit ChannelAttachments(Track &track, F &&f)
      : ChannelAttachmentsBase{ track, std::forward<F>(f) }
   {}
};

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

      //! Posted when a track needs to be scrolled into view; leader track only
      TRACK_REQUEST_VISIBLE,

      //! Posted when tracks are reordered but otherwise unchanged.
      /*! mpTrack points to the moved track that is earliest in the New ordering. */
      PERMUTED,

      //! Posted when some track changed its height.
      RESIZING,

      //! Posted when a track has been added to a tracklist.  Also posted when one track replaces another
      ADDITION,

      //! Posted when a track has been deleted from a tracklist. Also posted when one track replaces another
      /*! mpTrack points to the removed track. It is expected, that track is valid during the event.
       *! mExtra is 1 if the track is being replaced by another track, 0 otherwise.
       */
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
   , private ListOfTracks
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

   static TrackList *FindUndoTracks(const UndoStackElem &state);

   // Create an empty TrackList
   // Don't call directly -- use Create() instead
   explicit TrackList( AudacityProject *pOwner );

   // Create an empty TrackList
   static TrackListHolder Create(AudacityProject *pOwner);

   /*!
    @pre `!GetOwner() && !that.GetOwner()`
    */
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

   // Reverse iteration
   using reverse_iterator = std::reverse_iterator<iterator>;
   using const_reverse_iterator = std::reverse_iterator<const_iterator>;
   reverse_iterator rbegin() { return Any().rbegin(); }
   reverse_iterator rend() { return Any().rend(); }
   const_reverse_iterator rbegin() const { return Any().rbegin(); }
   const_reverse_iterator rend() const { return Any().rend(); }
   const_reverse_iterator crbegin() const { return rbegin(); }
   const_reverse_iterator crend() const { return rend(); }

   //! Turn a pointer into a TrackIter (constant time);
   //! get end iterator if this does not own the track
   TrackIter<Track> DoFind(Track *pTrack);

   // If the track is not an audio track, or not one of a group of channels,
   // return the track itself; else return the first channel of its group --
   // in either case as an iterator that will only visit other leader tracks.
   // (Generalizing away from the assumption of at most stereo)
   TrackIter<Track> Find(Track *pTrack);

   TrackIter<const Track> Find(const Track *pTrack) const
   {
      return const_cast<TrackList*>(this)->
         Find(const_cast<Track*>(pTrack)).Filter<const Track>();
   }


private:
   //! This private function still iterates channels not tracks
   iterator Begin() { return Tracks<Track>().begin(); }
   //! This private function still iterates channels not tracks
   iterator End() { return Tracks<Track>().end(); }

   //! This private function still iterates channels not tracks
   const_iterator Begin() const { return Tracks<const Track>().begin(); }
   //! This private function still iterates channels not tracks
   const_iterator End() const { return Tracks<const Track>().end(); }

public:
   template < typename TrackType = Track >
      auto Any()
         -> TrackIterRange< TrackType >
   {
      return Tracks< TrackType >( &Track::IsLeader );
   }

   template < typename TrackType = const Track >
      auto Any() const
         -> std::enable_if_t< std::is_const_v<TrackType>,
            TrackIterRange< TrackType >
         >
   {
      return Tracks< TrackType >( &Track::IsLeader );
   }


   template <typename TrackType = Track>
   auto Selected() -> TrackIterRange<TrackType>
   {
      return Tracks<TrackType>(&Track::IsSelectedLeader);
   }

   template <typename TrackType = const Track>
   auto Selected() const
      -> std::enable_if_t<std::is_const_v<TrackType>, TrackIterRange<TrackType>>
   {
      return Tracks<TrackType>(&Track::IsSelectedLeader);
   }


   template<typename TrackType>
      static auto SingletonRange( TrackType *pTrack )
         -> TrackIterRange< TrackType >
   {
      return pTrack->GetOwner()->template Tracks<TrackType>()
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
      return Channels_<TrackType>(pTrack->GetHolder()->Find(pTrack));
   }

   //! Count channels of a track
   static size_t NChannels(const Track &track)
   {
      return Channels(&track).size();
   }

   //! If the given track is one of a pair of channels, swap them
   /*! @return success */
   static bool SwapChannels(Track &track);

   friend class Track;

   /*!
    @pre `tracks` contains pointers only to leader tracks of this, and each of
    them exactly once
    */
   void Permute(const std::vector<Track *> &tracks);

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

   /*!
    Replace channel group `t` with the first group in the given list, return a
    temporary list of the removed tracks, modify given list by removing group

    Replacements may have fewer channels
    Give the replacements the same ids as the replaced

    @pre `t.IsLeader()`
    @pre `t.GetOwner().get() == this`
    @pre `t.NChannels() >= (*with.begin())->NChannels()`
    */
   TrackListHolder ReplaceOne(Track &t, TrackList &&with);

   //! Remove a channel group, given the leader
   /*!
    @pre `track.IsLeader()`
    */
   void Remove(Track &track);

   /// Make the list empty
   void Clear(bool sendEvent = true);

   bool CanMoveUp(Track * t) const;
   bool CanMoveDown(Track * t) const;

   bool MoveUp(Track * t);
   bool MoveDown(Track * t);
   bool Move(Track * t, bool up) { return up ? MoveUp(t) : MoveDown(t); }

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
   size_t NChannels() const;
   size_t Size() const { return Any().size(); }

   //! Return the least start time of the tracks, or 0 when no tracks
   double GetStartTime() const;
   //! Return the greatest end time of the tracks, or 0 when no tracks
   double GetEndTime() const;

   //! Construct a temporary list owned by `pProject` (if that is not null)
   //! so that `TrackList::Channels(left.get())` will enumerate the given
   //! tracks
   /*!
    @pre `left == nullptr || left->GetOwner() == nullptr`
    @pre `right == nullptr || (left && right->GetOwner() == nullptr)`
    */
   static TrackListHolder Temporary(AudacityProject *pProject,
      const Track::Holder &left = {}, const Track::Holder &right = {});

   //! Construct a temporary list whose first channel group contains the given
   //! channels, up to the limit of channel group size; excess channels go each
   //! into a separate group
   static TrackListHolder Temporary(
      AudacityProject *pProject, const std::vector<Track::Holder> &channels);

   /*!
    @copydoc Temporary(AudacityProject *, const std::vector<Track::Holder> &)
    Overload allowing shared pointers to some subclass of Track
    */
   template<typename T>
   static TrackListHolder Temporary(
      AudacityProject *pProject,
      const std::vector<std::shared_ptr<T>> &channels)
   {
      std::vector<Track::Holder> temp;
      static const auto convert = [](auto &pChannel){
         return std::static_pointer_cast<Track>(pChannel);
      };
      transform(channels.begin(), channels.end(), back_inserter(temp), convert);
      return Temporary(pProject, temp);
   }

   //! Remove all tracks from `list` and put them at the end of `this`
   void Append(TrackList &&list);

   //! Remove first channel group (if any) from `list` and put it at the end of
   //! `this`
   void AppendOne(TrackList &&list);

private:
   using ListOfTracks::size;

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
      || (mPendingUpdates && p.second == &*mPendingUpdates &&
          p.first == mPendingUpdates->ListOfTracks::end()); }
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
   void SelectionEvent(Track &track);
   void PermutationEvent(TrackNodePointer node);
   void DataEvent(
      const std::shared_ptr<Track> &pTrack, bool allChannels, int code );
   void EnsureVisibleEvent(
      const std::shared_ptr<Track> &pTrack, bool modifyState );
   void DeletionEvent(std::weak_ptr<Track> node, bool duringReplace);
   void AdditionEvent(TrackNodePointer node);
   void ResizingEvent(TrackNodePointer node);

   void SwapNodes(TrackNodePointer s1, TrackNodePointer s2);

   // Nondecreasing during the session.
   // Nonpersistent.
   // Used to assign ids to added tracks.
   static long sCounter;

public:
   //! The tracks supplied to this function will be leaders with the same number
   //! of channels
   using Updater = std::function<void(Track &dest, const Track &src)>;
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
   /*!
    @pre `GetOwner()`
    @pre `src->IsLeader()`
    @post result: `src->NChannels() == result.size()`
    */
   std::vector<Track*> RegisterPendingChangedTrack(
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

   /*
    Forget pending track additions and changes;
    if requested, give back the pending added tracks.
    @pre `GetOwner()`
    */
   void ClearPendingTracks(ListOfTracks *pAdded = nullptr);

   // Change the state of the project.
   // Strong guarantee for project state in case of exceptions.
   // Will always clear the pending updates.
   // Return true if the state of the track list really did change.
   bool ApplyPendingTracks();

   bool HasPendingTracks() const;

private:
   AudacityProject *mOwner;

   //! Shadow tracks holding append-recording in progress; need to put them into
   //! a list so that channel grouping works
   /*! Beware, they are in a disjoint iteration sequence from ordinary tracks */
   std::shared_ptr<TrackList> mPendingUpdates;
   //! This is in correspondence with leader tracks in mPendingUpdates
   std::vector< Updater > mUpdaters;
   //! Whether the list assigns unique ids to added tracks;
   //! false for temporaries
   bool mAssignsIds{ true };
};

TrackList* Track::GetHolder() const {
   return static_cast<TrackList*>(mNode.second); }

#endif
