/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file Channel.h
  @brief Abstract class ChannelGroup with two discrete iterable dimensions,
  channels and intervals; subclasses associate information with those and
  their intersections

  Dominic Mazzoni

  Paul Licameli split from Track.h

**********************************************************************/
#ifndef __AUDACITY_CHANNEL__
#define __AUDACITY_CHANNEL__

#include <cassert>
#include <optional>
#include <vector>

#include "ClientData.h"
#include "MemoryX.h"

//! A start and an end time, and whatever else subclasses associate with them
/*!
 Start and end are immutable, but subclasses may add other mutable data
 @invariant `Start() <= End()`
 */
class CHANNEL_API ChannelGroupInterval {
public:
   /*! @pre `start <= end` */
   ChannelGroupInterval(double start, double end)
      : mStart{ start }, mEnd{ end }
   {
      assert(start <= end);
   }

   virtual ~ChannelGroupInterval();

   double Start() const { return mStart; }
   double End() const { return mEnd; }

private:
   const double mStart, mEnd;
};

//! The intersection of a Channel and a WideChannelGroupInterval
class CHANNEL_API ChannelInterval {
public:
   virtual ~ChannelInterval();
};

class ChannelGroup;

//! Start and end time, and channel width, and whatever else subclasses
//! associate with them
/*!
 Start and end and number of channels are immutable, but subclasses may add
 other mutable data
 @invariant `Start() <= End()`
 */
class CHANNEL_API WideChannelGroupInterval : public ChannelGroupInterval {
public:
   //! Initialize immutable properties, constraining number of channels to
   //! equal that of the containing group
   /*!
    @pre `group.IsLeader()`
    @pre `start <= end`
    @post `NChannels() == group.NChannels()`
    */
   WideChannelGroupInterval(
      const ChannelGroup &group, double start, double end);
   ~WideChannelGroupInterval() override;

   //! Report the number of channels
   /*!
    @post result: `result >= 1`
    */
   size_t NChannels() const { return mNChannels; }

   //! Retrieve a channel, cast to the given type
   /*!
    Postconditions imply that `GetChannel(0)` is always non-null

    @post if IntervalType is default, then:
       result: `!(iChannel < NChannels()) || result`
    */
   template<typename IntervalType = ChannelInterval>
   std::shared_ptr<IntervalType> GetChannel(size_t iChannel)
   {
      return
         std::dynamic_pointer_cast<IntervalType>(DoGetChannel(iChannel));
   }

   /*!
    @copydetails GetChannel(size_t)
    */
   template<typename IntervalType = const ChannelInterval>
   auto GetChannel(size_t iChannel) const
      -> std::enable_if_t<std::is_const_v<IntervalType>,
         std::shared_ptr<IntervalType>>
   {
      return std::dynamic_pointer_cast<IntervalType>(
         const_cast<WideChannelGroupInterval*>(this)->DoGetChannel(iChannel));
   }

   //! Iterator for channels; destroying the related ChannelGroup or
   //! WideChannelGroupInterval invalidates it
   template<typename IntervalType>
   class ChannelIterator
      : public ValueIterator<
         std::shared_ptr<IntervalType>, std::bidirectional_iterator_tag
      >
   {
      using GroupType = std::conditional_t<std::is_const_v<IntervalType>,
         const WideChannelGroupInterval, WideChannelGroupInterval>;
   public:
      ChannelIterator() = default;
      ChannelIterator(GroupType *pGroup, size_t index)
         : mpGroup{ pGroup }, mIndex{ index }
      {}

      std::shared_ptr<IntervalType> operator *() const
      {
         if (!mpGroup || mIndex >= mpGroup->NChannels())
            return {};
         return mpGroup->template GetChannel<IntervalType>(mIndex);
      }

      ChannelIterator &operator ++() { ++mIndex; return *this; }
      ChannelIterator operator ++(int)
         { auto copy{ *this }; operator ++(); return copy; }

      ChannelIterator &operator --() { --mIndex; return *this; }
      ChannelIterator operator --(int)
         { auto copy{ *this }; operator --(); return copy; }

      friend inline bool operator ==(ChannelIterator a, ChannelIterator b)
         { return a.mpGroup == b.mpGroup && a.mIndex == b.mIndex; }
      friend inline bool operator !=(ChannelIterator a, ChannelIterator b)
         { return !(a == b); }

   private:
      GroupType *mpGroup{};
      size_t mIndex{};
   };

   //! Get range of ChannelInterval objects with mutative access
   template<typename IntervalType = ChannelInterval>
   IteratorRange<ChannelIterator<IntervalType>> Channels()
   {
      return { { this, 0 }, { this, NChannels() } };
   }

   //! Get range of channels with read-only access
   template<typename IntervalType = const ChannelInterval>
   auto Channels() const
      -> std::enable_if_t<std::is_const_v<IntervalType>,
         IteratorRange<ChannelIterator<IntervalType>>
      >
   {
      return { { this, 0 }, { this, NChannels() } };
   }

protected:
   //! Retrieve a channel
   /*!
    @post result: `!(iChannel < NChannels()) || result`
    */
   virtual std::shared_ptr<ChannelInterval> DoGetChannel(size_t iChannel) = 0;

private:
   const size_t mNChannels;
};

class CHANNEL_API Channel
{
public:
   virtual ~Channel();

   //! Channel object's lifetime is assumed to be nested in its Track's
   ChannelGroup &GetChannelGroup();
   /*!
    @copydoc GetChannelGroup()
    */
   const ChannelGroup &GetChannelGroup() const;

   /*!
    @return `ii` such that `this == GetChannelGroup().GetChannel(ii).get()`
    */
   size_t GetChannelIndex() const;

   size_t ReallyGetChannelIndex() const;

   /*!
      @name Acesss to intervals
      @{
   */

   using Interval = ChannelInterval;

   //! Report the number of intervals
   size_t NIntervals() const;

   //! Retrieve an interval, cast to the given type
   /*!
    @post if IntervalType is default, then:
       result: `!(iInterval < NIntervals()) || result`
    */
   template<typename IntervalType = Interval>
   std::shared_ptr<IntervalType> GetInterval(size_t iInterval);

   /*!
    @copydetails GetInterval(size_t)
    */
   template<typename IntervalType = const Interval>
   auto GetInterval(size_t iInterval) const
      -> std::enable_if_t<std::is_const_v<IntervalType>,
         std::shared_ptr<IntervalType>>;

   //! Iterator for intervals; destroying the related ChannelGroup or
   //! WideChannelGroupInterval invalidates it
   /*!
    Some intervals may have zero duration, and no ordering of the intervals is
    assumed.
   */
   template<typename IntervalType>
   class IntervalIterator
      : public ValueIterator<
         std::shared_ptr<IntervalType>, std::bidirectional_iterator_tag
      >
   {
      using ChannelType = std::conditional_t<std::is_const_v<IntervalType>,
         const Channel, Channel>;
   public:
      IntervalIterator() = default;
      IntervalIterator(ChannelType *pChannel, size_t index)
         : mpChannel{ pChannel }, mIndex{ index }
      {}

      std::shared_ptr<IntervalType> operator *() const
      {
         if (!mpChannel || mIndex >= mpChannel->NIntervals())
            return {};
         return mpChannel->template GetInterval<IntervalType>(mIndex);
      }

      IntervalIterator &operator ++() { ++mIndex; return *this; }
      IntervalIterator operator ++(int)
         { auto copy{ *this }; operator ++(); return copy; }

      IntervalIterator &operator --() { --mIndex; return *this; }
      IntervalIterator operator --(int)
         { auto copy{ *this }; operator --(); return copy; }

      friend inline bool operator ==(IntervalIterator a, IntervalIterator b)
         { return a.mpChannel == b.mpChannel && a.mIndex == b.mIndex; }
      friend inline bool operator !=(IntervalIterator a, IntervalIterator b)
         { return !(a == b); }

   private:
      ChannelType *mpChannel{};
      size_t mIndex{};
   };

   //! Get range of intervals with mutative access
   template<typename IntervalType = Interval>
   IteratorRange<IntervalIterator<IntervalType>> Intervals()
   {
      return { { this, 0 }, { this, NIntervals() } };
   }

   //! Get range of intervals with read-only access
   template<typename IntervalType = const Interval>
   auto Intervals() const
      -> std::enable_if_t<std::is_const_v<IntervalType>,
         IteratorRange<IntervalIterator<IntervalType>>
      >
   {
      return { { this, 0 }, { this, NIntervals() } };
   }

   /*!
      @}
   */

protected:
   //! Subclass must override
   /*!
    @post result: for some `ii` less than `result.NChannels()`,
       `this == result.GetChannel(ii).get()`
    */
   virtual ChannelGroup &DoGetChannelGroup() const = 0;

   //! This is temporary!  It defaults to call the above
   virtual ChannelGroup &ReallyDoGetChannelGroup() const;

private:
   int FindChannelIndex() const;
};

class CHANNEL_API ChannelGroup
{
public:
   virtual ~ChannelGroup();

   //! Get the minimum of Start() values of intervals, or 0 when none
   double GetStartTime() const;
   //! Get the maximum of End() values of intervals, or 0 when none
   double GetEndTime() const;

   //! Change start time by given duration
   /*
    @pre `IsLeader()`
    */
   void ShiftBy(double t) { MoveTo(GetStartTime() + t); }

   //! Change start time to given time point
   /*
    @pre `IsLeader()`
    */
   virtual void MoveTo(double o) = 0;

   /*!
      @name Acesss to channels
      @{
   */

   //! Report the number of channels
   /*!
    @post result: `result >= 1`
    */
   virtual size_t NChannels() const = 0;

   //! Retrieve a channel, cast to the given type
   /*!
    Postconditions imply that `GetChannel(0)` is always non-null

    @post if ChannelType is default, then:
       result: `!(iChannel < NChannels()) || result`
    */
   template<typename ChannelType = Channel>
   std::shared_ptr<ChannelType> GetChannel(size_t iChannel)
   {
      return
         std::dynamic_pointer_cast<ChannelType>(DoGetChannel(iChannel));
   }

   /*!
    @copydetails GetChannel(size_t)
    */
   template<typename ChannelType = const Channel>
   auto GetChannel(size_t iChannel) const
      -> std::enable_if_t<std::is_const_v<ChannelType>,
         std::shared_ptr<ChannelType>>
   {
      return std::dynamic_pointer_cast<ChannelType>(
         const_cast<ChannelGroup*>(this)->DoGetChannel(iChannel));
   }

   //! Iterator for channels; destroying the related ChannelGroup invalidates
   //! it
   template<typename ChannelType>
   class ChannelIterator
      : public ValueIterator<
         std::shared_ptr<ChannelType>, std::bidirectional_iterator_tag
      >
   {
      using GroupType = std::conditional_t<std::is_const_v<ChannelType>,
         const ChannelGroup, ChannelGroup>;
   public:
      ChannelIterator() = default;
      ChannelIterator(GroupType *pGroup, size_t index)
         : mpGroup{ pGroup }, mIndex{ index }
      {}

      std::shared_ptr<ChannelType> operator *() const
      {
         if (!mpGroup || mIndex >= mpGroup->NChannels())
            return {};
         return mpGroup->template GetChannel<ChannelType>(mIndex);
      }

      ChannelIterator &operator ++() { ++mIndex; return *this; }
      ChannelIterator operator ++(int)
         { auto copy{ *this }; operator ++(); return copy; }

      ChannelIterator &operator --() { --mIndex; return *this; }
      ChannelIterator operator --(int)
         { auto copy{ *this }; operator --(); return copy; }

      friend inline bool operator ==(ChannelIterator a, ChannelIterator b)
         { return a.mpGroup == b.mpGroup && a.mIndex == b.mIndex; }
      friend inline bool operator !=(ChannelIterator a, ChannelIterator b)
         { return !(a == b); }

   private:
      GroupType *mpGroup{};
      size_t mIndex{};
   };

   //! Get range of channels with mutative access
   /*!
    @pre `IsLeader()`
    */
   template<typename ChannelType = Channel>
   IteratorRange<ChannelIterator<ChannelType>> Channels()
   {
      assert(IsLeader());
      return { { this, 0 }, { this, NChannels() } };
   }

   //! Get range of channels with read-only access
   /*!
    @pre `IsLeader()`
    */
   template<typename ChannelType = const Channel>
   auto Channels() const
      -> std::enable_if_t<std::is_const_v<ChannelType>,
         IteratorRange<ChannelIterator<ChannelType>>
      >
   {
      assert(IsLeader());
      return { { this, 0 }, { this, NChannels() } };
   }

   /*!
      @}
      @name Acesss to intervals
      @{
   */

   using Interval = WideChannelGroupInterval;

   //! Report the number of intervals
   virtual size_t NIntervals() const = 0;

   //! Retrieve an interval, cast to the given type
   /*!
    @post if IntervalType is default, then:
       result: `!(iInterval < NIntervals()) || result`
    */
   template<typename IntervalType = Interval>
   std::shared_ptr<IntervalType> GetInterval(size_t iInterval)
   {
      return
         std::dynamic_pointer_cast<IntervalType>(DoGetInterval(iInterval));
   }

   /*!
    @copydetails GetInterval(size_t)
    */
   template<typename IntervalType = const Interval>
   auto GetInterval(size_t iInterval) const
      -> std::enable_if_t<std::is_const_v<IntervalType>,
         std::shared_ptr<IntervalType>>
   {
      return std::dynamic_pointer_cast<IntervalType>(
         const_cast<ChannelGroup*>(this)->DoGetInterval(iInterval));
   }

   //! Iterator for intervals; destroying the related ChannelGroup invalidates
   //! it
   /*!
    Some intervals may have zero duration, and no ordering of the intervals is
    assumed.
   */
   template<typename IntervalType>
   class IntervalIterator
      : public ValueIterator<
         std::shared_ptr<IntervalType>, std::bidirectional_iterator_tag
      >
   {
      using GroupType = std::conditional_t<std::is_const_v<IntervalType>,
         const ChannelGroup, ChannelGroup>;
   public:
      IntervalIterator() = default;
      IntervalIterator(GroupType *pGroup, size_t index)
         : mpGroup{ pGroup }, mIndex{ index }
      {}

      std::shared_ptr<IntervalType> operator *() const
      {
         if (!mpGroup || mIndex >= mpGroup->NIntervals())
            return {};
         return mpGroup->template GetInterval<IntervalType>(mIndex);
      }

      IntervalIterator &operator ++() { ++mIndex; return *this; }
      IntervalIterator operator ++(int)
         { auto copy{ *this }; operator ++(); return copy; }

      IntervalIterator &operator --() { --mIndex; return *this; }
      IntervalIterator operator --(int)
         { auto copy{ *this }; operator --(); return copy; }

      friend inline bool operator ==(IntervalIterator a, IntervalIterator b)
         { return a.mpGroup == b.mpGroup && a.mIndex == b.mIndex; }
      friend inline bool operator !=(IntervalIterator a, IntervalIterator b)
         { return !(a == b); }

   private:
      GroupType *mpGroup{};
      size_t mIndex{};
   };

   //! Get range of intervals with mutative access
   /*
      @pre `IsLeader()`
    */
   template<typename IntervalType = Interval>
   IteratorRange<IntervalIterator<IntervalType>> Intervals()
   {
      assert(IsLeader());
      return { { this, 0 }, { this, NIntervals() } };
   }

   //! Get range of intervals with read-only access
   /*
      @pre `IsLeader()`
    */
   template<typename IntervalType = const Interval>
   auto Intervals() const
      -> std::enable_if_t<std::is_const_v<IntervalType>,
         IteratorRange<IntervalIterator<IntervalType>>
      >
   {
      assert(IsLeader());
      return { { this, 0 }, { this, NIntervals() } };
   }

   /*!
      @}
   */

   // TODO remove this which is only used in assertions
   virtual bool IsLeader() const = 0;

   //! Hosting of objects attached by higher level code
   struct ChannelGroupData;
   using Attachments = ClientData::Site<
      ChannelGroupData, ClientData::Cloneable<>, ClientData::DeepCopying
   >;

   //! Make attachment site on demand as needed
   ChannelGroupData &GetGroupData();
   //! Make attachment site on demand as needed
   //! May make new group data on demand, but consider that logically const
   const ChannelGroupData &GetGroupData() const;

   //! Do not make attachment site on demand if absent
   ChannelGroupData *FindGroupData() { return mpGroupData.get(); }
   //! Do not make attachment site on demand if absent
   const ChannelGroupData *FindGroupData() const { return mpGroupData.get(); }

   //! Copy, including cloning of attached objects
   void Init(const ChannelGroup &other);

   //! Leave all attachments null
   void DestroyGroupData();

   //! Move attachments out
   std::unique_ptr<ChannelGroupData> DetachGroupData();

   //! Replace any previous attachments
   void AssignGroupData(std::unique_ptr<ChannelGroupData> pGroupData);

   // TODO wide wave tracks -- remove this
   //! For two tracks describes the type of the linkage
   enum class LinkType : int {
       None = 0, //< No linkage
       Group = 2, //< Tracks are grouped together
       Aligned, //< Tracks are grouped and changes should be synchronized
   };

   // Structure describing data common to channels of a group of tracks
   // Should be deep-copyable (think twice before adding shared pointers!)
   struct CHANNEL_API ChannelGroupData : Attachments {
      using Attachments = ChannelGroup::Attachments;
      wxString mName;
      LinkType mLinkType{ LinkType::None };
      std::optional<double> mProjectTempo;
      bool mSelected{ false };
   };

protected:
   //! Retrieve a channel
   /*!
    @post result: `!(iChannel < NChannels()) || result`
    */
   virtual std::shared_ptr<Channel> DoGetChannel(size_t iChannel) = 0;

   //! Retrieve an interval
   /*!
    @post result: `!(iInterval < NIntervals()) || result`
    */
   virtual std::shared_ptr<Interval> DoGetInterval(size_t iInterval) = 0;

private:
   // TODO wide wave tracks -- Make ChannelGroup itself the Site
   std::unique_ptr<ChannelGroupData> mpGroupData;
};

inline size_t Channel::NIntervals() const
{
   return GetChannelGroup().NIntervals();
}

template<typename IntervalType>
std::shared_ptr<IntervalType> Channel::GetInterval(size_t iInterval)
{
   return ReallyDoGetChannelGroup().GetInterval(iInterval)
      ->template GetChannel<IntervalType>(ReallyGetChannelIndex());
}

template<typename IntervalType>
auto Channel::GetInterval(size_t iInterval) const
   -> std::enable_if_t<std::is_const_v<IntervalType>,
      std::shared_ptr<IntervalType>>
{
   return ReallyDoGetChannelGroup().GetInterval(iInterval)
      ->template GetChannel<IntervalType>(ReallyGetChannelIndex());
}
#endif
