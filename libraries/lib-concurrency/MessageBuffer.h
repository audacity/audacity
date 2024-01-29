/*!********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file MessageBuffer.h
 
 Paul Licameli split from PlaybackSchedule.h
 
 **********************************************************************/

#ifndef __AUDACITY_MESSAGE_BUFFER__
#define __AUDACITY_MESSAGE_BUFFER__

#include "NonInterfering.h"
#include <atomic>

//! Communicate data atomically from one writer thread to one reader.
/*!
 This is not a queue: it is not necessary for each write to be read.
 Rather loss of a message is allowed:  writer may overwrite.
 Data must be default-constructible and reassignable.
 */
template<typename Data>
class MessageBuffer {
   struct UpdateSlot {
      Data mData;
      std::atomic<bool> mBusy{ false };
   };
   NonInterfering<UpdateSlot> mSlots[2];

   std::atomic<unsigned char> mLastWrittenSlot{ 0 };

public:
   void Initialize();

   //! Move data out (if available), or else copy it out
   /*!
    @tparam Result is constructible from Data&& and forwards of other arguments
   */
   template<typename Result = Data, typename... ConstructorArgs>
   Result Read(ConstructorArgs &&...args);
   
   //! Reassign a slot by move or copy
   template<typename Arg = Data&&> void Write( Arg &&arg );
};

template<typename Data>
void MessageBuffer<Data>::Initialize()
{
   for (auto &slot : mSlots)
      // Lock both slots first, maybe spinning a little
      while ( slot.mBusy.exchange( true, std::memory_order_acquire ) )
         {}

   mSlots[0].mData = {};
   mSlots[1].mData = {};
   mLastWrittenSlot.store( 0, std::memory_order_relaxed );

   for (auto &slot : mSlots)
      slot.mBusy.exchange( false, std::memory_order_release );
}

template<typename Data>
template<typename Result, typename... ConstructorArgs>
Result MessageBuffer<Data>::Read(ConstructorArgs &&...args)
{
   // Whichever slot was last written, prefer to read that.
   auto idx = mLastWrittenSlot.load( std::memory_order_relaxed );
   idx = 1 - idx;
   bool wasBusy = false;
   do {
      // This loop is unlikely to execute twice, but it might because the
      // producer thread is writing a slot.
      idx = 1 - idx;
      wasBusy = mSlots[idx].mBusy.exchange( true, std::memory_order_acquire );
   } while ( wasBusy );

   // Copy the slot
   Result result(
      std::move( mSlots[idx].mData ), std::forward<ConstructorArgs>(args)... );

   mSlots[idx].mBusy.store( false, std::memory_order_release );

   return result;
}

template<typename Data>
template<typename Arg>
void MessageBuffer<Data>::Write( Arg &&arg )
{
   // Whichever slot was last written, prefer to write the other.
   auto idx = mLastWrittenSlot.load( std::memory_order_relaxed );
   bool wasBusy = false;
   do {
      // This loop is unlikely to execute twice, but it might because the
      // consumer thread is reading a slot.
      idx = 1 - idx;
      wasBusy = mSlots[idx].mBusy.exchange( true, std::memory_order_acquire );
   } while ( wasBusy );

   mSlots[idx].mData = std::forward<Arg>(arg);
   mLastWrittenSlot.store( idx, std::memory_order_relaxed );

   mSlots[idx].mBusy.store( false, std::memory_order_release );
}

#endif
