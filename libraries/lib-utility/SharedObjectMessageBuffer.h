/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file SharedObjectMessageBuffer.h
 
 Paul Licameli
 
 **********************************************************************/
#ifndef __AUDACITY_SHARED_OBJECT_MESSAGE_BUFFER__
#define __AUDACITY_SHARED_OBJECT_MESSAGE_BUFFER__

#include <MessageBuffer.h>
#include <SharedObjectPool.h>

//! Producer supplies objects; consumer copies out shared pointers to objects
/*
 All heap allocation happens in the producer thread, and deallocation only when
 the buffer is destroyed or reinitialized
 @tparam T default-constructible, and copy- or move-assignable
 */
template<typename Data> class SharedObjectMessageBuffer
   : private MessageBuffer<std::shared_ptr<Data>>
{
public:
   //! Reset to default constructed state
   void Initialize() {
      MessageBuffer<std::shared_ptr<Data>>::Initialize();
      mPool.Clear();
   }
   using MessageBuffer<std::shared_ptr<Data>>::Read;
   void Write(Data arg) {
      auto pData = mPool.Get();
      *pData = std::move(arg);
      MessageBuffer<std::shared_ptr<Data>>::Write(std::move(pData));
   }
private:
   SharedObjectPool<Data> mPool;
};

#endif
