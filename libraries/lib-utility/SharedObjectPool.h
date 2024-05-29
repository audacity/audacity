/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file SharedObjectPool.h
 
 Paul Licameli
 
 **********************************************************************/
#ifndef __AUDACITY_SHARED_OBJECT_POOL__
#define __AUDACITY_SHARED_OBJECT_POOL__

#include <algorithm>
#include <memory>
#include <vector>

//! A collection of recyclable shared pointers to a common actual type
/*!
 This allows all memory allocation to occur only in the thread using Get().
 The pool grows monotonically.  Memory is reclaimed only when the pool is
 destroyed or cleared.
 @tparam T default-constructible
 */
template<typename T> class SharedObjectPool {
public:
   std::shared_ptr<T> Get() {
      const auto end = mPool.end();
      auto iter = std::find_if(mPool.begin(), end,
         [](auto &pObject){ return pObject.use_count() == 1; });
      if (iter == end) {
         mPool.emplace_back(std::make_shared<T>());
         return mPool.back();
      }
      else
         return *iter;
   }
   void Clear() {
      mPool.clear();
   }
private:
   std::vector<std::shared_ptr<T>> mPool;
};

#endif
