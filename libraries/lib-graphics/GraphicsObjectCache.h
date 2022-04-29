/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  GraphicsDataCache.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <array>
#include <algorithm>
#include <functional>
#include <map>
#include <numeric>

namespace internal
{
template <typename KeyType, bool useLookupHelper>
struct GraphicsObjectCacheLookup
{
};

template <typename KeyType>
struct GraphicsObjectCacheLookup<KeyType, true>
{
   std::map<KeyType, size_t, std::less<>> mCacheLookupHelper;
};
} // namespace internal

template<typename KeyType, typename ObjectType, size_t CacheSize = 64, bool useLookupHelper = true>
class GraphicsObjectCache final :
    private internal::GraphicsObjectCacheLookup<
       KeyType, useLookupHelper>
{
public:
   using ObjectFactory = std::function<ObjectType (const KeyType &)>;

   explicit GraphicsObjectCache(ObjectFactory factory)
       : mFactory(std::move(factory))
   {}

   template<typename K>
   ObjectType Get(const K& key)
   {
      const size_t cacheIndex = this->LookupKey(key);
      
      if (cacheIndex != CacheSize)
      {
         mCache[cacheIndex].lastUsed = ++mUseCounter;
         return mCache[cacheIndex].object;
      }

      KeyType realKey(key);

      if (mEntriesCount < CacheSize)
      {
         mCache[mEntriesCount++] = {
            realKey,
            mFactory(realKey),
            ++mUseCounter
         };

         if constexpr (useLookupHelper)
         {
            this->mCacheLookupHelper.emplace(
               std::move(realKey), mEntriesCount - 1);
         }
         
         return mCache[mEntriesCount - 1].object;
      }

      const auto oldestItem = std::min_element(
         mCache.begin(), mCache.end(),
         [](const auto& a, const auto& b) { return a.lastUsed < b.lastUsed; });

      if constexpr (useLookupHelper)
      {
         this->mCacheLookupHelper.erase(oldestItem->key);
         this->mCacheLookupHelper.emplace(
            realKey,
            std::distance(mCache.begin(), oldestItem));
      }

      *oldestItem = {
         realKey,
         mFactory(realKey),
         ++mUseCounter
      };
      
      return oldestItem->object;
   }

   void Clear()
   {
      if constexpr (useLookupHelper)
         this->mCacheLookupHelper.clear();

      mCache = {};
      
      mUseCounter = 0;
      mEntriesCount = 0;
   }

private:
   template <typename K> size_t LookupKey(const K& key) const
   {
      if constexpr (useLookupHelper)
      {
         auto it = this->mCacheLookupHelper.find(key);
         return it != this->mCacheLookupHelper.end() ? it->second : CacheSize;
      }
      else
      {
         for (size_t i = 0; i < mEntriesCount; ++i)
         {
            if (mCache[i].key == key)
               return i;
         }
      }

      return CacheSize;
   }
   
   ObjectFactory mFactory;

   struct CacheEntry final
   {
      KeyType key;
      ObjectType object;
      size_t lastUsed;
   };

   std::array<CacheEntry, CacheSize> mCache {};
   
   size_t mEntriesCount { 0 };
   size_t mUseCounter { 0 };
};
