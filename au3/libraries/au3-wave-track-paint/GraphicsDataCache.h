/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  GraphicsDataCache.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstdint>
#include <deque>
#include <functional>
#include <mutex>
#include <type_traits>
#include <vector>

#include "MemoryX.h"
#include "IteratorX.h"

class ZoomInfo;

//! A key into the graphics data cache
struct WAVE_TRACK_PAINT_API GraphicsDataCacheKey final
{
    //! Level 1 key: zoom level
    double PixelsPerSecond { 0.0 };
    //! Level 2 key: first cached sample in the "global" scale
    int64_t FirstSample { -1 };
};

//! A base class for the for cache elements
struct WAVE_TRACK_PAINT_API GraphicsDataCacheElementBase /* not final */
{
    virtual ~GraphicsDataCacheElementBase() = default;
    //! This method is called when the item is evicted from the cache. Default implementation is empty
    virtual void Dispose();
    //! This method is called during the lookup when new items are inserted. prevElement can be nullptr. Default implementation is empty
    virtual void Smooth(GraphicsDataCacheElementBase* prevElement);

    //! Index filled by GraphicsDataCacheBase to implement LRU eviction policy
    uint64_t LastCacheAccess { 0 };
    //! The last time the item was updated. If (!IsComplete && LastUpdate < LastCacheAccess) the item will be updated.
    uint64_t LastUpdate { 0 };
    //! Cache implementation is responsible to set this flag when all the data of the item is filled
    bool IsComplete { false };
    //! This flag is used to simplify the eviction algorithm
    bool AwaitsEviction { false };
};

//! A base class for the GraphicsDataCache. Implements LRU policy
class WAVE_TRACK_PAINT_API GraphicsDataCacheBase /* not final */
{
public:
    // Number of pixels in a single cache element
    constexpr static uint32_t CacheElementWidth = 256;

    virtual ~GraphicsDataCacheBase() = default;

    //! Invalidate the cache content
    void Invalidate();

    //! Returns the sample rate associated with cache
    double GetScaledSampleRate() const noexcept;

    void UpdateViewportWidth(int64_t width) noexcept;
    int64_t GetMaxViewportWidth() const noexcept;

protected:
    explicit GraphicsDataCacheBase(double scaledSampleRate);

    void SetScaledSampleRate(double scaledSampleRate);

    //! Element of the cache lookup
    struct WAVE_TRACK_PAINT_API LookupElement final
    {
        GraphicsDataCacheKey Key;
        GraphicsDataCacheElementBase* Data {};
    };
    //! Cache lookup is a vector, with items sorted using Key
    using Lookup = std::vector<LookupElement>;

    //! A result of the cache lookup
    struct WAVE_TRACK_PAINT_API BaseLookupResult final
    {
        //! Iterator to the first cache element that fulfills the request
        Lookup::iterator Begin;
        //! Iterator past the last cache element that fulfills the request
        Lookup::iterator End;
        //! First column of the first element that matches the request
        size_t LeftOffset {};
        //! Offset from the right for the last element that matches the request
        size_t RightOffset {};
    };
    //! Create a new Cache element. Implementation is responsible of the lifetime control.
    virtual GraphicsDataCacheElementBase* CreateElement(const GraphicsDataCacheKey& key) = 0;
    //! This method is called, when the cache element should be evicted. Implementation may not deallocate the object.
    virtual void DisposeElement(GraphicsDataCacheElementBase* element) = 0;

    //! This method is called on all elements matching the request that are not complete (i. e. IsComplete if false).
    virtual bool UpdateElement(
        const GraphicsDataCacheKey& key, GraphicsDataCacheElementBase& element) = 0;

    //! Perform a lookup inside the cache. This method modifies mLookup and invalidates any previous result.
    BaseLookupResult PerformBaseLookup(const ZoomInfo& zoomInfo, double t0, double t1);

    //! Perform a lookup for the given key. This method modifies mLookup and invalidates any previous result.
    const GraphicsDataCacheElementBase* PerformBaseLookup(GraphicsDataCacheKey key);

private:
    // Called internally to create a list of items in the mNewLookupItems
    bool CreateNewItems();
    // Called internally if the CreateNewItems has failed to dispose all the elements created
    void DisposeNewItems();
    Lookup::iterator FindKey(GraphicsDataCacheKey key);

    // Called internally to evict the no longer needed items. Cache keeps mCacheSizeMultiplier * mMaxWidth / CacheElementWidth items
    void PerformCleanup();
    // A heap based approach if cache needs to evict more than one item
    void PerformFullCleanup(int64_t currentSize, int64_t itemsToEvict);
    // This vector is sorted according to the key
    Lookup mLookup;
    // This vector is used to avoid memory allocations when growing the cache
    Lookup mLookupHelper;
    // This is a helper vector to store newly created items before they are inserted in cache
    Lookup mNewLookupItems;
    // This is a helper vector to implement the heap structure for the LRU policy
    std::vector<size_t> mLRUHelper;

    // Sample rate associated with this cache
    double mScaledSampleRate {}; // DV: Why do we use double for sample rate? I don't know

    // The max width of the request processed in pixels
    int64_t mMaxWidth { 1600 };
    // This value is incremented on every lookup
    uint64_t mCacheAccessIndex {};
    // A multiplier used to control the cache size
    int32_t mCacheSizeMultiplier { 4 };

    template<typename CacheElementType>
    friend class GraphicsDataCacheIterator;
};

//! A class that implements an iterator for the cache lookup result
template<typename CacheElementType>
class GraphicsDataCacheIterator final
{
public:
    using iterator_category = std::forward_iterator_tag;
    using difference_type = std::ptrdiff_t;
    using value_type = CacheElementType;
    using pointer = value_type*;
    using reference = value_type&;

    GraphicsDataCacheIterator() = default;
    GraphicsDataCacheIterator(const GraphicsDataCacheIterator&) = default;
    GraphicsDataCacheIterator(GraphicsDataCacheIterator&&) = default;
    GraphicsDataCacheIterator& operator=(const GraphicsDataCacheIterator&) = default;
    GraphicsDataCacheIterator& operator=(GraphicsDataCacheIterator&&) = default;

    reference operator*() const
    {
        return static_cast<reference>(*mIterator->Data);
    }

    pointer operator->()
    {
        return static_cast<pointer>(mIterator->Data);
    }

    // Prefix increment
    GraphicsDataCacheIterator& operator++()
    {
        mIterator++;
        return *this;
    }

    // Postfix increment
    GraphicsDataCacheIterator operator++(int)
    {
        GraphicsDataCacheIterator tmp = *this;
        ++(*this);
        return tmp;
    }

    size_t GetLeftOffset() const
    {
        return mIterator == mBaseLookup.Begin ? mBaseLookup.LeftOffset : 0;
    }

    size_t GetRightOffset() const
    {
        auto next = mIterator;
        ++next;

        return next == mBaseLookup.End ? mBaseLookup.RightOffset : 0;
    }

    friend bool operator==(
        const GraphicsDataCacheIterator& lhs,
        const GraphicsDataCacheIterator& rhs)
    {
        return lhs.mIterator == rhs.mIterator;
    }

    friend bool operator!=(
        const GraphicsDataCacheIterator& lhs,
        const GraphicsDataCacheIterator& rhs)
    {
        return lhs.mIterator != rhs.mIterator;
    }

private:
    GraphicsDataCacheIterator(
        const GraphicsDataCacheBase::BaseLookupResult& base, bool begin)
        : mBaseLookup(base)
        , mIterator(begin ? base.Begin : base.End)
    {
    }

    GraphicsDataCacheBase::BaseLookupResult mBaseLookup;
    GraphicsDataCacheBase::Lookup::const_iterator mIterator;

    template<typename T>
    friend class GraphicsDataCache;
};

template<typename CacheElementType>
class GraphicsDataCache /* not final */ : public GraphicsDataCacheBase
{
public:
    using ElementFactory = std::function<std::unique_ptr<CacheElementType>()>;

    GraphicsDataCache(const GraphicsDataCache&) = delete;
    GraphicsDataCache& operator =(const GraphicsDataCache&) = delete;

    using Initializer = std::function<bool (const GraphicsDataCacheKey& Key, CacheElementType& element)>;

    explicit GraphicsDataCache(double scaledSampleRate, ElementFactory elementFactory)
        : GraphicsDataCacheBase(scaledSampleRate), mElementFactory(std::move(elementFactory))
    {
    }

    ~GraphicsDataCache() override
    {
        Invalidate();
    }

    IteratorRange<GraphicsDataCacheIterator<CacheElementType> >
    PerformLookup(const ZoomInfo& zoomInfo, double t0, double t1)
    {
        CheckCache(zoomInfo, t0, t1);

        const auto base = this->PerformBaseLookup(zoomInfo, t0, t1);

        return { { base, true }, { base, false } };
    }

    const CacheElementType* PerformLookup(GraphicsDataCacheKey key)
    {
        return static_cast<const CacheElementType*>(PerformBaseLookup(key));
    }

    void setInitializer(Initializer initializer)
    {
        mInitializer = std::move(initializer);
    }

protected:
    virtual void
    CheckCache(const ZoomInfo& /*zoomInfo*/, double /*t0*/, double /*t1*/)
    {
    }

    virtual bool
    InitializeElement(const GraphicsDataCacheKey& key, CacheElementType& element)
    {
        if constexpr (std::is_assignable_v<
                          CacheElementType, GraphicsDataCacheKey>) {
            element = key;
            return true;
        } else if constexpr (std::is_constructible_v<
                                 CacheElementType, GraphicsDataCacheKey>) {
            element = CacheElementType(key);
            return true;
        } else {
            return false;
        }
    }

private:
    GraphicsDataCacheElementBase* CreateElement(const GraphicsDataCacheKey& key) override
    {
        CacheElementType* element = nullptr;

        if (!mFreeList.empty()) {
            element = mFreeList.back();
            mFreeList.pop_back();
        }

        if (element == nullptr) {
            mCache.push_back(std::move(mElementFactory()));
            element = mCache.back().get();
        }

        if (element == nullptr) {
            return nullptr;
        }

        if (mInitializer) {
            if (!mInitializer(key, *element)) {
                DisposeElement(element);
                return nullptr;
            }
        } else if (!InitializeElement(key, *element)) {
            DisposeElement(element);
            return nullptr;
        }

        return element;
    }

    void DisposeElement(GraphicsDataCacheElementBase* element) override
    {
        if (element == nullptr) {
            return;
        }

        element->Dispose();

        mFreeList.push_back(static_cast<CacheElementType*>(element));
    }

    bool UpdateElement(
        const GraphicsDataCacheKey& key,
        GraphicsDataCacheElementBase& element) override
    {
        return InitializeElement(key, static_cast<CacheElementType&>(element));
    }

    Initializer mInitializer;

    ElementFactory mElementFactory;
    std::deque<std::unique_ptr<CacheElementType> > mCache;
    std::vector<CacheElementType*> mFreeList;
};
