/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  GraphicsDataCache.cpp

  Dmitry Vedenko

**********************************************************************/
#include "GraphicsDataCache.h"

#include <algorithm>
#include <cassert>
#include <type_traits>

#include "ZoomInfo.h"

#include "float_cast.h"
#include "RoundUpUnsafe.h"

namespace {
bool IsSameSample(double sampleRate, double t0, double t1) noexcept
{
    const auto firstSample  = llrint(t0 * sampleRate);
    const auto secondSample = llrint(t1 * sampleRate);

    return firstSample == secondSample;
}

bool IsSamePPS(double sampleRate, double lhs, double rhs)
{
    return std::abs(1.0 / lhs - 1.0 / rhs)
           * GraphicsDataCacheBase::CacheElementWidth
           < (1.0 / sampleRate);
}

bool IsSameKey(
    double sampleRate, GraphicsDataCacheKey lhs, GraphicsDataCacheKey rhs)
{
    return lhs.FirstSample == rhs.FirstSample
           && IsSamePPS(sampleRate, lhs.PixelsPerSecond, rhs.PixelsPerSecond);
}

bool IsKeyLess(
    double sampleRate, GraphicsDataCacheKey lhs, GraphicsDataCacheKey rhs)
{
    if (IsSamePPS(sampleRate, lhs.PixelsPerSecond, rhs.PixelsPerSecond)) {
        return lhs.FirstSample < rhs.FirstSample;
    } else {
        return lhs.PixelsPerSecond < rhs.PixelsPerSecond;
    }
}

template<typename Container>
auto GetPPSMatchRange(
    const Container& container, double pixelsPerSecond, double sampleRate)
{
    return std::equal_range(
        container.begin(), container.end(), pixelsPerSecond,
        [sampleRate](auto lhs, auto rhs)
    {
        if constexpr (std::is_arithmetic_v<std::decay_t<decltype(lhs)> >) {
            return !IsSamePPS(sampleRate, lhs, rhs.Key.PixelsPerSecond)
                   && lhs < rhs.Key.PixelsPerSecond;
        } else {
            return !IsSamePPS(sampleRate, lhs.Key.PixelsPerSecond, rhs)
                   && lhs.Key.PixelsPerSecond < rhs;
        }
    });
}
} // namespace

void GraphicsDataCacheBase::Invalidate()
{
    for (auto& item : mLookup) {
        DisposeElement(item.Data);
    }

    mLookup.clear();
}

double GraphicsDataCacheBase::GetScaledSampleRate() const noexcept
{
    return mScaledSampleRate;
}

void GraphicsDataCacheBase::UpdateViewportWidth(int64_t width) noexcept
{
    mMaxWidth = std::max(mMaxWidth, width);
}

int64_t GraphicsDataCacheBase::GetMaxViewportWidth() const noexcept
{
    return mMaxWidth;
}

GraphicsDataCacheBase::GraphicsDataCacheBase(double sampleRate)
    : mScaledSampleRate{sampleRate}
{
}

void GraphicsDataCacheBase::SetScaledSampleRate(double scaledSampleRate)
{
    if (
        std::abs(mScaledSampleRate - scaledSampleRate)
        <= std::numeric_limits<double>::epsilon()) {
        return;
    }

    mScaledSampleRate = scaledSampleRate;
    Invalidate();
}

void GraphicsDataCacheElementBase::Dispose()
{
}

void GraphicsDataCacheElementBase::Smooth(
    GraphicsDataCacheElementBase* prevElement)
{
}

GraphicsDataCacheBase::BaseLookupResult
GraphicsDataCacheBase::PerformBaseLookup(
    const ZoomInfo& zoomInfo, double t0, double t1)
{
    if (bool(t0 > t1) || IsSameSample(mScaledSampleRate, t0, t1)) {
        return {};
    }

    const double pixelsPerSecond = zoomInfo.GetZoom();

    const int64_t left  = zoomInfo.TimeToPosition(t0);
    const int64_t right = zoomInfo.TimeToPosition(t1) + 1;

    const int64_t width = right - left;

    const int64_t cacheLeft       = left / CacheElementWidth;
    const int64_t cacheRight      = (right + CacheElementWidth - 1) / CacheElementWidth;
    const int64_t cacheItemsCount = cacheRight - cacheLeft;

    const int64_t cacheLeftColumn  = cacheLeft * CacheElementWidth;
    const int64_t cacheRightColumn = cacheRight * CacheElementWidth;

    const double samplesPerPixel = mScaledSampleRate / pixelsPerSecond;

    UpdateViewportWidth(width);

    mNewLookupItems.clear();
    mNewLookupItems.reserve(cacheItemsCount);

    const auto ppsMatchRange
        =GetPPSMatchRange(mLookup, pixelsPerSecond, mScaledSampleRate);

    for (int64_t itemIndex = 0; itemIndex < cacheItemsCount; ++itemIndex) {
        const int64_t column = cacheLeftColumn + itemIndex * CacheElementWidth;

        const int64_t firstSample
            =static_cast<int64_t>(column * samplesPerPixel);

        const auto it = std::find_if(
            ppsMatchRange.first, ppsMatchRange.second,
            [firstSample](auto element)
        { return element.Key.FirstSample == firstSample; });

        if (it == ppsMatchRange.second) {
            mNewLookupItems.push_back({ pixelsPerSecond, firstSample });
        }
    }

    bool needsSmoothing = !mNewLookupItems.empty();

    ++mCacheAccessIndex;

    if (!CreateNewItems()) {
        DisposeNewItems();
        return {};
    }

    mLookupHelper.reserve(mLookup.size() + mNewLookupItems.size());

    std::merge(
        mLookup.begin(), mLookup.end(), mNewLookupItems.begin(),
        mNewLookupItems.end(), std::back_inserter(mLookupHelper),
        [sampleRate = mScaledSampleRate](auto lhs, auto rhs)
    { return IsKeyLess(sampleRate, lhs.Key, rhs.Key); });

    std::swap(mLookup, mLookupHelper);
    mLookupHelper.clear();

    // Find the very first item satisfying the range
    const GraphicsDataCacheKey firstItemKey {
        pixelsPerSecond, int64_t(cacheLeftColumn * samplesPerPixel)
    };

    auto it = FindKey(firstItemKey);

    assert(it != mLookup.end());

    if (it == mLookup.end()) {
        return {};
    }

    GraphicsDataCacheElementBase* prevItem = nullptr;

    for (int64_t itemIndex = 0; itemIndex < cacheItemsCount; ++itemIndex) {
        auto data = it->Data;

        data->LastCacheAccess = mCacheAccessIndex;
        data->AwaitsEviction  = false;

        if (!data->IsComplete && data->LastUpdate != mCacheAccessIndex) {
            if (!UpdateElement(it->Key, *data)) {
                return {};
            }

            needsSmoothing = true;
        }

        if (needsSmoothing) {
            data->Smooth(prevItem);
        }

        prevItem = data;

        ++it;
    }

    PerformCleanup();

    it        = FindKey(firstItemKey);
    auto last = it;

    std::advance(last, cacheItemsCount);

    return { it, last,
             static_cast<size_t>(std::max(int64_t(0), left - cacheLeftColumn)),
             static_cast<size_t>(
                 std::max(int64_t(0), cacheRightColumn - right)) };
}

const GraphicsDataCacheElementBase*
GraphicsDataCacheBase::PerformBaseLookup(GraphicsDataCacheKey key)
{
    auto it = FindKey(key);

    ++mCacheAccessIndex;

    if (it != mLookup.end()) {
        GraphicsDataCacheElementBase* data = it->Data;

        if (!data->IsComplete && data->LastUpdate != mCacheAccessIndex) {
            if (!UpdateElement(it->Key, *data)) {
                return {};
            }
        }

        data->Smooth(it == mLookup.begin() ? nullptr : (it - 1)->Data);

        return data;
    }

    mNewLookupItems.clear();
    mNewLookupItems.reserve(1);

    mNewLookupItems.push_back({ key, nullptr });

    LookupElement newElement { key, CreateElement(key) };

    if (newElement.Data == nullptr) {
        return nullptr;
    }

    newElement.Data->LastUpdate      = mCacheAccessIndex;
    newElement.Data->LastCacheAccess = mCacheAccessIndex;
    newElement.Data->AwaitsEviction  = false;

    auto insertedPosition = mLookup.insert(
        std::upper_bound(
            mLookup.begin(), mLookup.end(), key,
            [sampleRate = mScaledSampleRate](auto lhs, auto rhs)
    {
        if constexpr (std::is_same_v<
                          std::decay_t<decltype(lhs)>, GraphicsDataCacheKey>) {
            return IsKeyLess(sampleRate, lhs, rhs.Key);
        } else {
            return IsKeyLess(sampleRate, lhs.Key, rhs);
        }
    }),
        newElement);

    newElement.Data->Smooth(
        insertedPosition == mLookup.begin() ? nullptr
        : (insertedPosition - 1)->Data);

    PerformCleanup();

    return newElement.Data;
}

bool GraphicsDataCacheBase::CreateNewItems()
{
    for (auto& item : mNewLookupItems) {
        item.Data = CreateElement(item.Key);

        if (item.Data == nullptr) {
            return false;
        }

        item.Data->LastUpdate = mCacheAccessIndex;
    }

    return true;
}

void GraphicsDataCacheBase::DisposeNewItems()
{
    std::for_each(
        mNewLookupItems.begin(), mNewLookupItems.end(),
        [](auto elem)
    {
        if (elem.Data != nullptr) {
            elem.Data->Dispose();
        }
    });
}

GraphicsDataCacheBase::Lookup::iterator
GraphicsDataCacheBase::FindKey(GraphicsDataCacheKey key)
{
    return std::find_if(
        mLookup.begin(), mLookup.end(),
        [sampleRate = mScaledSampleRate, key](auto lhs)
    { return IsSameKey(sampleRate, lhs.Key, key); });
}

void GraphicsDataCacheBase::PerformCleanup()
{
    const int64_t lookupSize = static_cast<int64_t>(mLookup.size());

    const auto allowedItems
        =RoundUpUnsafe(mMaxWidth, CacheElementWidth) * mCacheSizeMultiplier;

    const int64_t itemsToEvict = lookupSize - allowedItems;

    if (itemsToEvict <= 0) {
        return;
    }

    if (itemsToEvict == 1) {
        auto it = std::min_element(
            mLookup.begin(), mLookup.end(),
            [](auto lhs, auto rhs)
        { return lhs.Data->LastCacheAccess < rhs.Data->LastCacheAccess; });

        if (it->Data->LastCacheAccess < mCacheAccessIndex) {
            DisposeElement(it->Data);
            mLookup.erase(it);
        }
    } else {
        PerformFullCleanup(lookupSize, itemsToEvict);
    }
}

void GraphicsDataCacheBase::PerformFullCleanup(
    int64_t currentSize, int64_t itemsToEvict)
{
    mLRUHelper.reserve(currentSize);

    for (size_t i = 0; i < currentSize; ++i) {
        mLRUHelper.push_back(i);
    }

    std::make_heap(
        mLRUHelper.begin(), mLRUHelper.end(),
        [this](size_t lhs, size_t rhs)
    {
        return mLookup[lhs].Data->LastCacheAccess
               > mLookup[rhs].Data->LastCacheAccess;
    });

    for (int64_t itemIndex = 0; itemIndex < itemsToEvict; ++itemIndex) {
        std::pop_heap(mLRUHelper.begin(), mLRUHelper.end());

        const size_t index = mLRUHelper.back();
        mLRUHelper.pop_back();

        auto data = mLookup[index].Data;

        if (data->LastCacheAccess >= mCacheAccessIndex) {
            break;
        }

        DisposeElement(data);
        data->AwaitsEviction = true;
    }

    mLookup.erase(
        std::remove_if(
            mLookup.begin(), mLookup.end(),
            [](auto item) { return item.Data->AwaitsEviction; }),
        mLookup.end());

    mLRUHelper.clear();
}
