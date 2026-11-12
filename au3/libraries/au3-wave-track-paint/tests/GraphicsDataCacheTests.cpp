/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

 Audacity: A Digital Audio Editor

 GraphicsDataCacheTests.cpp

 Dmitry Vedenko
 **********************************************************************/

#include <catch2/catch.hpp>

#include <memory>
#include <string>
#include <string_view>
#include <type_traits>

#include "GraphicsDataCache.h"
#include "au3-screen-geometry/ZoomInfo.h"

namespace {
struct CacheElement : GraphicsDataCacheElementBase
{
    CacheElement& operator=(GraphicsDataCacheKey key)
    {
        Key = key;
        return *this;
    }

    void Dispose() override
    {
        Key = {};
    }

    GraphicsDataCacheKey Key;
};

class InvalidationTestCache final : public GraphicsDataCache<CacheElement>
{
public:
    InvalidationTestCache()
        : GraphicsDataCache<CacheElement>(44100, [] { return std::make_unique<CacheElement>(); })
    {
    }

    using GraphicsDataCacheBase::InvalidatePixelRange;
    using GraphicsDataCacheBase::InvalidateOtherZoomLevels;

    size_t initializations = 0;

private:
    bool InitializeElement(const GraphicsDataCacheKey& key, CacheElement& element) override
    {
        element = key;
        element.IsComplete = true;
        ++initializations;
        return true;
    }
};

size_t InitializationsDuringLookup(InvalidationTestCache& cache, const ZoomInfo& zoomInfo, double t0, double t1)
{
    const size_t before = cache.initializations;
    REQUIRE(!cache.PerformLookup(zoomInfo, t0, t1).empty());
    return cache.initializations - before;
}

void CheckCacheElementLookup(GraphicsDataCache<CacheElement>& cache, const ZoomInfo& zoomInfo, double t0, double t1, size_t count)
{
    auto range = cache.PerformLookup(zoomInfo, t0, t1);

    REQUIRE(count == range.size());

    if (count == 0) {
        return;
    }

    const size_t t0Pixel = zoomInfo.TimeToPosition(t0);
    const size_t t0Bin = t0Pixel / GraphicsDataCacheBase::CacheElementWidth;

    const size_t leftOffset
        =t0Pixel - t0Bin * GraphicsDataCacheBase::CacheElementWidth;

    const size_t rightOffset
        =(t0Bin + count) * GraphicsDataCacheBase::CacheElementWidth
          - zoomInfo.TimeToPosition(t1) - 1;

    const auto age = range.first->LastCacheAccess;

    if (count == 1) {
        REQUIRE(range.first.GetLeftOffset() == leftOffset);
        REQUIRE(range.first.GetRightOffset() == rightOffset);
    } else {
        REQUIRE(range.first.GetLeftOffset() == leftOffset);
        REQUIRE(range.first.GetRightOffset() == 0);

        auto it = range.first;
        ++it;

        while (--count > 1)
        {
            REQUIRE(it.GetLeftOffset() == 0);
            REQUIRE(it.GetRightOffset() == 0);
            REQUIRE(it->LastCacheAccess == age);

            ++it;
        }

        REQUIRE(it.GetLeftOffset() == 0);
        REQUIRE(it.GetRightOffset() == rightOffset);
        REQUIRE(it->LastCacheAccess == age);
    }
}
} // namespace

TEST_CASE("graphics-data-cache", "")
{
    ZoomInfo info(0.0, ZoomInfo::GetDefaultZoom());

    GraphicsDataCache<CacheElement> cache(44100, [](){ return std::make_unique<CacheElement>(); });

    REQUIRE(cache.PerformLookup(info, 0, 0).empty());
    REQUIRE(!cache.PerformLookup(info, 0, 1).empty());

    REQUIRE(cache.PerformLookup(info, 0, 1).first->LastCacheAccess == 2);

    CheckCacheElementLookup(cache, info, 0.0, 1.0, 1);
    CheckCacheElementLookup(cache, info, 0.0, 2.0, 1);
    CheckCacheElementLookup(cache, info, 0.0, 3.0, 2);
    CheckCacheElementLookup(cache, info, 0.0, 4.0, 2);
    CheckCacheElementLookup(cache, info, 0.0, 5.0, 2);
    CheckCacheElementLookup(cache, info, 0.0, 6.0, 3);

    CheckCacheElementLookup(cache, info, 0.5, 1.0, 1);

    for (int i = 0; i < 128; ++i) {
        const double t0 = i;
        const double t1 = t0 + 2;

        const int64_t itemsCount = 1
                                   + info.TimeToPosition(t1) / GraphicsDataCacheBase::CacheElementWidth
                                   - info.TimeToPosition(t0) / GraphicsDataCacheBase::CacheElementWidth;

        CheckCacheElementLookup(cache, info, t0, t1, itemsCount);
    }
}

TEST_CASE("graphics-data-cache-invalidation", "")
{
    const ZoomInfo zoomA(0.0, 100.0);
    const ZoomInfo zoomB(0.0, 200.0);

    InvalidationTestCache cache;

    REQUIRE(InitializationsDuringLookup(cache, zoomA, 0.0, 10.0) > 0);
    REQUIRE(InitializationsDuringLookup(cache, zoomB, 0.0, 10.0) > 0);

    SECTION("Cached elements are reused")
    {
        REQUIRE(InitializationsDuringLookup(cache, zoomA, 0.0, 10.0) == 0);
        REQUIRE(InitializationsDuringLookup(cache, zoomB, 0.0, 10.0) == 0);
    }

    SECTION("Pixel range invalidation touches only the given zoom and range")
    {
        // 1000 pixels at zoom A span 4 elements; pixels [300, 400) lie in the second one
        cache.InvalidatePixelRange(zoomA.GetZoom(), 300, 400);

        REQUIRE(InitializationsDuringLookup(cache, zoomA, 0.0, 10.0) == 1);
        REQUIRE(InitializationsDuringLookup(cache, zoomB, 0.0, 10.0) == 0);
    }

    SECTION("Other zoom levels are invalidated, the given one is kept")
    {
        cache.InvalidateOtherZoomLevels(zoomB.GetZoom());

        REQUIRE(InitializationsDuringLookup(cache, zoomB, 0.0, 10.0) == 0);
        REQUIRE(InitializationsDuringLookup(cache, zoomA, 0.0, 10.0) == 4);
    }
}
