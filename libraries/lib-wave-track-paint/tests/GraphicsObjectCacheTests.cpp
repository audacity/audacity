/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

 Audacity: A Digital Audio Editor

 GraphicsDataCacheTests.cpp

 Dmitry Vedenko
 **********************************************************************/

#include <catch2/catch.hpp>

#include <string>
#include <random>

#include "GraphicsObjectCache.h"

TEST_CASE("graphics-object-cache", "")
{
   GraphicsObjectCache<size_t, size_t> cache([](size_t idx)
                                             { return idx * idx; });

   std::default_random_engine generator;
   std::uniform_int_distribution<size_t> distribution(1, 256);
   
   for (size_t i = 0; i < 4096; ++i)
   {
      const auto key = distribution(generator);
      REQUIRE(cache.Get(key) == key * key);
   }

   GraphicsObjectCache<size_t, size_t, 16, false> cacheNoLookup(
      [](size_t idx) { return idx * idx; });

   for (size_t i = 0; i < 4096; ++i)
   {
      const auto key = distribution(generator);
      REQUIRE(cacheNoLookup.Get(key) == key * key);
   }
}
