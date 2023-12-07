/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: StringUtils.h
 * SPDX-FileContributor: Dmitry Vedenko
 */

#include <catch2/catch.hpp>
#include "StringUtils.h"

TEST_CASE ("Join", "[StringUtils]")
{
   SECTION ("Join empty container")
   {
      std::vector<std::string> container;
      REQUIRE(Join(container, ",") == "");
   }

   SECTION ("Join single item")
   {
      std::vector<std::string> container { "item" };
      REQUIRE(Join(container, ",") == "item");
   }

   SECTION ("Join multiple items")
   {
      std::vector<std::string> container { "item1", "item2", "item3" };
      REQUIRE(Join(container, ",") == "item1,item2,item3");
   }
}
