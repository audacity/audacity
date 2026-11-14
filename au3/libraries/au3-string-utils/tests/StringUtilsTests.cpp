/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: StringUtils.h
 * SPDX-FileContributor: Dmitry Vedenko
 */

#include "StringUtils.h"
#include <catch2/catch.hpp>

TEST_CASE("Join", "[StringUtils]")
{
    SECTION("Join empty container")
    {
        std::vector<std::string> container;
        REQUIRE(Join(container, ",") == "");
    }

    SECTION("Join single item")
    {
        std::vector<std::string> container { "item" };
        REQUIRE(Join(container, ",") == "item");
    }

    SECTION("Join multiple items")
    {
        std::vector<std::string> container { "item1", "item2", "item3" };
        REQUIRE(Join(container, ",") == "item1,item2,item3");
    }
}

TEST_CASE("IsPrefixed", "[StringUtils]")
{
    SECTION("Empty prefix")
    {
        REQUIRE(IsPrefixed("test", ""));
    }

    SECTION("Empty string")
    {
        REQUIRE_FALSE(IsPrefixed("", "test"));
    }

    SECTION("Prefix longer than string")
    {
        REQUIRE_FALSE(IsPrefixed("test", "test1"));
    }

    SECTION("Prefix is not a prefix")
    {
        REQUIRE_FALSE(IsPrefixed("test", "abc"));
    }

    SECTION("Prefix is a prefix")
    {
        REQUIRE(IsPrefixed("test", "tes"));
    }

    SECTION("Prefix matches string")
    {
        REQUIRE(IsPrefixed("test", "test"));
    }

    SECTION("Case insensitive")
    {
        REQUIRE(IsPrefixedInsensitive("test", "TEST"));
    }
}
