/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: UriParser.cpp
 * SPDX-FileContributor: Dmitry Vedenko
 */

#include <catch2/catch.hpp>

#include "UriParser.h"

TEST_CASE("ParseUri", "[StringUtils]")
{
    SECTION("Empty URI")
    {
        auto result = ParseUri("");
        REQUIRE(result.Scheme.empty());
        REQUIRE(result.UserInfo.empty());
        REQUIRE(result.Host.empty());
        REQUIRE(result.Port.empty());
        REQUIRE(result.Path.empty());
        REQUIRE(result.Query.empty());
        REQUIRE(result.Fragment.empty());
    }

    SECTION("URI without scheme")
    {
        auto result = ParseUri("test");
        REQUIRE(result.Scheme.empty());
        REQUIRE(result.UserInfo.empty());
        REQUIRE(result.Host == "test");
        REQUIRE(result.Port.empty());
        REQUIRE(result.Path.empty());
        REQUIRE(result.Query.empty());
        REQUIRE(result.Fragment.empty());
    }

    SECTION("URI with scheme")
    {
        auto result = ParseUri("http://test");
        REQUIRE(result.Scheme == "http");
        REQUIRE(result.UserInfo.empty());
        REQUIRE(result.Host == "test");
        REQUIRE(result.Port.empty());
        REQUIRE(result.Path.empty());
        REQUIRE(result.Query.empty());
        REQUIRE(result.Fragment.empty());
    }

    SECTION("URI with scheme and fragment")
    {
        auto result = ParseUri("http://test#fragment");
        REQUIRE(result.Scheme == "http");
        REQUIRE(result.UserInfo.empty());
        REQUIRE(result.Host == "test");
        REQUIRE(result.Port.empty());
        REQUIRE(result.Path.empty());
        REQUIRE(result.Query.empty());
        REQUIRE(result.Fragment == "fragment");
    }

    SECTION("URI with scheme and query")
    {
        auto result = ParseUri("http://test?query");
        REQUIRE(result.Scheme == "http");
        REQUIRE(result.UserInfo.empty());
        REQUIRE(result.Host == "test");
        REQUIRE(result.Port.empty());
        REQUIRE(result.Path.empty());
        REQUIRE(result.Query == "query");
        REQUIRE(result.Fragment.empty());
    }

    SECTION("URI with scheme and path")
    {
        auto result = ParseUri("http://test/path");
        REQUIRE(result.Scheme == "http");
        REQUIRE(result.UserInfo.empty());
        REQUIRE(result.Host == "test");
        REQUIRE(result.Port.empty());
        REQUIRE(result.Path == "path");
        REQUIRE(result.Query.empty());
        REQUIRE(result.Fragment.empty());
    }

    SECTION("URI with scheme, path and query")
    {
        auto result = ParseUri("http://test/path?query");
        REQUIRE(result.Scheme == "http");
        REQUIRE(result.UserInfo.empty());
        REQUIRE(result.Host == "test");
        REQUIRE(result.Port.empty());
        REQUIRE(result.Path == "path");
        REQUIRE(result.Query == "query");
        REQUIRE(result.Fragment.empty());
    }

    SECTION("URI with scheme, path and fragment")
    {
        auto result = ParseUri("http://test/path#fragment");
        REQUIRE(result.Scheme == "http");
        REQUIRE(result.UserInfo.empty());
        REQUIRE(result.Host == "test");
        REQUIRE(result.Port.empty());
        REQUIRE(result.Path == "path");
        REQUIRE(result.Query.empty());
        REQUIRE(result.Fragment == "fragment");
    }

    SECTION("URI with scheme, path, query and fragment")
    {
        auto result = ParseUri("http://test/path?query#fragment");
        REQUIRE(result.Scheme == "http");
        REQUIRE(result.UserInfo.empty());
        REQUIRE(result.Host == "test");
        REQUIRE(result.Port.empty());
        REQUIRE(result.Path == "path");
        REQUIRE(result.Query == "query");
        REQUIRE(result.Fragment == "fragment");
    }

    SECTION("URI with scheme, user info, host, port, path, query and fragment")
    {
        auto result
            =ParseUri("http://user:password@test:1234/path?query#fragment");
        REQUIRE(result.Scheme == "http");
        REQUIRE(result.UserInfo == "user:password");
        REQUIRE(result.Host == "test");
        REQUIRE(result.Port == "1234");
        REQUIRE(result.Path == "path");
        REQUIRE(result.Query == "query");
        REQUIRE(result.Fragment == "fragment");
    }
}

TEST_CASE("ParseUriQuery", "[StringUtils]")
{
    SECTION("Empty query")
    {
        auto result = ParseUriQuery("");
        REQUIRE(result.empty());
    }

    SECTION("Query without delimiter")
    {
        auto result = ParseUriQuery("test");
        REQUIRE(result.size() == 1);
        REQUIRE(result["test"].empty());
    }

    SECTION("Query with single item")
    {
        auto result = ParseUriQuery("test=value");
        REQUIRE(result.size() == 1);
        REQUIRE(result["test"] == "value");
    }

    SECTION("Query with multiple items")
    {
        auto result = ParseUriQuery("test1=value1&test2=value2");
        REQUIRE(result.size() == 2);
        REQUIRE(result["test1"] == "value1");
        REQUIRE(result["test2"] == "value2");
    }

    SECTION("Query with multiple items without values")
    {
        auto result = ParseUriQuery("test1&test2");
        REQUIRE(result.size() == 2);
        REQUIRE(result["test1"].empty());
        REQUIRE(result["test2"].empty());
    }

    SECTION("Query with multiple mixed items")
    {
        auto result = ParseUriQuery("test1&test2=value&test3");
        REQUIRE(result.size() == 3);
        REQUIRE(result["test1"].empty());
        REQUIRE(result["test2"] == "value");
        REQUIRE(result["test3"].empty());
    }

    SECTION("Query with multiple items and custom delimiter")
    {
        auto result = ParseUriQuery("test1=value1;test2=value2", ";");
        REQUIRE(result.size() == 2);
        REQUIRE(result["test1"] == "value1");
        REQUIRE(result["test2"] == "value2");
    }
}
