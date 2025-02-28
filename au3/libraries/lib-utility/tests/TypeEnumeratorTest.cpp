/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TypeEnumerator.cpp
  @brief compilation-only tests as examples of use of TypeEnumerator

  Paul Licameli

**********************************************************************/
#include <catch2/catch.hpp>
#include "TypeEnumerator.h"

using namespace TypeEnumerator;
using namespace TypeList;

BEGIN_TYPE_ENUMERATION(Tag1)
BEGIN_TYPE_ENUMERATION(Tag2)

ENUMERATE_TYPE(Tag1, void)
ENUMERATE_TYPE(Tag2, char)

TEST_CASE("Using different tags")
{
    struct Here1 : Tag1 {};
    struct Here2 : Tag2 {};
    static_assert(std::is_same_v<List<void>,
                                 CollectTypes<Tag1, Here1>::type>);
    static_assert(std::is_same_v<List<char>,
                                 CollectTypes<Tag2, Here2>::type>);
}

ENUMERATE_TYPE(Tag1, const volatile int&)
ENUMERATE_TYPE(Tag2, long)

TEST_CASE("Dependency on location")
{
    struct Here1 : Tag1 {};
    struct Here2 : Tag2 {};
    static_assert(std::is_same_v<List<void, const volatile int&>,
                                 CollectTypes<Tag1, Here1>::type>);
    static_assert(std::is_same_v<List<char, long>,
                                 CollectTypes<Tag2, Here2>::type>);
}

ENUMERATE_TYPE(Tag1, const volatile int&)
ENUMERATE_TYPE(Tag2, long)

TEST_CASE("Distinct types, only")
{
    struct Here1 : Tag1 {};
    struct Here2 : Tag2 {};
    static_assert(std::is_same_v<List<void, const volatile int&>,
                                 CollectTypes<Tag1, Here1>::type>);
    static_assert(std::is_same_v<List<char, long>,
                                 CollectTypes<Tag2, Here2>::type>);
}
