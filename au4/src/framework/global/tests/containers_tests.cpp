/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include <gtest/gtest.h>

#include "containers.h"

using namespace mu;

class Global_Types_ContainersTests : public ::testing::Test
{
public:
};

TEST_F(Global_Types_ContainersTests, MoveItem)
{
    // [GIVEN]
    std::vector<int> v { 1, 2, 3, 4, 5, 6, 7, 8, 9 };

    // [WHEN]
    moveItem(v, 1, 4);
    moveItem(v, 5, 1);
    moveItem(v, 3, 3);
    moveItem(v, 6, 12);

    // [THEN]
    std::vector<int> expected { 1, 6, 3, 4, 5, 2, 8, 9, 7 };
    EXPECT_EQ(v, expected);
}

TEST_F(Global_Types_ContainersTests, FindLessOrEqual)
{
    // [GIVEN]
    std::map<int, std::string> map {
        { 10, "value 1" },
        { 1, "value 2" },
        { 5, "value 3" },
        { 20, "value 4" },
        { 3, "value 5" }
    };

    // [WHEN] Trying to find a key that is less than the given key
    auto it = findLessOrEqual(map, 6);

    // [THEN] The first key that is less than 6 is 5
    EXPECT_EQ(it->first, 5);
    EXPECT_EQ(it->second, "value 3");

    // [WHEN] Trying to find a key equal to the given key
    it = findLessOrEqual(map, 20);

    // [THEN] There is the same key in the map
    EXPECT_EQ(it->first, 20);
    EXPECT_EQ(it->second, "value 4");

    // [WHEN] Trying to find a key that is not in the map
    it = findLessOrEqual(map, -10);

    // [THEN] Return cend()
    EXPECT_EQ(it, map.cend());
}
