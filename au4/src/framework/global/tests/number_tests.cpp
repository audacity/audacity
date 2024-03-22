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

#include "global/types/number.h"

using namespace mu;

class Global_Types_NumberTests : public ::testing::Test
{
public:
};

TEST_F(Global_Types_NumberTests, IsZero)
{
    //! GIVEN number
    real_t v = 1.0;
    EXPECT_FALSE(v.is_zero());

    v = 0.1;
    EXPECT_FALSE(v.is_zero());

    v = 0.000001;
    EXPECT_FALSE(v.is_zero());

    v = 0.0000099;
    EXPECT_FALSE(v.is_zero());

    v = 0.0000001;
    EXPECT_TRUE(v.is_zero());
}

TEST_F(Global_Types_NumberTests, IsEqual)
{
    //! GIVEN number
    real_t v1 = 1.0;
    real_t v2 = 1.0;
    EXPECT_EQ(v1, v2);

    v1 = 1.0;
    v2 = 2.0;
    EXPECT_NE(v1, v2);

    v1 = 1.0000001;
    v2 = 1.0000001;
    EXPECT_EQ(v1, v2);

    v1 = 1.000001;
    v2 = 1.000002;
    EXPECT_NE(v1, v2);

    v1 = 1.0000001;
    v2 = 1.0000002;
    EXPECT_EQ(v1, v2);
}
