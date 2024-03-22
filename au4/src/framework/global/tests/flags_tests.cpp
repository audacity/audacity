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

#include "types/flags.h"

using namespace mu;

class Global_Types_FlagsTests : public ::testing::Test
{
public:

    enum Option {
        NoOption = 0x0,
        Option1 = 0x1,
        Option2 = 0x2,
        Option3 = 0x4
    };
    DECLARE_FLAGS(Options, Option)
};

TEST_F(Global_Types_FlagsTests, Flags)
{
    //! GIVE
    Options opts(Option1 | Option2);
    //! CHECK
    EXPECT_TRUE(opts.testFlag(Option1));
    EXPECT_TRUE(opts.testFlag(Option2));
    EXPECT_FALSE(opts.testFlag(Option3));
}
