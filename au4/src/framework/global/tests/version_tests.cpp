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

#include "types/version.h"

using namespace mu;

class Global_Types_VersionTests : public ::testing::Test
{
public:
};

TEST_F(Global_Types_VersionTests, Compare_Major)
{
    Version v1(u"1.0.0");
    Version v2(u"2.0.0");

    EXPECT_TRUE(v1 < v2);
}

TEST_F(Global_Types_VersionTests, Compare_Minor)
{
    Version v1(u"1.0.0");
    Version v2(u"1.1.0");

    EXPECT_TRUE(v1 < v2);
}

TEST_F(Global_Types_VersionTests, Compare_Patch)
{
    Version v1(u"1.0.0");
    Version v2(u"1.0.1");

    EXPECT_TRUE(v1 < v2);
}

TEST_F(Global_Types_VersionTests, Compare_Alpha_Alpha)
{
    Version v1(u"1.0.0-alpha.1");
    Version v2(u"1.0.0-alpha.2");

    EXPECT_TRUE(v1 < v2);
}

TEST_F(Global_Types_VersionTests, Compare_Alpha_Beta)
{
    Version v1(u"1.0.0-alpha");
    Version v2(u"1.0.0-beta");

    EXPECT_TRUE(v1 < v2);
}

TEST_F(Global_Types_VersionTests, Compare_Alpha_RC)
{
    Version v1(u"1.0.0-alpha");
    Version v2(u"1.0.0-rc");

    EXPECT_TRUE(v1 < v2);
}

TEST_F(Global_Types_VersionTests, Compare_Alpha_Release)
{
    Version v1(u"1.0.0-alpha");
    Version v2(u"1.0.0");

    EXPECT_TRUE(v1 < v2);
}

TEST_F(Global_Types_VersionTests, Compare_Beta_Beta)
{
    Version v1(u"1.0.0-beta.1");
    Version v2(u"1.0.0-beta.2");

    EXPECT_TRUE(v1 < v2);
}

TEST_F(Global_Types_VersionTests, Compare_Beta_RC)
{
    Version v1(u"1.0.0-beta");
    Version v2(u"1.0.0-rc");

    EXPECT_TRUE(v1 < v2);
}

TEST_F(Global_Types_VersionTests, Compare_Beta_Release)
{
    Version v1(u"1.0.0-beta");
    Version v2(u"1.0.0");

    EXPECT_TRUE(v1 < v2);
}

TEST_F(Global_Types_VersionTests, Compare_RC_RC)
{
    Version v1(u"1.0.0-rc.1");
    Version v2(u"1.0.0-rc.2");

    EXPECT_TRUE(v1 < v2);
}

TEST_F(Global_Types_VersionTests, Compare_RC_Release)
{
    Version v1(u"1.0.0-rc");
    Version v2(u"1.0.0");

    EXPECT_TRUE(v1 < v2);
}

TEST_F(Global_Types_VersionTests, Compare_Equal)
{
    Version v1(u"1.0");
    Version v2(u"1");

    EXPECT_TRUE(v1 == v2);

    v1 = Version(u"1.0.0-rc");
    v2 = Version(u"1.0.0-rc");

    EXPECT_TRUE(v1 == v2);
}
