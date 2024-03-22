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

#include "types/val.h"

using namespace mu;

class Global_ValTests : public ::testing::Test
{
public:
};

//Undefined
//Bool,
//Int,
//Double,
//String,
//Color

TEST_F(Global_ValTests, Val_Undefined)
{
    //! GIVEN Undefined value

    Val v;

    EXPECT_EQ(v.type(), Val::Type::Undefined);
    EXPECT_EQ(v.isNull(), true);
    EXPECT_EQ(v.toBool(), false);
    EXPECT_EQ(v.toInt(), 0);
    EXPECT_EQ(v.toDouble(), 0.0);
    EXPECT_EQ(v.toString(), std::string());
    EXPECT_EQ(v.toQColor(), QColor());
}

TEST_F(Global_ValTests, Val_Bool)
{
    //! GIVEN Value as bool

    Val v(true);

    EXPECT_EQ(v.type(), Val::Type::Bool);
    EXPECT_EQ(v.isNull(), false);
    EXPECT_EQ(v.toBool(), true);
    EXPECT_EQ(v.toInt(), 1);
    EXPECT_EQ(v.toDouble(), 1.0);
    EXPECT_EQ(v.toString(), "true");
    EXPECT_EQ(v.toQColor(), QColor());
}

TEST_F(Global_ValTests, Val_Int)
{
    //! GIVEN Value as int

    Val v(42);

    EXPECT_EQ(v.type(), Val::Type::Int);
    EXPECT_EQ(v.isNull(), false);
    EXPECT_EQ(v.toBool(), true);
    EXPECT_EQ(v.toInt(), 42);
    EXPECT_EQ(v.toDouble(), 42.0);
    EXPECT_EQ(v.toString(), "42");
    EXPECT_EQ(v.toQColor(), QColor());
}

TEST_F(Global_ValTests, Val_Double)
{
    //! GIVEN Value as double

    Val v(42.42);

    EXPECT_EQ(v.type(), Val::Type::Double);
    EXPECT_EQ(v.isNull(), false);
    EXPECT_EQ(v.toBool(), true);
    EXPECT_EQ(v.toInt(), 42);
    EXPECT_EQ(v.toDouble(), 42.42);
    EXPECT_EQ(v.toString(), "42.42");
    EXPECT_EQ(v.toQColor(), QColor());
}

TEST_F(Global_ValTests, Val_String)
{
    //! GIVEN Value as string

    Val v("hello");

    EXPECT_EQ(v.type(), Val::Type::String);
    EXPECT_EQ(v.isNull(), false);
    EXPECT_EQ(v.toBool(), false);
    EXPECT_EQ(v.toString(), "hello");
    EXPECT_EQ(v.toQColor(), QColor());
}

TEST_F(Global_ValTests, Val_Color)
{
    //! GIVEN Value as color

    Val v(QColor("#800000"));

    EXPECT_EQ(v.type(), Val::Type::Color);
    EXPECT_EQ(v.isNull(), false);
    EXPECT_EQ(v.toBool(), false);
    EXPECT_EQ(v.toString(), "#800000");
    EXPECT_EQ(v.toQColor(), QColor("#800000"));
}
