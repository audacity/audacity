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

#include "logremover.h"

namespace mu {
class Global_LogRemoverTests : public ::testing::Test
{
public:
};

TEST_F(Global_LogRemoverTests, ParseDate)
{
    // Correct name format: *_yyMMdd*
    EXPECT_EQ(LogRemover::parseDate(u"MuseScore_210229_154033.log"), Date(2021, 2, 29));
    EXPECT_EQ(LogRemover::parseDate(u"MuseScore_390910_154033"), Date(2039, 9, 10));
    EXPECT_EQ(LogRemover::parseDate(u"MuseScore_290910-_154033__"), Date(2029, 9, 10));
    EXPECT_EQ(LogRemover::parseDate(u"audiopluginregistration_230615.log"), Date(2023, 6, 15));

    // Incorrect name format: missing _ or wrong position
    EXPECT_EQ(LogRemover::parseDate(u"MuseScore210229154033.log"), Date());
    EXPECT_EQ(LogRemover::parseDate(u"MuseScore210229154033_.log"), Date());
    EXPECT_EQ(LogRemover::parseDate(u"_MuseScore_210229_154033.log"), Date());

    // Incorrect name format: missing/wrong date
    EXPECT_EQ(LogRemover::parseDate(u"MuseScore_"), Date());
    EXPECT_EQ(LogRemover::parseDate(u"MuseScore_00229_154033"), Date()); // wrong year
    EXPECT_EQ(LogRemover::parseDate(u"MuseScore_219929_154033.log"), Date()); // wrong month
    EXPECT_EQ(LogRemover::parseDate(u"MuseScore_210299_154033.log"), Date()); // wrong day
}
}
