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

#include "types/datetime.h"

using namespace mu;

class Global_Types_DateTimeTests : public ::testing::Test
{
public:
};

TEST_F(Global_Types_DateTimeTests, DateTime_To_From_String)
{
    // to ISO
    {
        //! GIVEN Some date
        DateTime dt(Date(2022, 6, 24), Time(14, 27, 35));
        //! DO
        String str = dt.toString(DateFormat::ISODate);
        //! CHECK
        EXPECT_EQ(str, u"2022-06-24T14:27:35");
    }

    // from ISO
    {
        //! GIVEN Some str date
        String str = u"2022-06-24T14:27:35";
        //! DO
        DateTime dt = DateTime::fromStringISOFormat(str);
        //! CHECK
        EXPECT_EQ(dt, DateTime(Date(2022, 6, 24), Time(14, 27, 35)));
    }

    // to Local short (C locale)
    {
        std::locale::global(std::locale());
        //! GIVEN Some date
        DateTime dt(Date(2022, 6, 24), Time(14, 27, 35));
        //! DO
        String str = dt.toString(DateFormat::LocaleShortFormat);
        //! CHECK
        EXPECT_EQ(str, u"06/24/22 14:27");
    }
}

TEST_F(Global_Types_DateTimeTests, Date_To_From_String)
{
    // to ISO
    {
        //! GIVEN Some date
        Date d(2022, 6, 24);
        //! DO
        String str = d.toString(DateFormat::ISODate);
        //! CHECK
        EXPECT_EQ(str, u"2022-06-24");
    }

    // from ISO
    {
        //! GIVEN Some str date
        String str = u"2022-06-24";
        //! DO
        Date d = Date::fromStringISOFormat(str);
        //! CHECK
        EXPECT_EQ(d, Date(2022, 6, 24));
    }

    // to Local short (C locale)
    {
        std::locale::global(std::locale());
        //! GIVEN Some date
        Date d(2022, 6, 24);
        //! DO
        String str = d.toString(DateFormat::LocaleShortFormat);
        //! CHECK
        EXPECT_EQ(str, u"06/24/22");
    }
}

TEST_F(Global_Types_DateTimeTests, Time_To_From_String)
{
    // to ISO
    {
        //! GIVEN Some time
        Time t(14, 27, 35);
        //! DO
        String str = t.toString(DateFormat::ISODate);
        //! CHECK
        EXPECT_EQ(str, u"14:27:35");
    }

    // from ISO
    {
        //! GIVEN Some str time
        String str = u"14:27:35";
        //! DO
        Time t = Time::fromStringISOFormat(str);
        //! CHECK
        EXPECT_EQ(t, Time(14, 27, 35));
    }

    // to Local short (C locale)
    {
        std::locale::global(std::locale());
        //! GIVEN Some time
        Time t(14, 27, 35);
        //! DO
        String str = t.toString(DateFormat::LocaleShortFormat);
        //! CHECK
        EXPECT_EQ(str, u"14:27");
    }
}
