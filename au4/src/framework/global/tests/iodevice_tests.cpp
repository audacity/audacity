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

#include <cstring>

#include "io/buffer.h"

#ifdef NDEBUG
#include "log.h"
#endif

using namespace mu;
using namespace mu::io;

class Global_IO_IODeviceTests : public ::testing::Test
{
public:
};

TEST_F(Global_IO_IODeviceTests, Open_ReadOnly)
{
    //! GIVEN Some buffer
    std::string ref = "Hello";
    ByteArray ba(reinterpret_cast<const uint8_t*>(ref.c_str()), ref.size());
    Buffer buf(&ba);

    //! DO Open ReadOnly
    EXPECT_TRUE(buf.open(IODevice::ReadOnly));

    //! DO Read
    ByteArray rba = buf.readAll();

    //! CHECK
    EXPECT_EQ(rba, ba);

#ifndef NDEBUG
    //! DO Try Write data
    std::string data = " World!";
    ByteArray wrba(reinterpret_cast<const uint8_t*>(data.c_str()), data.size());

    //! CHECK Should assert
    EXPECT_DEATH({
        size_t size = buf.write(wrba);
        EXPECT_EQ(size, 0);
    }, ".*isOpenModeWriteable\\(\\).*");
#else
    LOGW() << "Cannot check for assertion failure in Release mode; please build in Debug mode instead";
#endif
}

TEST_F(Global_IO_IODeviceTests, Open_WriteOnly)
{
    //! GIVEN Some buffer
    std::string ref = "Hello";
    ByteArray ba(reinterpret_cast<const uint8_t*>(ref.c_str()), ref.size());
    Buffer buf(&ba);

    //! DO Open WriteOnly
    EXPECT_TRUE(buf.open(IODevice::WriteOnly));

    //! DO Write data
    std::string data = " World!";
    ByteArray wrba(reinterpret_cast<const uint8_t*>(data.c_str()), data.size());
    size_t size = buf.write(wrba);

    //! CHECK
    EXPECT_EQ(size, wrba.size());

#ifndef NDEBUG
    //! DO Try Read
    EXPECT_DEATH({
        ByteArray rba = buf.readAll();
        EXPECT_TRUE(rba.empty());
    }, ".*isOpenModeReadable\\(\\).*");

    EXPECT_DEATH({
        ByteArray rba = buf.read(4);
        EXPECT_TRUE(rba.empty());
    }, ".*isOpenModeReadable\\(\\).*");

    EXPECT_DEATH({
        uint8_t d = 0;
        size_t s = buf.read(&d, 1);
        EXPECT_EQ(s, 0);
    }, ".*isOpenModeReadable\\(\\).*");
#else
    LOGW() << "Cannot check for assertion failure in Release mode; please build in Debug mode instead";
#endif
}

TEST_F(Global_IO_IODeviceTests, Open_ReadWrite)
{
    //! GIVEN Some buffer
    std::string ref = "Hello";
    ByteArray ba(reinterpret_cast<const uint8_t*>(ref.c_str()), ref.size());
    Buffer buf(&ba);

    //! DO Open ReadWrite
    EXPECT_TRUE(buf.open(IODevice::ReadWrite));

    //! DO Write data
    std::string data = "World!";
    ByteArray wrba(reinterpret_cast<const uint8_t*>(data.c_str()), data.size());
    size_t size = buf.write(wrba);

    //! CHECK
    EXPECT_EQ(size, wrba.size());

    //! DO Read
    buf.seek(0);
    ByteArray rba = buf.readAll();

    //! CHECK
    EXPECT_EQ(rba, wrba);
}

TEST_F(Global_IO_IODeviceTests, Open_Append)
{
    //! GIVEN Some buffer
    std::string ref = "Hello";
    ByteArray ba(reinterpret_cast<const uint8_t*>(ref.c_str()), ref.size());
    Buffer buf(&ba);

    //! DO Open ReadWrite
    EXPECT_TRUE(buf.open(IODevice::Append));

    //! DO Write data
    std::string data = " World!";
    ByteArray wrba(reinterpret_cast<const uint8_t*>(data.c_str()), data.size());
    size_t size = buf.write(wrba);

    //! CHECK
    EXPECT_EQ(size, wrba.size());

    //! CHECK
    std::string all = "Hello World!";
    ByteArray aba(reinterpret_cast<const uint8_t*>(all.c_str()), all.size());

    EXPECT_EQ(ba, aba);
}
