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

#include <string>
#include <cstring>

#include "io/buffer.h"

using namespace mu;
using namespace mu::io;

class Global_IO_BufferTests : public ::testing::Test
{
public:
};

static void writeDevice(IODevice& dev, const std::string& data)
{
    if (!dev.isOpen()) {
        dev.open(IODevice::WriteOnly);
    }

    dev.write(reinterpret_cast<const uint8_t*>(data.c_str()), data.size());
}

TEST_F(Global_IO_BufferTests, Buffer_Write_Default)
{
    //! GIVEN Default Buffer
    Buffer buf;
    buf.open(IODevice::ReadWrite);

    EXPECT_EQ(buf.size(), 0);

    //! DO Write some data
    std::string ref = "Hello";
    writeDevice(buf, ref);

    //! CHECK
    EXPECT_EQ(buf.size(), ref.size());

    //! CHECK
    buf.seek(0);
    ByteArray d = buf.readAll();
    EXPECT_EQ(d, ByteArray(reinterpret_cast<const uint8_t*>(ref.c_str()), ref.size()));
}

TEST_F(Global_IO_BufferTests, Buffer_Write_PreAllocated)
{
    {
        //! GIVEN Buffer with pre allocated size > ref size)
        Buffer buf(10);
        buf.open(IODevice::ReadWrite);

        EXPECT_EQ(buf.size(), 10);

        //! DO Write some data
        std::string ref = "Hello";
        writeDevice(buf, ref);

        //! CHECK
        EXPECT_EQ(buf.size(), 10);

        //! CHECK
        buf.seek(0);
        ByteArray d = buf.readAll();
        EXPECT_TRUE(std::memcmp(d.constData(), &ref[0], ref.size()) == 0);
    }

    {
        //! GIVEN Buffer with pre allocated size < ref size)
        Buffer buf(3);
        buf.open(IODevice::ReadWrite);

        EXPECT_EQ(buf.size(), 3);

        //! DO Write some data
        std::string ref = "Hello";
        writeDevice(buf, ref);

        //! CHECK
        EXPECT_EQ(buf.size(), ref.size());

        //! CHECK
        buf.seek(0);
        ByteArray d = buf.readAll();
        EXPECT_EQ(d, ByteArray(reinterpret_cast<const uint8_t*>(ref.c_str()), ref.size()));
    }
}

TEST_F(Global_IO_BufferTests, Buffer_Write_To_ByteArray)
{
    //! GIVEN Buffer for given ByteArray
    ByteArray ba;
    Buffer buf(&ba);
    buf.open(IODevice::WriteOnly);

    EXPECT_EQ(buf.size(), 0);

    //! DO Write some data
    std::string ref = "Hello";
    writeDevice(buf, ref);

    //! CHECK
    EXPECT_EQ(buf.size(), ref.size());

    //! CHECK
    EXPECT_EQ(ba, ByteArray(reinterpret_cast<const uint8_t*>(ref.c_str()), ref.size()));
}

TEST_F(Global_IO_BufferTests, Buffer_Read_From_ByteArray)
{
    //! GIVEN ByteArray with data
    std::string ref = "Hello";
    ByteArray ba(reinterpret_cast<const uint8_t*>(ref.c_str()), ref.size());

    //! DO Create Buffer for given ByteArray
    Buffer buf(&ba);
    buf.open(IODevice::ReadOnly);

    //! DO Read data
    ByteArray data = buf.readAll();

    //! CHECK
    EXPECT_EQ(ba.size(), data.size());

    //! CHECK
    EXPECT_EQ(ba, data);
}

TEST_F(Global_IO_BufferTests, Buffer_Read_Pos)
{
    //! GIVEN Buffer with data
    std::string ref = "Hello";
    ByteArray ba(reinterpret_cast<const uint8_t*>(ref.c_str()), ref.size());
    Buffer buf(&ba);
    buf.open(IODevice::ReadOnly);

    EXPECT_EQ(buf.pos(), 0);

    uint8_t ch;

    //! DO Read byte
    buf.read(&ch, 1);
    //! CHECK
    EXPECT_EQ(buf.pos(), 1);
    //! CHECK
    EXPECT_EQ(ch, 'H');

    //! DO Read byte
    buf.read(&ch, 1);
    //! CHECK
    EXPECT_EQ(buf.pos(), 2);
    //! CHECK
    EXPECT_EQ(ch, 'e');

    //! DO Seek and read byte
    buf.seek(4);
    buf.read(&ch, 1);
    //! CHECK
    EXPECT_EQ(buf.pos(), 5);
    //! CHECK
    EXPECT_EQ(ch, 'o');

    //! DO Seek back and read byte
    buf.seek(1);
    buf.read(&ch, 1);
    //! CHECK
    EXPECT_EQ(buf.pos(), 2);
    //! CHECK
    EXPECT_EQ(ch, 'e');
}
