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

#include "io/file.h"

using namespace mu;
using namespace mu::io;

class Global_IO_FileTests : public ::testing::Test
{
public:
};

TEST_F(Global_IO_FileTests, FileTests_RW)
{
    path_t filePath("FileTests_RW.txt");
    std::string ref = "Hello World!";
    {
        //! GIVEN Some file
        File f(filePath);

        //! DO Open file
        EXPECT_TRUE(f.open(IODevice::WriteOnly));

        //! DO Write data
        ByteArray ba(reinterpret_cast<const uint8_t*>(ref.c_str()), ref.size());

        size_t size = f.write(ba);

        //! CHECK
        EXPECT_EQ(size, ref.size());
    }

    {
        //! GIVEN file
        File f(filePath);

        //! CHECK exists
        EXPECT_TRUE(f.exists());

        //! DO Open file
        EXPECT_TRUE(f.open(IODevice::ReadOnly));

        ByteArray ba = f.readAll();
        EXPECT_EQ(ba, ByteArray(reinterpret_cast<const uint8_t*>(ref.c_str()), ref.size()));
    }
}

static void createFile(const mu::io::path_t& p, const std::string& content)
{
    File f(p);
    EXPECT_TRUE(f.open(IODevice::WriteOnly));

    ByteArray ba(reinterpret_cast<const uint8_t*>(content.c_str()), content.size());

    size_t size = f.write(ba);
    EXPECT_EQ(size, content.size());
}

TEST_F(Global_IO_FileTests, FileTests_Exists_WriteOnly)
{
    path_t filePath("FileTests_Exists_WriteOnly.txt");
    createFile(filePath, "Hello World!");

    {
        //! GIVE Open file again for write
        File f(filePath);

        //! DO Open file WriteOnly
        EXPECT_TRUE(f.open(IODevice::WriteOnly));
        //! CHECK
        EXPECT_EQ(f.size(), 0);

        //! DO Write data
        std::string ref = "mimi";
        ByteArray ba(reinterpret_cast<const uint8_t*>(ref.c_str()), ref.size());

        size_t size = f.write(ba);

        //! CHECK
        EXPECT_EQ(size, ref.size());
    }

    {
        //! GIVE Open file again for read
        File f(filePath);

        //! DO Open file
        EXPECT_TRUE(f.open(IODevice::ReadOnly));

        //! DO Read data
        ByteArray data = f.readAll();

        //! CHECK
        std::string ref = "mimi";
        ByteArray refba(reinterpret_cast<const uint8_t*>(ref.c_str()), ref.size());
        EXPECT_EQ(refba, data);
    }
}

TEST_F(Global_IO_FileTests, FileTests_Exists_ReadWrite)
{
    path_t filePath("FileTests_Exists_ReadWrite.txt");
    createFile(filePath, "Hello World!");

    {
        //! GIVE Open file again for write
        File f(filePath);

        //! DO Open file WriteOnly
        EXPECT_TRUE(f.open(IODevice::ReadWrite));

        std::cout << "f.size: " << f.size() << "\n";

        //! CHECK
        EXPECT_EQ(f.size(), 12);

        //! DO Write data
        std::string ref = "mimi";
        ByteArray ba(reinterpret_cast<const uint8_t*>(ref.c_str()), ref.size());

        size_t size = f.write(ba);

        //! CHECK
        EXPECT_EQ(size, ref.size());
    }

    {
        //! GIVE Open file again for read
        File f(filePath);

        //! DO Open file
        EXPECT_TRUE(f.open(IODevice::ReadOnly));

        //! DO Read data
        ByteArray data = f.readAll();

        //! CHECK
        std::string ref = "mimio World!";
        ByteArray refba(reinterpret_cast<const uint8_t*>(ref.c_str()), ref.size());
        EXPECT_EQ(refba, data);
    }
}

TEST_F(Global_IO_FileTests, FileTests_Exists_Append)
{
    path_t filePath("FileTests_Exists_Append.txt");
    createFile(filePath, "Hello World!");

    {
        //! GIVE Open file again for write
        File f(filePath);

        //! DO Open file WriteOnly
        EXPECT_TRUE(f.open(IODevice::Append));

        std::cout << "f.size: " << f.size() << "\n";

        //! CHECK
        EXPECT_EQ(f.size(), 12);

        //! DO Write data
        std::string ref = "mimi";
        ByteArray ba(reinterpret_cast<const uint8_t*>(ref.c_str()), ref.size());

        size_t size = f.write(ba);

        //! CHECK
        EXPECT_EQ(size, ref.size());
    }

    {
        //! GIVE Open file again for read
        File f(filePath);

        //! DO Open file
        EXPECT_TRUE(f.open(IODevice::ReadOnly));

        //! DO Read data
        ByteArray data = f.readAll();

        //! CHECK
        std::string ref = "Hello World!mimi";
        ByteArray refba(reinterpret_cast<const uint8_t*>(ref.c_str()), ref.size());
        EXPECT_EQ(refba, data);
    }
}
