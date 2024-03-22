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

#include "io/fileinfo.h"

#include <QFileInfo>
#include <QDir>

using namespace mu::io;

class Global_IO_FileInfoTests : public ::testing::Test
{
public:
};

TEST_F(Global_IO_FileInfoTests, Path)
{
    {
        //! GIVE Some file path
        path_t filePath = "path/to/filename.ext1.ext2";
        //! DO
        path_t path = FileInfo(filePath).path();
        //! CHECK
        EXPECT_EQ(path, "path/to");
    }

    {
        //! GIVE Some file path
        path_t filePath = "path/to/filename";
        //! DO
        path_t path = FileInfo(filePath).path();
        //! CHECK
        EXPECT_EQ(path, "path/to");
    }

    {
        //! GIVE Some file path
        path_t filePath = "path/to/";
        //! DO
        path_t path = FileInfo(filePath).path();
        //! CHECK
        EXPECT_EQ(path, "path/to");
    }

    {
        //! GIVE Some file path
        path_t filePath = "path";
        //! DO
        path_t path = FileInfo(filePath).path();
        //! CHECK
        EXPECT_EQ(path, ".");
    }
}

TEST_F(Global_IO_FileInfoTests, FilePath)
{
    {
        //! GIVE Some file path
        path_t filePath = "path/to/filename.ext1.ext2";
        //! DO
        path_t path = FileInfo(filePath).filePath();
        //! CHECK
        EXPECT_EQ(path, "path/to/filename.ext1.ext2");
    }

    {
        //! GIVE Some file path
        path_t filePath = "path/to/filename";
        //! DO
        path_t path = FileInfo(filePath).filePath();
        //! CHECK
        EXPECT_EQ(path, "path/to/filename");
    }

    {
        //! GIVE Some file path
        path_t filePath = "path/to/";
        //! DO
        path_t path = FileInfo(filePath).filePath();
        //! CHECK
        EXPECT_EQ(path, "path/to/");
    }

    {
        //! GIVE Some file path
        path_t filePath = "filename.ext1";
        //! DO
        path_t path = FileInfo(filePath).filePath();
        //! CHECK
        EXPECT_EQ(path, "filename.ext1");
    }
}

TEST_F(Global_IO_FileInfoTests, FileName)
{
    {
        //! GIVE Some file path
        path_t filePath = "path/to/filename.ext1.ext2";
        //! DO
        QString name = FileInfo(filePath).fileName();
        //! CHECK
        EXPECT_EQ(name, "filename.ext1.ext2");
    }

    {
        //! GIVE Some file path
        path_t filePath = "path/to/filename";
        //! DO
        QString name = FileInfo(filePath).fileName();
        //! CHECK
        EXPECT_EQ(name, "filename");
    }

    {
        //! GIVE Some file path
        path_t filePath = "filename.ext1";
        //! DO
        QString name = FileInfo(filePath).fileName();
        //! CHECK
        EXPECT_EQ(name, "filename.ext1");
    }

    {
        //! GIVE Some file path
        path_t filePath = "path/to/";
        //! DO
        QString name = FileInfo(filePath).fileName();
        //! CHECK
        EXPECT_EQ(name, "");
    }
}

TEST_F(Global_IO_FileInfoTests, BaseName)
{
    {
        //! GIVE Some file path
        path_t filePath = "path/to/filename.ext1.ext2";
        //! DO
        QString name = FileInfo(filePath).baseName();
        //! CHECK
        EXPECT_EQ(name, "filename");
    }

    {
        //! GIVE Some file path
        path_t filePath = "path/to/filename.ext1";
        //! DO
        QString name = FileInfo(filePath).baseName();
        //! CHECK
        EXPECT_EQ(name, "filename");
    }

    {
        //! GIVE Some file path
        path_t filePath = "path/to/filename";
        //! DO
        QString name = FileInfo(filePath).baseName();
        //! CHECK
        EXPECT_EQ(name, "filename");
    }

    {
        //! GIVE Some file path
        path_t filePath = "filename.ext1";
        //! DO
        QString name = FileInfo(filePath).baseName();
        //! CHECK
        EXPECT_EQ(name, "filename");
    }

    {
        //! GIVE Some file path
        path_t filePath = "filename";
        //! DO
        QString name = FileInfo(filePath).baseName();
        //! CHECK
        EXPECT_EQ(name, "filename");
    }

    {
        //! GIVE Some file path
        path_t filePath = "path/to/";
        //! DO
        QString name = FileInfo(filePath).baseName();
        //! CHECK
        EXPECT_EQ(name, "");
    }
}

TEST_F(Global_IO_FileInfoTests, CompleteBaseName)
{
    {
        //! GIVE Some file path
        path_t filePath = "path/to/filename.ext1.ext2";
        //! DO
        QString name = FileInfo(filePath).completeBaseName();
        //! CHECK
        EXPECT_EQ(name, "filename.ext1");
    }

    {
        //! GIVE Some file path
        path_t filePath = "path/to/filename.ext1";
        //! DO
        QString name = FileInfo(filePath).completeBaseName();
        //! CHECK
        EXPECT_EQ(name, "filename");
    }

    {
        //! GIVE Some file path
        path_t filePath = "path/to/filename";
        //! DO
        QString name = FileInfo(filePath).completeBaseName();
        //! CHECK
        EXPECT_EQ(name, "filename");
    }

    {
        //! GIVE Some file path
        path_t filePath = "path/to/";
        //! DO
        QString name = FileInfo(filePath).completeBaseName();
        //! CHECK
        EXPECT_EQ(name, "");
    }
}

TEST_F(Global_IO_FileInfoTests, Suffix)
{
    {
        //! GIVE Some file path
        path_t filePath = "path/to/filename.ext1.ext2";
        //! DO
        QString name = FileInfo(filePath).suffix();
        //! CHECK
        EXPECT_EQ(name, "ext2");
    }

    {
        //! GIVE Some file path
        path_t filePath = "path/to/filename.ext1";
        //! DO
        QString name = FileInfo(filePath).suffix();
        //! CHECK
        EXPECT_EQ(name, "ext1");
    }

    {
        //! GIVE Some file path
        path_t filePath = "path/to/filename";
        //! DO
        QString name = FileInfo(filePath).suffix();
        //! CHECK
        EXPECT_EQ(name, "");
    }

    {
        //! GIVE Some file path
        path_t filePath = "path/to/";
        //! DO
        QString name = FileInfo(filePath).suffix();
        //! CHECK
        EXPECT_EQ(name, "");
    }

    {
        //! GIVE Some file path
        path_t filePath = "./path/to/";
        //! DO
        QString name = FileInfo(filePath).suffix();
        //! CHECK
        EXPECT_EQ(name, "");
    }
}

TEST_F(Global_IO_FileInfoTests, IsRelative)
{
    {
        //! GIVE Some file path
        path_t filePath = "path/to/filename.ext1.ext2";
        //! DO
        bool ret = FileInfo(filePath).isRelative();
        //! CHECK
        EXPECT_TRUE(ret);
    }

    {
        //! GIVE Some file path
        path_t filePath = "./path/to/filename.ext1.ext2";
        //! DO
        bool ret = FileInfo(filePath).isRelative();
        //! CHECK
        EXPECT_TRUE(ret);
    }

    {
        //! GIVE Some file path
        path_t filePath = "/path/to/filename.ext1.ext2";
        //! DO
        bool ret = FileInfo(filePath).isRelative();
        //! CHECK
        EXPECT_FALSE(ret);
    }

    {
        //! GIVE Some file path
        path_t filePath = ":/path/to/filename.ext1.ext2";
        //! DO
        bool ret = FileInfo(filePath).isRelative();
        //! CHECK
        EXPECT_FALSE(ret);
    }
}

TEST_F(Global_IO_FileInfoTests, IsAbsolute)
{
    {
        //! GIVE Some file path
        path_t filePath = "path/to/filename.ext1.ext2";
        //! DO
        bool ret = FileInfo(filePath).isAbsolute();
        //! CHECK
        EXPECT_FALSE(ret);
    }

    {
        //! GIVE Some file path
        path_t filePath = "./path/to/filename.ext1.ext2";
        //! DO
        bool ret = FileInfo(filePath).isAbsolute();
        //! CHECK
        EXPECT_FALSE(ret);
    }

    {
        //! GIVE Some file path
        path_t filePath = "/path/to/filename.ext1.ext2";
        //! DO
        bool ret = FileInfo(filePath).isAbsolute();
        //! CHECK
        EXPECT_TRUE(ret);
    }

    {
        //! GIVE Some file path
        path_t filePath = ":/path/to/filename.ext1.ext2";
        //! DO
        bool ret = FileInfo(filePath).isAbsolute();
        //! CHECK
        EXPECT_TRUE(ret);
    }
}

TEST_F(Global_IO_FileInfoTests, DirPath)
{
    {
        //! GIVE Some file path
        path_t filePath = "/path/to/filename.ext1";
        //! DO
        path_t dirPath = FileInfo(filePath).dir().path();
        //! CHECK
        EXPECT_EQ(dirPath, "/path/to");
    }
}
