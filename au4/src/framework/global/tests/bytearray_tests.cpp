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

#include "types/bytearray.h"

using namespace mu;

class Global_Types_ByteArrayTests : public ::testing::Test
{
public:
};

TEST_F(Global_Types_ByteArrayTests, Construct)
{
    std::vector<uint8_t> ref = { 1, 2, 3, 4, 5, 6 };

    //! GIVEN Empty ByteArray
    ByteArray ba;

    //! CHECK ByteArray is empty
    EXPECT_TRUE(ba.empty());

    //! DO Construct ByteArray with given data
    ba = ByteArray(&ref[0], ref.size());

    //! CHECK ByteArray not empty
    EXPECT_FALSE(ba.empty());
    //! CHECK ByteArray size equal given data size
    EXPECT_EQ(ba.size(), ref.size());
    //! CHECK Some byte equal same byte in given data
    EXPECT_EQ(ba[1], ref[1]);

    //! CHECK ByteArray data equal given data
    const uint8_t* d = ba.constData();
    EXPECT_EQ(std::memcmp(d, &ref[0], ref.size()), 0);

    //! CHECK ByteArray data is  given data copy (not modified given data)
    ba[2] = 42;
    EXPECT_EQ(ba[2], 42);
    EXPECT_EQ(ref[2], 3);
}

TEST_F(Global_Types_ByteArrayTests, Compare)
{
    std::vector<uint8_t> ref = { 1, 2, 3, 4, 5, 6 };
    //! GIVEN Two ByteArrays with same the data
    ByteArray ba1 = ByteArray(&ref[0], ref.size());
    ByteArray ba2 = ByteArray(&ref[0], ref.size());

    //! CHECK ByteArrays is equals
    EXPECT_TRUE(ba1 == ba2);

    //! DO Modified ba2
    ba2[4] = 9;
    EXPECT_TRUE(ba1 != ba2);
}

TEST_F(Global_Types_ByteArrayTests, Modification)
{
    std::vector<uint8_t> ref = { 1, 2, 3, 4, 5, 6 };
    //! GIVEN ByteArray with the data
    ByteArray ba = ByteArray(&ref[0], ref.size());

    //! CHECK Size equal given data size
    EXPECT_EQ(ba.size(), ref.size());

    //! DO Insert byte to pos = 0 (first)
    ba.insert(0, 11);

    //! CHECK First byte is 11
    EXPECT_EQ(ba[0], 11);
    //! CHECK Size equal given data size + 1
    EXPECT_EQ(ba.size(), ref.size() + 1);
    //! CHECK
    const uint8_t* d1 = ba.constData();
    std::vector<uint8_t> ref1 = { 11, 1, 2, 3, 4, 5, 6 };
    EXPECT_EQ(std::memcmp(d1, &ref1[0], ref1.size()), 0);

    //! DO Insert byte to pos = 4 (middle)
    ba.insert(4, 14);
    //! CHECK
    EXPECT_EQ(ba[4], 14);
    //! CHECK Size equal given data size + 2
    EXPECT_EQ(ba.size(), ref.size() + 2);
    //! CHECK
    const uint8_t* d2 = ba.constData();
    std::vector<uint8_t> ref2 = { 11, 1, 2, 3, 14, 4, 5, 6 };
    EXPECT_EQ(std::memcmp(d2, &ref2[0], ref2.size()), 0);

    //! DO Insert byte to pos = 8 (last)
    ba.insert(8, 17);
    //! CHECK
    EXPECT_EQ(ba[8], 17);
    //! CHECK
    EXPECT_EQ(ba.size(), ref.size() + 3);
    //! CHECK
    const uint8_t* d3 = ba.constData();
    std::vector<uint8_t> ref3 = { 11, 1, 2, 3, 14, 4, 5, 6, 17 };
    EXPECT_EQ(std::memcmp(d3, &ref3[0], ref3.size()), 0);

    //! DO Insert byte to pos = 12 (after last)
    ba.insert(12, 22);
    //! CHECK
    EXPECT_EQ(ba.size(), ref.size() + 3);
    //! CHECK
    const uint8_t* d4 = ba.constData();
    std::vector<uint8_t> ref4 = { 11, 1, 2, 3, 14, 4, 5, 6, 17 };
    EXPECT_EQ(std::memcmp(d4, &ref4[0], ref4.size()), 0);

    //! DO push_back
    ba.push_back(24);
    //! CHECK
    EXPECT_EQ(ba[ba.size() - 1], 24);
    //! CHECK
    EXPECT_EQ(ba.size(), ref.size() + 4);
    //! CHECK
    const uint8_t* d5 = ba.constData();
    std::vector<uint8_t> ref5 = { 11, 1, 2, 3, 14, 4, 5, 6, 17, 24 };
    EXPECT_EQ(std::memcmp(d5, &ref5[0], ref5.size()), 0);

    //! GIVEN Two ByteArrays
    std::vector<uint8_t> ref6 = { 1, 2, 3 };
    std::vector<uint8_t> ref7 = { 4, 5, 6 };
    ByteArray ba6 = ByteArray(&ref6[0], ref6.size());
    ByteArray ba7 = ByteArray(&ref7[0], ref7.size());

    //! DO push_back
    ba6.push_back(ba7);

    //! CHECK
    std::vector<uint8_t> ref8 = { 1, 2, 3, 4, 5, 6 };
    EXPECT_EQ(ba6.size(), ref8.size());

    //! CHECK
    const uint8_t* d6 = ba6.constData();
    EXPECT_EQ(std::memcmp(d6, &ref8[0], ref8.size()), 0);

    //! GIVEN ByteArray
    std::vector<uint8_t> ref10 = { 1, 2, 3, 4, 5, 6 };
    ByteArray ba10 = ByteArray(&ref10[0], ref10.size());

    //! DO resize (new size < size)
    ba10.resize(4);

    //! CHECK
    EXPECT_EQ(ba10.size(), 4);
    //! CHECK
    const uint8_t* d10 = ba10.constData();
    std::vector<uint8_t> ref11 = { 1, 2, 3, 4 };
    EXPECT_EQ(std::memcmp(d10, &ref11[0], ref11.size()), 0);

    //! DO resize (new size > size)
    ba10.resize(8);

    //! CHECK
    EXPECT_EQ(ba10.size(), 8);
    //! CHECK (data beyond the first 4 positions is garbage)
    const uint8_t* d11 = ba10.constData();
    EXPECT_EQ(std::memcmp(d11, &ref11[0], 4), 0);

    //! DO reserve
    size_t sizeBeforeReserving = ba10.size();
    ba10.reserve(10);

    //! CHECK (reserving should only affect capacity, not size)
    EXPECT_EQ(ba10.size(), sizeBeforeReserving);
    //! CHECK (data beyond the first 4 positions is garbage)
    const uint8_t* d12 = ba10.constData();
    EXPECT_EQ(std::memcmp(d12, &ref11[0], 4), 0);

    //! DO clear
    ba10.clear();

    //! CHECK (should be empty now)
    EXPECT_EQ(ba10.size(), 0);
    EXPECT_TRUE(ba10.empty());
}
