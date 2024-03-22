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

#include "allocator.h"

#include "log.h"

using namespace mu;

namespace mu {
class ItemBase
{
    OBJECT_ALLOCATOR(test, ItemBase)

public:
    ItemBase(uint8_t n)
        : wasDestroyed(0), num(n)
    {
    }

    virtual ~ItemBase()
    {
        wasDestroyed++;
    }

    int wasDestroyed = 0;
    uint8_t num = 0;
    std::string str;

    bool alive()
    {
        str = "some string";
        return str == "some string";
    }
};

#define DECLARE_ITEM(size) \
    class Item##size : public ItemBase { \
        OBJECT_ALLOCATOR(test, Item##size) \
    public: \
        Item##size(uint8_t n) \
            : ItemBase(n) \
        { \
            LOGI() << "created: " << num; \
        } \
        ~Item##size() \
        { \
            LOGI() << "destroyed: " << num; \
        } \
        uint8_t data[size]; \
    };

DECLARE_ITEM(3)
DECLARE_ITEM(8)
DECLARE_ITEM(13)
DECLARE_ITEM(131)
}

class Global_AllocatorTests : public ::testing::Test
{
public:

    void SetUp() override
    {
        ObjectAllocator::s_used++;
    }
};

TEST_F(Global_AllocatorTests, Single_NewDelete)
{
    //! DO Create Item
    ItemBase* item = new Item3(1);

    //! CHECK
    EXPECT_TRUE(item);
    EXPECT_TRUE(item->alive());
    EXPECT_EQ(item->wasDestroyed, 0);

    //! CHECK Allocator state
    ObjectAllocator::Info info = Item3::allocator().stateInfo();
    EXPECT_EQ(info.totalChunks - info.freeChunks, 1); // used 1 chunk

    //! DO Destroy Item
    delete item;

    //! CHECK
    EXPECT_EQ(item->wasDestroyed, 1);

    //! CHECK Allocator state
    info = Item3::allocator().stateInfo();
    EXPECT_EQ(info.totalChunks - info.freeChunks, 0); // used 0 chunk
}

TEST_F(Global_AllocatorTests, Single_NewCleanup)
{
    //! DO Create Item
    ItemBase* item = new Item3(2);

    //! CHECK
    EXPECT_TRUE(item);
    EXPECT_TRUE(item->alive());
    EXPECT_FALSE(item->wasDestroyed);

    //! DO Allocator cleanup
    Item3::allocator().cleanup();

    //! CHECK
    EXPECT_TRUE(item->wasDestroyed);
}

TEST_F(Global_AllocatorTests, Single_BigNewDelete)
{
    //! GIVEN the default size of the allocator block is less than the size of an item
    size_t itemSize = sizeof(Item131);
    ObjectAllocator::DEFAULT_BLOCK_SIZE = itemSize - 16; // bytes

    //! DO Create Item
    ItemBase* item = new Item131(3);

    //! CHECK
    EXPECT_TRUE(item);
    EXPECT_TRUE(item->alive());
    EXPECT_EQ(item->wasDestroyed, 0);

    //! CHECK Allocator state
    ObjectAllocator::Info info = Item131::allocator().stateInfo();
    EXPECT_EQ(info.totalChunks, 1);
    EXPECT_EQ(info.freeChunks, 0);

    //! DO Destroy Item
    delete item;

    //! CHECK
    EXPECT_EQ(item->wasDestroyed, 1);

    //! CHECK Allocator state
    info = Item131::allocator().stateInfo();
    EXPECT_EQ(info.totalChunks, 1);
    EXPECT_EQ(info.freeChunks, 1);
}

TEST_F(Global_AllocatorTests, Many_NewCleanup)
{
    //! GIVEN the default size of the allocator block is less than the size of all items
    size_t itemSize = sizeof(Item8);
    ObjectAllocator::DEFAULT_BLOCK_SIZE = itemSize * 4;  // bytes

    //! DO Create Items (more then one block size)
    std::vector<ItemBase*> items;
    for (size_t i = 0; i < 10; ++i) {
        items.push_back(new Item8(static_cast<uint8_t>(i)));
    }

    //! CHECK
    for (ItemBase* item : items) {
        EXPECT_TRUE(item);
        EXPECT_TRUE(item->alive());
        EXPECT_EQ(item->wasDestroyed, 0);
    }

    //! CHECK Allocator state
    ObjectAllocator::Info info = Item8::allocator().stateInfo();
    EXPECT_EQ(info.totalChunks, 12); // DEFAULT_BLOCK_SIZE * 3
    EXPECT_EQ(info.freeChunks, 2);

    //! DO Allocator cleanup
    Item8::allocator().cleanup();

    //! CHECK
    for (const ItemBase* item : items) {
        EXPECT_EQ(item->wasDestroyed, 1);
    }

    //! CHECK Allocator state
    info = Item8::allocator().stateInfo();
    EXPECT_EQ(info.totalChunks, 12); // DEFAULT_BLOCK_SIZE * 3
    EXPECT_EQ(info.freeChunks, 12);
}
