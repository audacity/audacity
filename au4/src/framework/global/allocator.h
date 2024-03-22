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
#ifndef MU_GLOBAL_ALLOCATOR_H
#define MU_GLOBAL_ALLOCATOR_H

#include <cstdint>
#include <vector>
#include <list>
#include <string>

namespace mu {
#define OBJECT_ALLOCATOR(Module, ClassName) \
public: \
    static ObjectAllocator& allocator() { \
        static ObjectAllocator a(#Module, #ClassName, ObjectAllocator::destroyer<ClassName>); \
        return a; \
    } \
    static void* operator new(size_t sz) { \
        return ObjectAllocator::enabled() ? allocator().alloc(sz) : ::operator new(sz); \
    } \
    static void operator delete(void* ptr) { \
        if (ObjectAllocator::enabled()) { \
            allocator().free(ptr); \
        } else { \
            ::operator delete(ptr); \
        } \
    } \
    static void* operator new[](size_t sz) { \
        return ObjectAllocator::enabled() ? allocator().not_supported("new[]") : ::operator new[](sz); \
    } \
    static void* operator new(size_t sz, void* ptr) { \
        return ObjectAllocator::enabled() ? allocator().not_supported("new(size_t, void*)") : ::operator new(sz, ptr); \
    } \
    static void operator delete[](void* ptr) { \
        if (ObjectAllocator::enabled()) { \
            allocator().not_supported("delete[]"); \
        } else { \
            ::operator delete[](ptr); \
        } \
    } \
private:

class ObjectAllocator
{
public:

    using destroyer_t = void (*)(void*);

    ObjectAllocator(const char* module, const char* name, destroyer_t dtor);
    ~ObjectAllocator();

    static size_t DEFAULT_BLOCK_SIZE;

    const char* module() const;
    const char* name() const;

    void* alloc(size_t size);
    void free(void* ptr);
    void cleanup();

    template<class T>
    static void destroyer(void* ptr)
    {
        T* p = reinterpret_cast<T*>(ptr);
        p->~T();
    }

    void* not_supported(const char* info);

    struct Info
    {
        std::string module;
        std::string name;
        size_t chunkSize = 0;
        size_t blockCount = 0;
        size_t totalChunks = 0;
        size_t freeChunks = 0;

        uint64_t totalAllocatedCount = 0;
        uint64_t totalFreeCount = 0;

        uint64_t usedChunks() const { return totalChunks - freeChunks; }
        uint64_t allocatedBytes() const { return totalChunks * chunkSize; }
    };

    Info stateInfo() const;

    static bool enabled() { return s_used; }
    static void used();
    static void unused();

    static int s_used;
private:

    struct Chunk {
        /**
         * When a chunk is free, the `next` contains the
         * address of the next chunk in a list.
         *
         * When it's allocated, this space is used by
         * the user.
         */
        Chunk* next = nullptr;
    };

    struct Block {
        Chunk* begin = nullptr;
        size_t chunkCount = 0;
        size_t chunkSize = 0;
    };

    Block allocateBlock(size_t chunkSize) const;

    const char* m_module = nullptr;
    const char* m_name = nullptr;
    size_t m_chunkSize = 0;
    destroyer_t m_dtor = nullptr;
    Chunk* m_free = nullptr;
    std::vector<Block> m_blocks;

    struct Statistic
    {
        uint64_t totalAllocatedCount = 0;
        uint64_t totalFreeCount = 0;
    };

    Statistic m_statistic;
};

class AllocatorsRegister
{
public:

    static AllocatorsRegister* instance()
    {
        static AllocatorsRegister r;
        return &r;
    }

    void reg(ObjectAllocator* a);
    void unreg(ObjectAllocator* a);

    void cleanupAll(const std::string& module);

    void printStatistic(const std::string& title);
    void printState(const std::string& title);

private:
    std::list<ObjectAllocator*> m_allocators;
};
}

#endif // MU_GLOBAL_ALLOCATOR_H
