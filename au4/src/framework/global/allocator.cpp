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
#include "allocator.h"

#include <cstdlib>
#include <iostream>
#include <sstream>

#include "stringutils.h"
#include "log.h"

using namespace mu;

int ObjectAllocator::s_used = 0;
size_t ObjectAllocator::DEFAULT_BLOCK_SIZE(1024 * 256); // 256 kB

static inline size_t align(size_t n)
{
    return (n + sizeof(intptr_t) - 1) & ~(sizeof(intptr_t) - 1);
}

// ============================================
// ObjectAllocator
// ============================================
void ObjectAllocator::used()
{
#ifdef MUE_ENABLE_CUSTOM_ALLOCATOR
    s_used++;
#endif
}

void ObjectAllocator::unused()
{
#ifdef MUE_ENABLE_CUSTOM_ALLOCATOR
    s_used--;
#endif
}

ObjectAllocator::ObjectAllocator(const char* module, const char* name, destroyer_t dtor)
    : m_module(module), m_name(name), m_dtor(dtor)
{
    AllocatorsRegister::instance()->reg(this);
}

ObjectAllocator::~ObjectAllocator()
{
    AllocatorsRegister::instance()->unreg(this);
}

const char* ObjectAllocator::module() const
{
    return m_module;
}

const char* ObjectAllocator::name() const
{
    return m_name;
}

void* ObjectAllocator::alloc(size_t size)
{
    size = align(size);

    if (!m_chunkSize) {
        m_chunkSize = size;
    }

    assert(m_chunkSize == size);

    if (!m_free) {
        Block b = allocateBlock(m_chunkSize);
        m_blocks.push_back(b);
        m_free = b.begin;
    }

    // The return value is the current position of
    // the allocation pointer:
    Chunk* freeChunk = m_free;

    // Advance (bump) the allocation pointer to the next chunk.
    //
    // When no chunks left, the `m_free` will be set to `nullptr`, and
    // this will cause allocation of a new block on the next request:
    m_free = m_free->next;

    m_statistic.totalAllocatedCount++;

    return freeChunk;
}

void ObjectAllocator::free(void* chunk)
{
    // The freed chunk's next pointer points to the
    // current allocation pointer:
    reinterpret_cast<Chunk*>(chunk)->next = m_free;

    // And the allocation pointer is now set
    // to the returned (free) chunk:
    m_free = reinterpret_cast<Chunk*>(chunk);

    m_statistic.totalFreeCount++;
}

void ObjectAllocator::cleanup()
{
    if (m_blocks.empty()) {
        return;
    }

    std::set<Chunk*> freeChunks;
    {
        Chunk* free = m_free;
        while (free) {
            freeChunks.insert(free);
            free = free->next;
        }
    }

    for (size_t bi = 0; bi < m_blocks.size(); ++bi) {
        const Block& b = m_blocks.at(bi);
        Chunk* chunk = b.begin;
        for (size_t i = 0; i < b.chunkCount - 1; ++i) {
            // if not free chunk, then destroy object
            if (freeChunks.find(chunk) == freeChunks.cend()) {
                m_dtor(reinterpret_cast<void*>(chunk));
            }

            chunk->next = reinterpret_cast<Chunk*>(reinterpret_cast<uint8_t*>(chunk) + b.chunkSize);
            chunk = chunk->next;
        }

        if (freeChunks.find(chunk) == freeChunks.cend()) {
            m_dtor(reinterpret_cast<void*>(chunk));
        }

        if (bi < (m_blocks.size() - 1)) {
            chunk->next = m_blocks.at(bi + 1).begin;
        } else {
            chunk->next = nullptr;
        }
    }

    m_free = m_blocks.front().begin;
}

ObjectAllocator::Block ObjectAllocator::allocateBlock(size_t chunkSize) const
{
    size_t blockSize = std::max(DEFAULT_BLOCK_SIZE, chunkSize);
    size_t chunkCount = blockSize / chunkSize;

    Block b;
    b.begin = reinterpret_cast<Chunk*>(malloc(blockSize));
    b.chunkCount = chunkCount;
    b.chunkSize = chunkSize;

    // Once the block is allocated, we need to chain all
    // the chunks in this block:

    Chunk* chunk = b.begin;

    for (size_t i = 0; i < chunkCount - 1; ++i) {
        chunk->next = reinterpret_cast<Chunk*>(reinterpret_cast<uint8_t*>(chunk) + chunkSize);
        chunk = chunk->next;
    }

    chunk->next = nullptr;

    return b;
}

void* ObjectAllocator::not_supported(const char* info)
{
    LOGE() << m_name << ": " << info << " not supported";
    std::abort();
    //return nullptr; // NOTREACHED
}

ObjectAllocator::Info ObjectAllocator::stateInfo() const
{
    Info info;
    info.module = m_module;
    info.name = m_name;
    info.chunkSize = m_chunkSize;
    info.blockCount = m_blocks.size();
    info.totalAllocatedCount = m_statistic.totalAllocatedCount;
    info.totalFreeCount = m_statistic.totalFreeCount;

    for (const Block& b : m_blocks) {
        info.totalChunks += b.chunkCount;
    }

    Chunk* free = m_free;
    while (free) {
        ++info.freeChunks;
        free = free->next;
    }

    return info;
}

// ============================================
// AllocatorsRegister
// ============================================
void AllocatorsRegister::reg(ObjectAllocator* a)
{
    m_allocators.push_back(a);
}

void AllocatorsRegister::unreg(ObjectAllocator* a)
{
    m_allocators.remove(a);
}

void AllocatorsRegister::cleanupAll(const std::string& module)
{
    for (ObjectAllocator* a : m_allocators) {
        if (a->module() == module) {
            a->cleanup();
        }
    }
}

#define FORMAT(str, width) mu::strings::leftJustified(str, width)
#define TITLE(str) FORMAT(std::string(str), 20)
#define VALUE(val) FORMAT(std::to_string(val), 20)

void AllocatorsRegister::printStatistic(const std::string& title)
{
    std::stringstream stream;
    stream << "\n\n";
    stream << title << "\n";
    stream << "allocators: " << m_allocators.size() << '\n';
    stream << TITLE("Object") << TITLE("Total alloc") << TITLE("Total free") << TITLE("Used (leak?)") << TITLE("Object size") << "\n";

    uint64_t totalBytes = 0;
    uint64_t totalAllocatedCount = 0;
    uint64_t totalFreeCount = 0;
    uint64_t totalUsedCount = 0;
    for (ObjectAllocator* a : m_allocators) {
        ObjectAllocator::Info info = a->stateInfo();
        stream << FORMAT(info.name, 20)
               << VALUE(info.totalAllocatedCount)
               << VALUE(info.totalFreeCount)
               << VALUE(info.usedChunks())
               << VALUE(info.chunkSize)
               << "\n";

        totalAllocatedCount += info.totalAllocatedCount;
        totalFreeCount += info.totalFreeCount;
        totalUsedCount += info.usedChunks();
        totalBytes += info.allocatedBytes();
    }

    stream << "--------------------------------------------------------------------------------------------\n";
    stream << FORMAT("Total", 20) << VALUE(totalAllocatedCount) << VALUE(totalFreeCount) << VALUE(totalUsedCount) << "\n";
    stream << "Total allocated: " << totalBytes << " bytes\n";

    LOGD() << stream.str() << '\n';
}

void AllocatorsRegister::printState(const std::string& title)
{
    std::stringstream stream;
    stream << "\n\n";
    stream << title << "\n";
    stream << "allocators: " << m_allocators.size() << '\n';
    stream << TITLE("Object") << TITLE("blockCount") << TITLE("totalChunks") << TITLE("freeChunks") << TITLE("chunkSize")
           << TITLE("allocatedBytes") << "\n";

    uint64_t totalBytes = 0;
    for (ObjectAllocator* a : m_allocators) {
        ObjectAllocator::Info info = a->stateInfo();
        stream << FORMAT(info.name, 20)
               << VALUE(info.blockCount)
               << VALUE(info.totalChunks)
               << VALUE(info.freeChunks)
               << VALUE(info.chunkSize)
               << VALUE(info.allocatedBytes())
               << "\n";

        totalBytes += info.allocatedBytes();
    }

    stream << "-----------------------------------------------------\n";
    stream << "Total allocated: " << totalBytes << " bytes\n";

    LOGD() << stream.str() << '\n';
}
