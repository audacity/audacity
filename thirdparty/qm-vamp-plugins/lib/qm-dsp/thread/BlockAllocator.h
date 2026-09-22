/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    QM DSP Library

    Centre for Digital Music, Queen Mary, University of London.

    This file is derived from the FSB Allocator by Juha Nieminen.  The
    underlying method is unchanged, but the class has been refactored
    to permit multiple separate allocators (e.g. one per thread)
    rather than use a single global one (and to fit house style).

Copyright (c) 2008 Juha Nieminen

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

#ifndef QM_DSP_BLOCK_ALLOCATOR_H
#define QM_DSP_BLOCK_ALLOCATOR_H

#include <cstdlib>

/**
 * BlockAllocator is a simple allocator for fixed-size (usually small)
 * chunks of memory.  The size of an element is specified in the
 * BlockAllocator constructor, and the functions allocate() and
 * deallocate() are used to obtain and release a single element at a
 * time.
 *
 * BlockAllocator may be an appropriate class to use in situations
 * involving a very large number of allocations and deallocations of
 * simple, identical objects across multiple threads (a hard situation
 * for a generic system malloc implementation to handle well).  Retain
 * one BlockAllocator per thread (the class itself is not
 * thread-safe), and ensure that each thread uses its own allocator
 * exclusively.
 *
 * BlockAllocator is based on Juha Nieminen's more general
 * FSBAllocator.
 */
class BlockAllocator
{
public:
    typedef std::size_t data_t;

    BlockAllocator(int elementSize) : m_sz(elementSize) { }

    void *
    allocate()
    {
        if (m_freelist.empty()) {
            m_freelist.push_back(m_blocks.data.size());
            m_blocks.data.push_back(Block(this));
        }

        const data_t index = m_freelist.back();
        Block &block = m_blocks.data[index];
        void *retval = block.allocate(index);
        if (block.isFull()) m_freelist.pop_back();

        return retval;
    }

    void
    deallocate(void *ptr)
    {
        if (!ptr) return;

        data_t *unitPtr = (data_t *)ptr;
        const data_t blockIndex = unitPtr[elementSizeInDataUnits()];
        Block& block = m_blocks.data[blockIndex];

        if (block.isFull()) m_freelist.push_back(blockIndex);
        block.deallocate(unitPtr);
    }

private:
    inline data_t elementsPerBlock() const {
        return 512;
    }
    inline data_t dataSize() const {
        return sizeof(data_t);
    }
    inline data_t elementSizeInDataUnits() const {
        return (m_sz + (dataSize() - 1)) / dataSize();
    }
    inline data_t unitSizeInDataUnits() const {
        return elementSizeInDataUnits() + 1;
    }
    inline data_t blockSizeInDataUnits() const {
        return elementsPerBlock() * unitSizeInDataUnits();
    }

    class Block
    {
    public:
        Block(BlockAllocator *a) :
            m_a(a),
            m_block(0),
            m_firstFreeUnit(data_t(-1)),
            m_allocated(0),
            m_end(0)
        {}

        ~Block() {
            delete[] m_block;
        }

        bool isFull() const {
            return m_allocated == m_a->elementsPerBlock();
        }

        void clear() {
            delete[] m_block;
            m_block = 0;
            m_firstFreeUnit = data_t(-1);
        }

        void *allocate(data_t index) {

            if (m_firstFreeUnit == data_t(-1)) {

                if (!m_block) {
                    m_block = new data_t[m_a->blockSizeInDataUnits()];
                    m_end = 0;
                }

                data_t *retval = m_block + m_end;
                m_end += m_a->unitSizeInDataUnits();
                retval[m_a->elementSizeInDataUnits()] = index;
                ++m_allocated;
                return retval;

            } else {

                data_t *retval = m_block + m_firstFreeUnit;
                m_firstFreeUnit = *retval;
                ++m_allocated;
                return retval;
            }
        }

        void deallocate(data_t *ptr) {

            *ptr = m_firstFreeUnit;
            m_firstFreeUnit = ptr - m_block;

            if (--m_allocated == 0) clear();
        }

    private:
        const BlockAllocator *m_a;
        data_t *m_block;
        data_t m_firstFreeUnit;
        data_t m_allocated;
        data_t m_end;
    };

    struct Blocks
    {
        std::vector<Block> data;

        Blocks() {
            data.reserve(1024);
        }
    };

    const int m_sz;
    Blocks m_blocks;
    std::vector<data_t> m_freelist;
};

#endif
