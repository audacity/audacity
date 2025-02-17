/**********************************************************************

  Audacity: A Digital Audio Editor

  MemoryStream.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <array>
#include <cstdint>
#include <list>
#include <vector>

#include "IteratorX.h"

/*!
 * @brief A low overhead memory stream with O(1) append, low heap fragmentation and a linear memory view.
 *
 * wxMemoryBuffer always appends 1Kb to the end of the buffer, causing severe performance issues
 * and significant heap fragmentation. There is no possibility to control the increment value.
 *
 * std::vector doubles its memory size which can be problematic for large projects as well.
 * Not as bad as wxMemoryBuffer though.
 *
 */
class UTILITY_API MemoryStream final
{
public:
    using StreamData = std::vector<uint8_t>;
    using StreamChunk = std::pair<const void*, size_t>;

private:
    static constexpr size_t ChunkSize
        =1024 * 1024 // 1Mb
          - 2 * sizeof(void*) // account for the list node pointers
          - sizeof(size_t); // account for the bytes used member

    struct Chunk final
    {
        std::array<uint8_t, ChunkSize> Data;
        size_t BytesUsed { 0 };

        // Returns data size left to append
        size_t Append(StreamChunk& dataView);
    };

    using ChunksList = std::list<Chunk>;

public:

    MemoryStream() = default;
    MemoryStream(MemoryStream&&) = default;

    void Clear();

    void AppendByte(char data);
    void AppendData(const void* data, const size_t length);

    // This function possibly has O(size) complexity as it may
    // require copying bytes to a linear chunk
    const void* GetData() const;
    const size_t GetSize() const noexcept;

    bool IsEmpty() const noexcept;

    struct UTILITY_API Iterator : ValueIterator<const StreamChunk, std::forward_iterator_tag>
    {
        Iterator(const Iterator&) = default;

        Iterator& operator++();

        Iterator operator++(int);

        StreamChunk operator*() const;
        StreamChunk operator->() const;

        bool operator==(const Iterator& rhs) const noexcept;
        bool operator!=(const Iterator& rhs) const noexcept;
    private:
        Iterator(const MemoryStream* stream, bool isBegin);

        const MemoryStream* mStream { nullptr };
        ChunksList::const_iterator mListIterator;
        bool mShowLinearPart { false };

        friend class MemoryStream;
    };

    Iterator begin() const;
    Iterator end() const;

private:
    // This structures are lazily updated by get data
    mutable ChunksList mChunks;
    mutable StreamData mLinearData;

    size_t mDataSize { 0 };
};
