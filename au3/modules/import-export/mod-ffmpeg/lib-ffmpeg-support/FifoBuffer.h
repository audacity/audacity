/**********************************************************************

  Audacity: A Digital Audio Editor

  FifoBuffer.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <cstdint>
#include <deque>
#include <queue>

class FFMPEG_SUPPORT_API FifoBuffer final
{
public:
    explicit FifoBuffer(int pageSize);

    int64_t Write(const void* data, int64_t size);
    int64_t Read(void* data, int64_t size);

    int64_t GetAvailable() const;

private:
    struct Page final
    {
        explicit Page(int size);

        void Reset();

        std::vector<char> Data;
        int WritePosition {};
        int ReadPosition {};
    };

    std::deque<Page> mAllocatedPages;

    std::queue<Page*> mActivePages;
    std::deque<Page*> mFreePages;

    int64_t mAvaliable {};
    const int mPageSize {};
};
