/**********************************************************************

  Audacity: A Digital Audio Editor

  FifoBuffer.h

  Dmitry Vedenko

**********************************************************************/

#include "FifoBuffer.h"

#include <algorithm>

FifoBuffer::Page::Page(int size)
    : Data(size)
{
}

void FifoBuffer::Page::Reset()
{
    WritePosition = 0;
    ReadPosition  = 0;
}

FifoBuffer::FifoBuffer(int pageSize)
    : mPageSize{pageSize}
{
}

int64_t FifoBuffer::Write(const void* dataPtr, int64_t size)
{
    const int8_t* data = static_cast<const int8_t*>(dataPtr);

    auto bytesLeft = size;

    while (bytesLeft > 0)
    {
        if (
            mActivePages.empty()
            || mActivePages.back()->WritePosition == mPageSize) {
            if (mFreePages.empty()) {
                mAllocatedPages.emplace_back(mPageSize);
                mFreePages.push_back(&mAllocatedPages.back());
            }

            mActivePages.push(mFreePages.back());
            mFreePages.pop_back();
        }

        auto& page   = mActivePages.back();
        auto toWrite = std::min(
            bytesLeft, static_cast<int64_t>(mPageSize - page->WritePosition));

        std::copy(data, data + toWrite, page->Data.begin() + page->WritePosition);
        page->WritePosition += toWrite;
        mAvaliable += toWrite;

        data += toWrite;
        bytesLeft -= toWrite;
    }

    return size;
}

int64_t FifoBuffer::Read(void* data, int64_t size)
{
    size = std::min(size, mAvaliable);

    int8_t* dataPtr = static_cast<int8_t*>(data);

    int bytesRead = 0;

    while (size > 0)
    {
        auto& page = mActivePages.front();
        auto toRead
            =std::min(size, static_cast<int64_t>(mPageSize - page->ReadPosition));

        std::copy(
            page->Data.begin() + page->ReadPosition,
            page->Data.begin() + page->ReadPosition + toRead, dataPtr);
        page->ReadPosition += toRead;
        mAvaliable -= toRead;

        dataPtr += toRead;
        size -= toRead;
        bytesRead += toRead;

        if (page->ReadPosition == mPageSize) {
            page->Reset();
            mFreePages.push_back(page);
            mActivePages.pop();
        }
    }

    return bytesRead;
}

int64_t FifoBuffer::GetAvailable() const
{
    return mAvaliable;
}
