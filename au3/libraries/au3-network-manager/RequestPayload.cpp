/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file RequestPayload.cpp
 @brief Declare a class for constructing HTTP requests.

 Dmitry Vedenko
 **********************************************************************/

#include "RequestPayload.h"

#include <iterator>
#include <vector>

#include <wx/file.h>

#include "CodeConversions.h"

namespace audacity::network_manager {
namespace {
int64_t CalculateOffset(
    int64_t currentOffset, int64_t offset, int64_t size, RequestPayloadStream::SeekDirection direction)
{
    switch (direction) {
    case RequestPayloadStream::SeekDirection::Start:
        return offset;
    case RequestPayloadStream::SeekDirection::Current:
        return currentOffset + offset;
    case RequestPayloadStream::SeekDirection::End:
        return size - offset;
    default:
        return 0;
    }
}

class EmptyRequestPayloadStream final : public RequestPayloadStream
{
public:
    bool HasData() const override
    {
        return false;
    }

    int64_t GetDataSize() const override
    {
        return 0;
    }

    bool Seek(int64_t offset, SeekDirection direction) override
    {
        return false;
    }

    int64_t Read(void* buffer, int64_t size) override
    {
        return 0;
    }
}; // class EmptyRequestPayloadStream

class MemoryRequestPayloadStream final : public RequestPayloadStream
{
public:
    MemoryRequestPayloadStream(const void* data, int64_t size, bool copyData)
        : mStreamSize{size}
    {
        if (copyData && data != nullptr && mStreamSize > 0) {
            mData.reserve(size);
            std::copy_n(static_cast<const uint8_t*>(data), size, std::back_inserter(mData));
            mDataPointer = mData.data();
        } else {
            mDataPointer = static_cast<const uint8_t*>(data);
        }
    }

    bool HasData() const override
    {
        return mDataPointer != nullptr && mStreamSize > 0;
    }

    int64_t GetDataSize() const override
    {
        return mStreamSize;
    }

    bool Seek(int64_t offset, SeekDirection direction) override
    {
        const auto requestedOffset = CalculateOffset(mStreamPosition, offset, mStreamSize, direction);

        if (requestedOffset < 0 || requestedOffset > mStreamSize) {
            return false;
        }

        mStreamPosition = requestedOffset;
        return true;
    }

    int64_t Read(void* buffer, int64_t size) override
    {
        const auto bytesToRead = std::min(size, mStreamSize - mStreamPosition);

        if (bytesToRead <= 0) {
            return 0;
        }

        std::copy_n(mDataPointer + mStreamPosition, bytesToRead, static_cast<uint8_t*>(buffer));
        mStreamPosition += bytesToRead;

        return bytesToRead;
    }

private:
    std::vector<uint8_t> mData;

    const uint8_t* mDataPointer {};
    int64_t mStreamSize {};

    int64_t mStreamPosition {};
}; // class MemoryRequestPayloadStream

class FileRequestPayloadStream final : public RequestPayloadStream
{
public:
    FileRequestPayloadStream(const std::string& filePath)
        : mFilePath{audacity::ToWXString(filePath)}
        , mFile{mFilePath}
    {
        wxStat(mFilePath, &mFileStat);
    }

    bool HasData() const override
    {
        return mFile.IsOpened();
    }

    int64_t GetDataSize() const override
    {
        return mFileStat.st_size;
    }

    bool Seek(int64_t offset, SeekDirection direction) override
    {
        const auto platformSeekDirection = [&]
        {
            switch (direction) {
            case SeekDirection::Start:
                return wxFromStart;
            case SeekDirection::Current:
                return wxFromCurrent;
            case SeekDirection::End:
                return wxFromEnd;
            default:
                return wxFromStart;
            }
        }();

        return mFile.Seek(offset, platformSeekDirection) != wxInvalidOffset;
    }

    int64_t Read(void* buffer, int64_t size) override
    {
        return mFile.Read(buffer, size);
    }

private:
    wxString mFilePath;
    wxFile mFile;
    wxStructStat mFileStat;
}; // class FileRequestPayloadStream
} // namespace

RequestPayloadStream::~RequestPayloadStream()
{
}

RequestPayloadStreamPtr
CreateRequestPayloadStream(const void* data, int64_t size, bool copyData)
{
    return std::make_shared<MemoryRequestPayloadStream>(data, size, copyData);
}

RequestPayloadStreamPtr CreateRequestPayloadStream()
{
    return std::make_shared<EmptyRequestPayloadStream>();
}

RequestPayloadStreamPtr CreateRequestPayloadStream(const std::string& filePath)
{
    return std::make_shared<FileRequestPayloadStream>(filePath);
}
} // namespace audacity::network_manager
