/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file MultipartData.cpp

 Dmitry Vedenko
 **********************************************************************/

#include "MultipartData.h"

#include <algorithm>
#include <cstdint>
#include <cstring>

#include "AudacityException.h"

namespace audacity {
namespace network_manager {
namespace {
class ByteBufferPart final : public MultipartData::Part
{
public:
    ByteBufferPart(const void* data, size_t size)
    {
        mData.resize(size);
        std::memcpy(mData.data(), data, size);
    }

    int64_t GetSize() const override
    {
        return static_cast<int64_t>(mData.size());
    }

    size_t GetOffset() const override
    {
        return mOffset;
    }

    size_t Read(void* buffer, size_t maxBytes) override
    {
        const size_t availableBytes = mData.size() - mOffset;

        maxBytes = std::min(maxBytes, availableBytes);

        std::memcpy(buffer, mData.data() + mOffset, maxBytes);

        mOffset += maxBytes;

        return maxBytes;
    }

    bool Seek(int64_t offset, int origin) override
    {
        switch (origin) {
        case SEEK_SET:
            if (offset <= GetSize()) {
                mOffset = offset;
                return true;
            }
            break;
        case SEEK_CUR:
        {
            const int64_t targetOffset = offset + mOffset;

            if (targetOffset >= 0 && targetOffset < GetSize()) {
                mOffset = targetOffset;
                return true;
            }
        }
        break;
        case SEEK_END:
        {
            const int64_t targetOffset = GetSize() + offset;

            if (targetOffset >= 0 && targetOffset < GetSize()) {
                mOffset = targetOffset;
                return true;
            }
        }
        break;
        default:
            break;
        }

        return false;
    }

private:
    std::vector<uint8_t> mData;
    size_t mOffset { 0 };
};

class FilePart final : public MultipartData::Part
{
public:
    explicit FilePart(const wxFileName& fileName)
        : mFileName(fileName)
    {
        if (!fileName.FileExists()) {
            throw SimpleMessageBoxException(ExceptionType::BadUserAction,
                                            XO("Failed to open the file for upload: %s")
                                            .Format(fileName.GetFullPath()));
        }

        if (!mFile.Open(fileName.GetFullPath())) {
            throw SimpleMessageBoxException(
                      ExceptionType::BadUserAction,
                      XO("Failed to open the file for upload: %s")
                      .Format(fileName.GetFullPath()));
        }
    }

    int64_t GetSize() const override
    {
        return static_cast<int64_t>(mFileName.GetSize().GetValue());
    }

    size_t GetOffset() const override
    {
        return static_cast<size_t>(mFile.Tell());
    }

    size_t Read(void* buffer, size_t maxBytes) override
    {
        const auto bytesRead = mFile.Read(buffer, maxBytes);

        if (bytesRead == wxInvalidOffset) {
            return 0;
        }

        return static_cast<size_t>(bytesRead);
    }

    bool Seek(int64_t offset, int origin = SEEK_SET) override
    {
        static constexpr wxSeekMode map[] = { wxFromStart, wxFromCurrent,
                                              wxFromEnd };

        if (origin < SEEK_SET || origin > SEEK_END) {
            return false;
        }

        return wxInvalidOffset != mFile.Seek(offset, map[origin]);
    }

private:
    wxFile mFile;
    wxFileName mFileName;
};
}

void MultipartData::Part::SetHeader(
    const std::string& headerName, const std::string& headerValue)
{
    mHeaders.setHeader(headerName, headerValue);
}

void MultipartData::Part::SetContentType(const std::string& mimeType)
{
    SetHeader(common_headers::ContentType, mimeType);
}

void MultipartData::Part::SetContentDisposition(const std::string& disposition)
{
    SetHeader(common_headers::ContentDisposition, disposition);
}

const HeadersList& MultipartData::Part::GetHeaders() const
{
    return mHeaders;
}

void MultipartData::Add(std::string_view name, std::string_view value)
{
    Add(name, {}, value.data(), value.length());
}

void MultipartData::Add(
    std::string_view name, std::string_view contentType, const void* value,
    size_t size)
{
    mParts.emplace_back(std::make_unique<ByteBufferPart>(value, size));

    mParts.back()->SetContentDisposition("form-data; name=\"" + std::string(name) + "\"");

    if (!contentType.empty()) {
        mParts.back()->SetContentType(std::string(contentType));
    }
}

void MultipartData::AddFile(
    std::string_view name, std::string_view contentType,
    const wxFileName& fileName)
{
    mParts.emplace_back(std::make_unique<FilePart>(fileName));

    mParts.back()->SetContentDisposition(
        "form-data; name=\"" + std::string(name) + "\"; filename=\""
        + fileName.GetFullName().ToUTF8().data() + "\"");

    if (!contentType.empty()) {
        mParts.back()->SetContentType(std::string(contentType));
    }
}

size_t MultipartData::GetPartsCount() const
{
    return mParts.size();
}

MultipartData::Part* MultipartData::GetPart(size_t idx)
{
    if (idx < mParts.size()) {
        return mParts[idx].get();
    }

    return nullptr;
}

const MultipartData::Part* MultipartData::GetPart(size_t idx) const
{
    if (idx < mParts.size()) {
        return mParts[idx].get();
    }

    return nullptr;
}

bool MultipartData::IsEmpty() const
{
    return mParts.empty();
}
}
}
