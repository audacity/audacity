/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file MultipartData.h

 Dmitry Vedenko
 **********************************************************************/

#pragma once

#include <vector>
#include <memory>
#include <cstddef>
#include <string_view>
#include <string>

#include <wx/filename.h>

#include "HeadersList.h"

namespace audacity {
namespace network_manager {
class NETWORK_MANAGER_API MultipartData final
{
public:
    MultipartData() = default;
    MultipartData(const MultipartData&) = delete;
    MultipartData& operator =(const MultipartData&) = delete;

    class NETWORK_MANAGER_API Part
    {
    public:
        virtual ~Part() = default;
        void SetHeader(const std::string& headerName, const std::string& headerValue);

        void SetContentType(const std::string& mimeType);
        void SetContentDisposition(const std::string& disposition);

        virtual int64_t GetSize() const = 0;
        virtual size_t GetOffset() const = 0;

        virtual size_t Read(void* buffer, size_t maxBytes) = 0;
        virtual bool Seek(int64_t offset, int origin = SEEK_SET) = 0;

        const HeadersList& GetHeaders() const;
    private:
        HeadersList mHeaders;
    };

    void Add(std::string_view name, std::string_view value);
    void Add(
        std::string_view name, std::string_view contentType, const void* value, size_t size);

    void AddFile(std::string_view name, std::string_view contentType, const wxFileName& fileName);

    size_t GetPartsCount() const;

    Part* GetPart(size_t idx);
    const Part* GetPart(size_t idx) const;

    bool IsEmpty() const;

private:
    std::vector<std::unique_ptr<Part> > mParts;
};
}
}
