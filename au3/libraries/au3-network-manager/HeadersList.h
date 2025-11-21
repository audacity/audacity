/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file HeadersList.h
 @brief Declare HTTP headers list class.

 Dmitry Vedenko
 **********************************************************************/

#pragma once

#include <string>
#include <vector>

#include "NetworkManagerApi.h"

namespace audacity {
namespace network_manager {
namespace common_headers {
extern NETWORK_MANAGER_API const std::string Accept;
extern NETWORK_MANAGER_API const std::string AcceptEncoding;
extern NETWORK_MANAGER_API const std::string AcceptLanguage;
extern NETWORK_MANAGER_API const std::string Authorization;
extern NETWORK_MANAGER_API const std::string CacheControl;
extern NETWORK_MANAGER_API const std::string Connection;
extern NETWORK_MANAGER_API const std::string ContentEncoding;
extern NETWORK_MANAGER_API const std::string ContentLength;
extern NETWORK_MANAGER_API const std::string ContentType;
extern NETWORK_MANAGER_API const std::string ContentDisposition;
extern NETWORK_MANAGER_API const std::string Cookie;
extern NETWORK_MANAGER_API const std::string Host;
extern NETWORK_MANAGER_API const std::string Origin;
extern NETWORK_MANAGER_API const std::string Referer;
extern NETWORK_MANAGER_API const std::string UserAgent;
extern NETWORK_MANAGER_API const std::string IfNoneMatch;
extern NETWORK_MANAGER_API const std::string IfModifiedSince;
} // namespace common_headers

namespace common_content_types {
extern NETWORK_MANAGER_API const std::string ApplicationJson;
extern NETWORK_MANAGER_API const std::string ApplicationXml;
extern NETWORK_MANAGER_API const std::string ApplicationXWwwFormUrlencoded;
extern NETWORK_MANAGER_API const std::string ApplicationXOctetStream;
extern NETWORK_MANAGER_API const std::string ApplicationXGzip;
extern NETWORK_MANAGER_API const std::string MultipartFormData;
} // namespace common_content_types
struct NETWORK_MANAGER_API Header final
{
    std::string Name;
    std::string Value;

    bool hasSameName(const Header& header) const;
    bool hasSameName(const std::string& name) const;

    static Header Parse(const std::string& header);
};

class NETWORK_MANAGER_API HeadersList final
{
    using HeadersStorageType = std::vector<Header>;
public:
    using HeadersIterator = HeadersStorageType::iterator;
    using HeadersConstIterator = HeadersStorageType::const_iterator;

    void setHeader(const Header& header);
    void setHeader(const std::string& headerName, std::string headerValue);

    void addHeader(Header header);
    void addHeader(std::string headerName, std::string headerValue);

    bool hasHeader(const std::string& headerName) const noexcept;

    std::string getHeaderValue(const std::string& headerName) const;

    const Header* getHeader(size_t idx) const noexcept;
    const Header* getHeader(const std::string& name) const noexcept;

    size_t getHeadersCount() const noexcept;

    HeadersIterator begin() noexcept;
    HeadersIterator end() noexcept;

    HeadersConstIterator begin() const noexcept;
    HeadersConstIterator end() const noexcept;
private:
    Header* getHeader(size_t idx) noexcept;
    Header* getHeader(const std::string& headerName) noexcept;

    HeadersStorageType mHeaders;
};
}
}
