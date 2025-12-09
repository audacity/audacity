/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file HeadersList.cpp
 @brief Define HTTP headers list class.

 Dmitry Vedenko
 **********************************************************************/

/*!********************************************************************

 @class Header
 @brief A string pair, representing HTTP header.

 **********************************************************************/

/*!********************************************************************

 @class HeadersList
 @brief A class, representing a list of HTTP headers.

 **********************************************************************/

#include "HeadersList.h"

#include <algorithm>
#include <cctype>

namespace audacity {
namespace network_manager {
namespace common_headers {
const std::string Accept = "Accept";
const std::string AcceptEncoding = "Accept-Encoding";
const std::string AcceptLanguage = "Accept-Language";
const std::string Authorization = "Authorization";
const std::string CacheControl = "Cache-Control";
const std::string Connection = "Connection";
const std::string ContentEncoding = "Content-Encoding";
const std::string ContentLength = "Content-Length";
const std::string ContentType = "Content-Type";
const std::string ContentDisposition = "Content-Disposition";
const std::string Cookie = "Cookie";
const std::string Host = "Host";
const std::string Origin = "Origin";
const std::string Referer = "Referer";
const std::string UserAgent = "User-Agent";
const std::string IfNoneMatch = "If-None-Match";
const std::string IfModifiedSince = "If-Modified-Since";
} // namespace common_headers

namespace common_content_types {
const std::string ApplicationJson = "application/json";
const std::string ApplicationXml = "application/xml";
const std::string ApplicationXWwwFormUrlencoded = "application/x-www-form-urlencoded";
const std::string ApplicationXOctetStream = "application/x-octet-stream";
const std::string ApplicationXGzip = "application/x-gzip";
const std::string MultipartFormData = "multipart/form-data";
} // namespace common_content_types

bool Header::hasSameName(const Header& header) const
{
    return hasSameName(header.Name);
}

bool Header::hasSameName(const std::string& name) const
{
    return std::equal(
        name.begin(), name.end(),
        Name.begin(), Name.end(),
        [](const char leftChar, const char rightChar) {
        return std::tolower(leftChar) == std::tolower(rightChar);
    });
}

Header Header::Parse(const std::string& header)
{
    const size_t colonPosition = header.find(": ");

    if (colonPosition == std::string::npos) { // This can happen when we receive the first line of the response
        return { header, std::string() }
    }

    return {
        header.substr(0, colonPosition),
        header.substr(colonPosition + 2)
    };
}

void HeadersList::setHeader(const Header& header)
{
    setHeader(header.Name, header.Value);
}

void HeadersList::setHeader(const std::string& headerName, std::string headerValue)
{
    Header* item = getHeader(headerName);

    if (item != nullptr) {
        item->Value = std::move(headerValue);
    } else {
        mHeaders.push_back({ headerName, std::move(headerValue) });
    }
}

void HeadersList::addHeader(Header header)
{
    addHeader(std::move(header.Name), std::move(header.Value));
}

void HeadersList::addHeader(std::string headerName, std::string headerValue)
{
    mHeaders.push_back({ std::move(headerName), std::move(headerValue) });
}

bool HeadersList::hasHeader(const std::string& headerName) const noexcept
{
    return getHeader(headerName) != nullptr;
}

std::string HeadersList::getHeaderValue(const std::string& headerName) const
{
    const Header* header = getHeader(headerName);

    if (header != nullptr) {
        return header->Value;
    }

    return {};
}

const Header* HeadersList::getHeader(size_t idx) const noexcept
{
    return const_cast<HeadersList*>(this)->getHeader(idx);
}

const Header* HeadersList::getHeader(const std::string& name) const noexcept
{
    return const_cast<HeadersList*>(this)->getHeader(name);
}

size_t HeadersList::getHeadersCount() const noexcept
{
    return mHeaders.size();
}

HeadersList::HeadersIterator HeadersList::begin() noexcept
{
    return mHeaders.begin();
}

HeadersList::HeadersIterator HeadersList::end() noexcept
{
    return mHeaders.end();
}

HeadersList::HeadersConstIterator HeadersList::begin() const noexcept
{
    return mHeaders.begin();
}

HeadersList::HeadersConstIterator HeadersList::end() const noexcept
{
    return mHeaders.end();
}

Header* HeadersList::getHeader(size_t idx) noexcept
{
    if (idx < mHeaders.size()) {
        return &mHeaders[idx];
    }

    return nullptr;
}

Header* HeadersList::getHeader(const std::string& headerName) noexcept
{
    for (Header& header : mHeaders) {
        if (header.hasSameName(headerName)) {
            return &header;
        }
    }

    return nullptr;
}
}
}
