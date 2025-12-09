/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file Request.h
 @brief Declare a class for constructing HTTP requests.

 Dmitry Vedenko
 **********************************************************************/

#pragma once

#include <string>
#include <chrono>
#include <numeric>

#include "NetworkManagerApi.h"

#include "HeadersList.h"
#include "CookiesList.h"

namespace audacity {
namespace network_manager {
class NETWORK_MANAGER_API Request final
{
public:
    using Timeout = std::chrono::milliseconds;

    static constexpr size_t INFINITE_REDIRECTS = std::numeric_limits<size_t>::max();

    Request() = default;
    explicit Request(std::string url) noexcept;

    Request(const Request&) = default;
    Request(Request&&) = default;
    Request& operator =(const Request&) = default;
    Request& operator =(Request&&) = default;

    Request& setURL(std::string url) noexcept;
    const std::string& getURL() const noexcept;

    Request& setHeader(const std::string& name, std::string value);
    std::string getHeader(const std::string& name) const;

    const HeadersList& getHeaders() const noexcept;

    Request& setCookie(const std::string& name, std::string value);
    Request& appendCookies(const CookiesList& list);

    std::string getCookie(const std::string& name) const;

    const CookiesList& getCookies() noexcept;

    Request& setMaxRedirects(size_t redirects) noexcept;
    size_t getMaxRedirects() const noexcept;

    Request& setTimeout(Timeout timeout) noexcept;
    Timeout getTimeout() const noexcept;
private:
    std::string mUrl;

    HeadersList mHeaders;
    CookiesList mCookies;

    size_t mMaxRedirects { INFINITE_REDIRECTS };

    Timeout mTimeout { std::chrono::seconds(5) };
};
}
}
