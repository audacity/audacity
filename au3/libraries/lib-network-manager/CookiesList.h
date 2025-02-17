/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file CookiesList.h
 @brief Define HTTP cookies list class.

 Dmitry Vedenko
 **********************************************************************/

#pragma once

#include <string>
#include <vector>
#include <chrono>

#include "NetworkManagerApi.h"

namespace audacity {
namespace network_manager {
using ExpiresTime = std::chrono::system_clock::time_point;

struct NETWORK_MANAGER_API Cookie final
{
    std::string Name;
    std::string Value;

    ExpiresTime Expires {};

    static Cookie Parse(const std::string& cookieString);

    bool isSession() const noexcept;
    bool isExpired() const noexcept;

    std::string toString(bool fullString) const;
};

class NETWORK_MANAGER_API CookiesList final
{
    using CookiesStorageType = std::vector<Cookie>;
public:
    using CookiesIterator = CookiesStorageType::iterator;
    using CookiesConstIterator = CookiesStorageType::const_iterator;

    void setCookie(const Cookie& cookie);
    void setCookie(const std::string& cookieName, std::string cookieValue);

    void addCookie(Cookie cookie);
    void addCookie(std::string cookieName, std::string cookieValue);

    bool hasCookie(const std::string& cookieName) const noexcept;

    std::string getCookieValue(const std::string& cookieName) const;

    const Cookie* getCookie(size_t idx) const noexcept;
    const Cookie* getCookie(const std::string& name) const noexcept;

    size_t getCookiesCount() const noexcept;

    std::string getCookiesString() const;

    CookiesIterator begin() noexcept;
    CookiesIterator end() noexcept;

    CookiesConstIterator begin() const noexcept;
    CookiesConstIterator end() const noexcept;
private:
    Cookie* getCookie(size_t idx) noexcept;
    Cookie* getCookie(const std::string& cookieName) noexcept;

    CookiesStorageType mCookies;
};
}
}
