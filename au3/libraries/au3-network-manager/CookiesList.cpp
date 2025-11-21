/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file CookiesList.cpp
 @brief Define HTTP cookies list class.

 Dmitry Vedenko
 **********************************************************************/

/*!********************************************************************

 @class Cookie
 @brief A struct, representing a Cookie object.

 **********************************************************************/

/*!********************************************************************

 @class CookiesList
 @brief A class, representing a list of HTTP cookies.

 **********************************************************************/

#include "CookiesList.h"

#include <sstream>
#include <cstring>
#include <algorithm>

#include "DateTimeConversions.h"

namespace audacity {
namespace network_manager {
Cookie Cookie::Parse(const std::string& cookieString)
{
    const size_t equalsPosition = cookieString.find('=');

    if (equalsPosition == std::string::npos) {
        return { cookieString, std::string() }
    }

    std::string name = cookieString.substr(0, equalsPosition);

    const size_t firstValueIndex = equalsPosition + 1;

    const size_t semicolonPosition = cookieString.find(';', firstValueIndex);

    Cookie cookie;

    cookie.Name = std::move(name);
    cookie.Value = semicolonPosition == std::string::npos
                   ? cookieString.substr(firstValueIndex)
                   : cookieString.substr(firstValueIndex, semicolonPosition - firstValueIndex);

    size_t expiresPosition = cookieString.find("Expires=");

    if (expiresPosition != std::string::npos) {
        expiresPosition += std::strlen("Expires=");

        const size_t trailingSemicolon = cookieString.find(';', expiresPosition);

        std::string expiresValue
            =trailingSemicolon == std::string::npos
              ? cookieString.substr(expiresPosition)
              : cookieString.substr(expiresPosition, trailingSemicolon - expiresPosition);

        // Hack around Yandex violating RFC
        std::replace(expiresValue.begin(), expiresValue.end(), '-', ' ');
        ParseRFC822Date(expiresValue, &cookie.Expires);
    }

    return cookie;
}

bool Cookie::isSession() const noexcept
{
    return Expires == ExpiresTime {};
}

std::string Cookie::toString(bool fullString) const
{
    std::ostringstream stream;

    stream << Name << "=" << Value;

    if (fullString) {
        if (!isSession()) {
            stream << "; " << "Expires=" << SerializeRFC822Date(Expires);
        }
    }

    return stream.str();
}

bool Cookie::isExpired() const noexcept
{
    return !isSession()
           && std::chrono::system_clock::now() >= Expires;
}

void CookiesList::setCookie(const Cookie& cookie)
{
    setCookie(cookie.Name, cookie.Value);
}

void CookiesList::setCookie(const std::string& cookieName, std::string cookieValue)
{
    Cookie* item = getCookie(cookieName);

    if (item != nullptr) {
        item->Value = std::move(cookieValue);
    } else {
        mCookies.push_back({ cookieName, std::move(cookieValue) });
    }
}

void CookiesList::addCookie(Cookie cookie)
{
    addCookie(std::move(cookie.Name), std::move(cookie.Value));
}

void CookiesList::addCookie(std::string cookieName, std::string cookieValue)
{
    mCookies.push_back({ std::move(cookieName), std::move(cookieValue) });
}

bool CookiesList::hasCookie(const std::string& cookieName) const noexcept
{
    return getCookie(cookieName) != nullptr;
}

std::string CookiesList::getCookieValue(const std::string& cookieName) const
{
    const Cookie* item = getCookie(cookieName);

    if (item != nullptr) {
        return item->Value;
    }

    return {};
}

const Cookie* CookiesList::getCookie(size_t idx) const noexcept
{
    return const_cast<CookiesList*>(this)->getCookie(idx);
}

const Cookie* CookiesList::getCookie(const std::string& name) const noexcept
{
    return const_cast<CookiesList*>(this)->getCookie(name);
}

size_t CookiesList::getCookiesCount() const noexcept
{
    return mCookies.size();
}

std::string CookiesList::getCookiesString() const
{
    std::string result;

    for (const Cookie& cookie : mCookies) {
        if (!result.empty()) {
            result.push_back(';');
        }

        result += cookie.Name + "=" + cookie.Value;
    }

    return result;
}

CookiesList::CookiesIterator CookiesList::begin() noexcept
{
    return mCookies.begin();
}

CookiesList::CookiesIterator CookiesList::end() noexcept
{
    return mCookies.end();
}

CookiesList::CookiesConstIterator CookiesList::begin() const noexcept
{
    return mCookies.begin();
}

CookiesList::CookiesConstIterator CookiesList::end() const noexcept
{
    return mCookies.end();
}

Cookie* CookiesList::getCookie(size_t idx) noexcept
{
    if (idx < mCookies.size()) {
        return &mCookies[idx];
    }

    return nullptr;
}

Cookie* CookiesList::getCookie(const std::string& cookieName) noexcept
{
    for (Cookie& cookie : mCookies) {
        if (cookie.Name == cookieName) {
            return &cookie;
        }
    }

    return nullptr;
}
}
}
