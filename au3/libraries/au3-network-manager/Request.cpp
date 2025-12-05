/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file Request.cpp
 @brief Define a class for constructing HTTP requests.

 Dmitry Vedenko
 **********************************************************************/

/*!********************************************************************

 @class Request
 @brief Class to construct the HTTP request.

 **********************************************************************/

#include "Request.h"

#include <cctype>
#include <algorithm>

namespace audacity {
namespace network_manager {
Request::Request(std::string url) noexcept
    : mUrl(std::move(url))
{
}

Request& Request::setURL(std::string url) noexcept
{
    mUrl = std::move(url);

    return *this;
}

const std::string& Request::getURL() const noexcept
{
    return mUrl;
}

Request& Request::setHeader(const std::string& name, std::string value)
{
    mHeaders.setHeader(name, std::move(value));

    return *this;
}

std::string Request::getHeader(const std::string& name) const
{
    return mHeaders.getHeaderValue(name);
}

const HeadersList& Request::getHeaders() const noexcept
{
    return mHeaders;
}

Request& Request::setCookie(const std::string& name, std::string value)
{
    mCookies.setCookie(name, std::move(value));

    return *this;
}

std::string Request::getCookie(const std::string& name) const
{
    return mCookies.getCookieValue(name);
}

const CookiesList& Request::getCookies() noexcept
{
    return mCookies;
}

Request& Request::setMaxRedirects(size_t redirects) noexcept
{
    mMaxRedirects = redirects;

    return *this;
}

size_t Request::getMaxRedirects() const noexcept
{
    return mMaxRedirects;
}

Request& Request::setTimeout(Timeout timeout) noexcept
{
    mTimeout = timeout;
    return *this;
}

Request::Timeout Request::getTimeout() const noexcept
{
    return mTimeout;
}

Request& Request::appendCookies(const CookiesList& list)
{
    for (const Cookie& cookie : list) {
        if (!cookie.isExpired()) {
            mCookies.setCookie(cookie);
        }
    }

    return *this;
}
}
}
