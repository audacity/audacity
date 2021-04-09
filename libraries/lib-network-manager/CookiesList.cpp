#include "CookiesList.h"

namespace audacity
{
namespace network_manager
{

Cookie Cookie::Parse (const std::string& cookieString)
{
    const size_t equalsPosition = cookieString.find ('=');

    if (equalsPosition == std::string::npos)
        return { cookieString, std::string () };

    std::string name = cookieString.substr (0, equalsPosition);

    const size_t firstValueIndex = equalsPosition + 1;

    const size_t semicolonPosition = cookieString.find (';', firstValueIndex);

    if (semicolonPosition == std::string::npos)
        return { std::move (name), cookieString.substr (firstValueIndex) };
    else
        return { std::move (name), cookieString.substr (firstValueIndex, semicolonPosition - firstValueIndex) };
}

void CookiesList::setCookie (const Cookie& cookie)
{
    setCookie (cookie.Name, cookie.Value);
}

void CookiesList::setCookie (const std::string& cookieName, std::string cookieValue)
{
    Cookie* item = getCookie (cookieName);

    if (item != nullptr)
        item->Value = std::move (cookieValue);
    else
        mCookies.push_back ({ cookieName, std::move (cookieValue) });
}

void CookiesList::addCookie (Cookie cookie)
{
    addCookie (std::move (cookie.Name), std::move (cookie.Value));
}

void CookiesList::addCookie (std::string cookieName, std::string cookieValue)
{
    mCookies.push_back ({ std::move (cookieName), std::move (cookieValue) });
}

bool CookiesList::hasCookie (const std::string& cookieName) const noexcept
{
    return getCookie (cookieName) != nullptr;
}

std::string CookiesList::getCookieValue (const std::string& cookieName) const
{
    const Cookie* item = getCookie (cookieName);

    if (item != nullptr)
        return item->Value;

    return {};
}

const Cookie* CookiesList::getCookie (size_t idx) const noexcept
{
    return const_cast<CookiesList*>(this)->getCookie(idx);
}

const Cookie* CookiesList::getCookie (const std::string& name) const noexcept
{
    return const_cast<CookiesList*>(this)->getCookie (name);
}

size_t CookiesList::getCookiesCount () const noexcept
{
    return size_t ();
}

std::string CookiesList::getCookiesString () const
{
    std::string result;

    for (const Cookie& cookie : mCookies)
    {
        if (!result.empty ())
            result.push_back (';');

        result += cookie.Name + "=" + cookie.Value;
    }

    return result;
}

CookiesList::CookiesIterator CookiesList::begin () noexcept
{
    return mCookies.begin ();
}

CookiesList::CookiesIterator CookiesList::end () noexcept
{
    return mCookies.end ();
}

CookiesList::CookiesConstIterator CookiesList::begin () const noexcept
{
    return mCookies.begin ();
}

CookiesList::CookiesConstIterator CookiesList::end () const noexcept
{
    return mCookies.end ();
}

Cookie* CookiesList::getCookie (size_t idx) noexcept
{
    if (idx < mCookies.size ())
        return &mCookies[idx];

    return nullptr;
}

Cookie* CookiesList::getCookie (const std::string& cookieName) noexcept
{
    for (Cookie& cookie : mCookies)
    {
        if (cookie.Name == cookieName)
            return &cookie;
    }

    return nullptr;
}

}
}
