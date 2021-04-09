#include "CurlHandleManager.h"

#include <algorithm>

namespace audacity
{
namespace network_manager
{
constexpr std::chrono::milliseconds CurlHandleManager::KEEP_ALIVE_IDLE;
constexpr std::chrono::milliseconds CurlHandleManager::KEEP_ALIVE_PROBE;

CurlHandleManager::Handle::Handle(CurlHandleManager* owner, CURL* handle, RequestVerb verb, std::string url) noexcept
    : mHandle (handle),
    mOwner (owner),
    mVerb (verb),
    mUrl (std::move (url)),
    mHandleFromCache (handle != nullptr)
{
    if (mHandle == nullptr)
        mHandle = curl_easy_init ();

    setOption (CURLOPT_URL, mUrl);
        
    switch (verb)
    {
    case RequestVerb::Head:
        setOption (CURLOPT_NOBODY, 1);
        break;
    case RequestVerb::Get:
        // This is a default, no additional setup is needed
        // We cache handles by the verb, so there is no need to 
        // reset the handle state
        break;
    case RequestVerb::Post:
        setOption (CURLOPT_POST, 1);
        break;
    case RequestVerb::Put:
        setOption (CURLOPT_UPLOAD, 1);
        break;
    case RequestVerb::Delete:
        setOption (CURLOPT_CUSTOMREQUEST, "DELETE");
        break;
    }

    setOption (CURLOPT_NOSIGNAL, 1L);

    setOption (CURLOPT_SSL_VERIFYPEER, 1L);
    setOption (CURLOPT_SSL_VERIFYHOST, 2L);

    setOption (CURLOPT_ACCEPT_ENCODING, "");
}

CurlHandleManager::Handle::Handle (Handle&& rhs) noexcept
{
    *this = std::move (rhs);
}

CurlHandleManager::Handle::~Handle () noexcept
{
    if (mReuse)
        mOwner->cacheHandle (*this);
    else
        curl_easy_cleanup (mHandle);
}

CurlHandleManager::Handle& CurlHandleManager::Handle::operator=(Handle&& rhs) noexcept
{
    std::swap (mHandle, rhs.mHandle);
    std::swap (mOwner, rhs.mOwner);

    std::swap (mVerb, rhs.mVerb);
    
    mUrl = std::move (rhs.mUrl);
    mHeaders = std::move (rhs.mHeaders);

    mReuse = rhs.mReuse;
    rhs.mReuse = false;

    return *this;
}

CURLcode CurlHandleManager::Handle::setOption (CURLoption option, const std::string& value) noexcept
{
    return setOption (option, value.c_str ());
}

CURLcode CurlHandleManager::Handle::appendCookie (const Cookie& cookie) noexcept
{
    return setOption(CURLOPT_COOKIE, "Set-Cookie: " + cookie.Name + "=" + cookie.Value);
}

CURLcode CurlHandleManager::Handle::appendCookies (const CookiesList& cookies)noexcept
{
    for (const Cookie& cookie : cookies)
    {
        const CURLcode result = appendCookie (cookie);

        if (result != CURLE_OK)
            return result;
    }

    return CURLE_OK;
}

void CurlHandleManager::Handle::appendHeader (const Header& header)
{
    mHeaders.append(header.Name + ": " + header.Value);
}

void CurlHandleManager::Handle::appendHeaders (const HeadersList& headers)
{
    for (const Header& header : headers)
        appendHeader (header);
}

CurlHandleManager::Handle::Result CurlHandleManager::Handle::perform ()
{
    CURLcode result = setOption (CURLOPT_HTTPHEADER, mHeaders.getCurlList ());

    if (result != CURLE_OK)
        return { result, std::string () };

    char currentError[CURL_ERROR_SIZE] = {};
    setOption(CURLOPT_ERRORBUFFER, currentError);

    result = curl_easy_perform (mHandle);

    mReuse = mReuse && result == CURLE_OK;

    return { result, std::string (currentError) };
}

void CurlHandleManager::Handle::markKeepAlive ()
{
    mReuse = true;
}

bool CurlHandleManager::Handle::isHandleFromCache () const noexcept
{
    return mHandleFromCache;
}

unsigned CurlHandleManager::Handle::getHTTPCode () const noexcept
{
    long code = 0;

    if (CURLE_OK != curl_easy_getinfo(mHandle, CURLINFO_RESPONSE_CODE, &code))
        return 0;

    return code;
}

CurlHandleManager::~CurlHandleManager ()
{
    std::lock_guard<std::mutex> lock (mHandleCacheLock);

    for (auto& cachedHandle : mHandleCache)
        curl_easy_cleanup (cachedHandle.Handle);
}

void CurlHandleManager::setProxy (std::string proxy)
{
    mProxy = std::move (proxy);
}

CurlHandleManager::Handle CurlHandleManager::getHandle (RequestVerb verb, const std::string& url)
{
    Handle handle (this, getCurlHandleFromCache (verb, url), verb, url);

    if (!mProxy.empty ())
    {
        handle.setOption (CURLOPT_PROXY, mProxy);
        // If we use proxy, checking the CRL will likely break the SSL proxying
        handle.setOption (CURLOPT_SSL_OPTIONS, CURLSSLOPT_NO_REVOKE);
    }

    handle.setOption (CURLOPT_TCP_KEEPALIVE, 1L);

    handle.setOption (CURLOPT_TCP_KEEPIDLE, 
        std::chrono::duration_cast<std::chrono::seconds> (KEEP_ALIVE_IDLE).count ()
    );

    handle.setOption (CURLOPT_TCP_KEEPINTVL, 
        std::chrono::duration_cast<std::chrono::seconds> (KEEP_ALIVE_PROBE).count ()
    );

    return handle;
}

CURL* CurlHandleManager::getCurlHandleFromCache (RequestVerb verb, const std::string& url)
{
    std::lock_guard<std::mutex> lock (mHandleCacheLock);

    cleanupHandlesCache ();

    const std::string schemeAndDomain = GetSchemeAndDomain (url);

    auto it = std::find_if (mHandleCache.begin (), mHandleCache.end (), [verb, schemeAndDomain](const CachedHandle& handle) {
        return handle.Verb == verb && handle.SchemeAndDomain == schemeAndDomain;
    });

    if (it == mHandleCache.end ())
        return nullptr;

    CURL* handle = it->Handle;

    mHandleCache.erase (it);

    return handle;
}

void CurlHandleManager::cacheHandle (Handle& handle)
{
    // Reset the state to the safe defaults
    handle.setOption (CURLOPT_COOKIELIST, nullptr);
    handle.setOption (CURLOPT_PROXY, nullptr);
    handle.setOption (CURLOPT_SSL_OPTIONS, 0);

    std::lock_guard<std::mutex> lock (mHandleCacheLock);

    cleanupHandlesCache ();

    mHandleCache.push_back ({
        handle.mVerb,
        GetSchemeAndDomain (handle.mUrl),
        handle.mHandle,
        RequestClock::now ()
    });
}

void CurlHandleManager::cleanupHandlesCache ()
{
    const RequestTimePoint timePoint = RequestClock::now ();

    mHandleCache.erase (std::remove_if (mHandleCache.begin (), mHandleCache.end (), [timePoint](const CachedHandle& cachedHandle) {
        return (timePoint - cachedHandle.RequestTime) >= KEEP_ALIVE_IDLE;
    }), mHandleCache.end ());
}

std::string CurlHandleManager::GetSchemeAndDomain (const std::string& url)
{
    const size_t schemeEndPosition = url.find ("://");

    if (schemeEndPosition == std::string::npos) // Is url even valid?
        return url;

    const size_t domainStartPosition = schemeEndPosition + 3;

    const size_t slashLocation = url.find ('/', domainStartPosition);

    if (slashLocation == std::string::npos)
        return url;

    return url.substr (domainStartPosition, slashLocation - domainStartPosition);
}

}
}