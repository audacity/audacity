/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file CurlHandleManager.h
 @brief Declare a class responsible for reuse of CURL handles.

 Dmitry Vedenko
 **********************************************************************/

#pragma once

#include <string>
#include <chrono>
#include <vector>
#include <mutex>

#include <curl/curl.h>

#include "CurlStringList.h"
#include "../IResponseFactory.h"

#include "../CookiesList.h"
#include "../HeadersList.h"

namespace audacity {
namespace network_manager {
class CurlHandleManager final
{
public:
    class Handle final
    {
        friend class CurlHandleManager;
        Handle (CurlHandleManager* owner, CURL* handle, RequestVerb verb, std::string url) noexcept;
    public:
        Handle (Handle&& rhs) noexcept;
        ~Handle () noexcept;

        Handle& operator =(Handle&& rhs) noexcept;

        template<typename ... Args>
        CURLcode setOption(CURLoption option, Args... value) noexcept
        {
            return curl_easy_setopt(mHandle, option, value ...);
        }

        CURLcode setOption(CURLoption option, const std::string& value) noexcept;

        CURLcode appendCookie(const Cookie& cookie) noexcept;
        CURLcode appendCookies(const CookiesList& cookie) noexcept;

        void appendHeader(const Header& header);
        void appendHeaders(const HeadersList& headers);

        struct Result final
        {
            CURLcode Code;
            std::string Message;
        };

        Result perform();

        void markKeepAlive();

        bool isHandleFromCache() const noexcept;

        unsigned getHTTPCode() const noexcept;

        void reset() noexcept;

        CURL* getCurlHandle() const noexcept;

        void disableSSLValidation();
        void enableSSLValidation();
    private:
        CURL* mHandle { nullptr };
        CurlHandleManager* mOwner { nullptr };

        RequestVerb mVerb;
        std::string mUrl;

        CurlStringList mHeaders;

        bool mUserAgentSet { false };
        bool mReuse { false };
        bool mHandleFromCache { false };
    };

    CurlHandleManager ();
    ~CurlHandleManager ();

    void setProxy(std::string proxy);

    Handle getHandle(RequestVerb verb, const std::string& url);
private:
    using RequestClock = std::chrono::steady_clock;
    using RequestTimePoint = RequestClock::time_point;

    static constexpr std::chrono::milliseconds KEEP_ALIVE_IDLE { std::chrono::seconds(120) };
    static constexpr std::chrono::milliseconds KEEP_ALIVE_PROBE { std::chrono::seconds(60) };

    struct CachedHandle final
    {
        RequestVerb Verb;
        std::string SchemeAndDomain;

        CURL* Handle { nullptr };

        RequestTimePoint RequestTime;
    };

    std::string getUserAgent() const;

    CURL* getCurlHandleFromCache(RequestVerb verb, const std::string& url);
    void cacheHandle(Handle& handle);

    void cleanupHandlesCache();

    static std::string GetSchemeAndDomain(const std::string& url);

    std::string mProxy;
    std::string mUserAgent;

    std::mutex mHandleCacheLock;
    std::vector<CachedHandle> mHandleCache;
};
}
}
