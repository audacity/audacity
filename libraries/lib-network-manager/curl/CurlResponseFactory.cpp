/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file CurlResponseFactory.cpp
 @brief Define an implementation of IResponseFactory using libcurl.

 Dmitry Vedenko
 **********************************************************************/

#include "CurlResponseFactory.h"

#include <algorithm>

#include "CurlResponse.h"

namespace audacity
{
namespace network_manager
{

constexpr decltype(std::thread::hardware_concurrency ()) MIN_CURL_THREADS = 6;

CurlResponseFactory::CurlResponseFactory ()
    : mThreadPool (std::make_unique<ThreadPool>(
        std::max (
            MIN_CURL_THREADS, 
            std::thread::hardware_concurrency ()
        )))
{

}

void CurlResponseFactory::setProxy (const std::string& proxy)
{
    mHandleManager->setProxy (proxy);
}

ResponsePtr CurlResponseFactory::performRequest (RequestVerb verb, const Request& request)
{
    return performRequest (verb, request, nullptr, 0);
}

ResponsePtr CurlResponseFactory::performRequest (RequestVerb verb, const Request& request, const void* data, size_t size)
{
    if (!mThreadPool)
        return {};

    std::shared_ptr<CurlResponse> response = std::make_shared<CurlResponse> (
        verb, request, mHandleManager.get ()
    );

    std::vector<uint8_t> buffer;

    if (data != nullptr && size != 0)
    {
        const uint8_t* start = static_cast<const uint8_t*>(data);
        const uint8_t* end = static_cast<const uint8_t*>(data) + size;

        buffer.insert (buffer.begin (), start, end);
    }

    mThreadPool->enqueue ([response, dataBuffer = std::move (buffer)]() {
        if (!dataBuffer.empty())
            response->perform (dataBuffer.data (), dataBuffer.size ());
        else
            response->perform (nullptr, 0);
    });

    return response;
}

void CurlResponseFactory::terminate ()
{
    mThreadPool.reset ();
    mHandleManager.reset ();
}

}
}
