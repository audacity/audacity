/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file CurlResponseFactory.h
 @brief Declare an implementation of IResponseFactory using libcurl.

 Dmitry Vedenko
 **********************************************************************/

#pragma once

#include <memory>

#include "../IResponseFactory.h"

#include "CurlHandleManager.h"

class ThreadPool;

namespace audacity {
namespace network_manager {
class CurlResponseFactory final : public IResponseFactory
{
public:
    CurlResponseFactory ();
    ~CurlResponseFactory ();

    void setProxy(const std::string& proxy) override;

    ResponsePtr performRequest(RequestVerb verb, const Request& request) override;
    ResponsePtr performRequest(
        RequestVerb verb, const Request& request, RequestPayloadStreamPtr payloadStream) override;
    ResponsePtr performRequest(RequestVerb verb, const Request& request, std::unique_ptr<MultipartData> form) override;

    void terminate() override;

private:
    std::unique_ptr<CurlHandleManager> mHandleManager { std::make_unique<CurlHandleManager> () };
    std::unique_ptr<ThreadPool> mThreadPool;
};
}
}
