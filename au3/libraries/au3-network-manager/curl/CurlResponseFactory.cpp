/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file CurlResponseFactory.cpp
 @brief Define an implementation of IResponseFactory using libcurl.

 Dmitry Vedenko
 **********************************************************************/
#include "CurlResponseFactory.h"

#include "CurlResponse.h"
#include "MultipartData.h"
#include "RequestPayload.h"

#include "ThreadPool/ThreadPool.h"

namespace audacity {
namespace network_manager {
namespace {
class StubResponse final : public IResponse
{
public:
    explicit StubResponse(const Request& request)
        : mRequest{request}
    {
    }

    bool isFinished() const noexcept override
    {
        return true;
    }

    unsigned getHTTPCode() const noexcept override
    {
        return 0;
    }

    NetworkError getError() const noexcept override
    {
        return NetworkError::OperationCancelled;
    }

    std::string getErrorString() const override
    {
        return {};
    }

    bool headersReceived() const noexcept override
    {
        return false;
    }

    bool hasHeader(const std::string& headerName) const noexcept override
    {
        return false;
    }

    std::string getHeader(const std::string& headerName) const override
    {
        return {};
    }

    const HeadersList& getHeaders() const noexcept override
    {
        static HeadersList empty;
        return empty;
    }

    const CookiesList& getCookies() const noexcept override
    {
        static CookiesList empty;
        return empty;
    }

    const Request& getRequest() const noexcept override
    {
        return mRequest;
    }

    std::string getURL() const override
    {
        return {};
    }

    void abort() noexcept override
    {
    }

    void setOnDataReceivedCallback(RequestCallback) override
    {
    }

    void setRequestFinishedCallback(RequestCallback callback) override
    {
        if (callback) {
            callback(this);
        }
    }

    void setDownloadProgressCallback(ProgressCallback) override
    {
    }

    void setUploadProgressCallback(ProgressCallback) override
    {
    }

    uint64_t getBytesAvailable() const noexcept override
    {
        return 0;
    }

    uint64_t readData(void*, uint64_t) override
    {
        return 0;
    }

private:
    Request mRequest;
};
} // namespace

constexpr decltype(std::thread::hardware_concurrency()) MIN_CURL_THREADS = 6;

CurlResponseFactory::CurlResponseFactory ()
    : mThreadPool(std::make_unique<ThreadPool>(
                      std::max(
                          MIN_CURL_THREADS,
                          std::thread::hardware_concurrency()
                          )))
{
}

CurlResponseFactory::~CurlResponseFactory ()
{
}

void CurlResponseFactory::setProxy(const std::string& proxy)
{
    mHandleManager->setProxy(proxy);
}

ResponsePtr CurlResponseFactory::performRequest(RequestVerb verb, const Request& request)
{
    return performRequest(verb, request, RequestPayloadStreamPtr {});
}

ResponsePtr CurlResponseFactory::performRequest(
    RequestVerb verb, const Request& request,
    RequestPayloadStreamPtr payloadStream)
{
    if (!mThreadPool) {
        return std::make_shared<StubResponse>(request);
    }

    auto response
        =std::make_shared<CurlResponse>(verb, request, mHandleManager.get());

    mThreadPool->enqueue(
        [response, payloadStream = std::move(payloadStream)]()
    {
        if (payloadStream) {
            response->setPayload(payloadStream);
        }

        response->perform();
    });

    return response;
}

ResponsePtr CurlResponseFactory::performRequest(
    RequestVerb verb, const Request& request,
    std::unique_ptr<MultipartData> form)
{
    if (!mThreadPool) {
        return std::make_shared<StubResponse>(request);
    }

    std::shared_ptr<CurlResponse> response
        =std::make_shared<CurlResponse>(verb, request, mHandleManager.get());

    mThreadPool->enqueue(
        [response, rawForm = form.release()]() mutable
    {
        if (rawForm != nullptr && !rawForm->IsEmpty()) {
            response->setForm(std::unique_ptr<MultipartData>(rawForm));
        }

        response->perform();
    });

    return response;
}

void CurlResponseFactory::terminate()
{
    mThreadPool.reset();
    mHandleManager.reset();
}
}
}
