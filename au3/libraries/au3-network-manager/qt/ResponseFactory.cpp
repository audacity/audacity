/*!********************************************************************
 Audacity: A Digital Audio Editor
 **********************************************************************/

#include "ResponseFactory.h"

#include "Response.h"
#include "../MultipartData.h"
#include "../RequestPayload.h"

#include <QNetworkAccessManager>
#include <QNetworkProxy>
#include <QThread>
#include <QCoreApplication>
#include <QUrl>

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

    bool hasHeader(const std::string&) const noexcept override
    {
        return false;
    }

    std::string getHeader(const std::string&) const override
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

ResponseFactory::ResponseFactory()
{
    mNetworkThread = new QThread();
    mNetworkThread->setObjectName("NetworkThread");

    mNetworkManager = new QNetworkAccessManager();
    mNetworkManager->setTransferTimeout();

    mNetworkManager->moveToThread(mNetworkThread);

    mNetworkThread->start();
}

ResponseFactory::~ResponseFactory()
{
    terminate();
}

void ResponseFactory::setProxy(const std::string& proxy)
{
    if (mTerminated || !mNetworkManager) {
        return;
    }

    QNetworkProxy networkProxy;

    if (proxy.empty()) {
        networkProxy.setType(QNetworkProxy::NoProxy);
    } else {
        QUrl proxyUrl = QUrl::fromUserInput(QString::fromStdString(proxy));

        if (proxyUrl.scheme().toLower() == "socks5") {
            networkProxy.setType(QNetworkProxy::Socks5Proxy);
        } else {
            networkProxy.setType(QNetworkProxy::HttpProxy);
        }

        networkProxy.setHostName(proxyUrl.host());
        networkProxy.setPort(static_cast<quint16>(proxyUrl.port(8080)));

        if (!proxyUrl.userName().isEmpty()) {
            networkProxy.setUser(proxyUrl.userName());
            networkProxy.setPassword(proxyUrl.password());
        }
    }

    QMetaObject::invokeMethod(mNetworkManager, [this, networkProxy]() {
        mNetworkManager->setProxy(networkProxy);
    }, Qt::QueuedConnection);
}

ResponsePtr ResponseFactory::performRequest(RequestVerb verb, const Request& request)
{
    return performRequest(verb, request, RequestPayloadStreamPtr {});
}

ResponsePtr ResponseFactory::performRequest(
    RequestVerb verb, const Request& request,
    RequestPayloadStreamPtr payloadStream)
{
    if (mTerminated || !mNetworkManager) {
        return std::make_shared<StubResponse>(request);
    }

    auto response = std::make_shared<Response>(verb, request, mNetworkManager);

    response->moveToThread(mNetworkThread);

    QMetaObject::invokeMethod(response.get(),
                              [response, payloadStream = std::move(payloadStream)]() mutable {
        if (payloadStream) {
            response->setPayload(std::move(payloadStream));
        }
        response->perform();
    }, Qt::QueuedConnection);

    return response;
}

ResponsePtr ResponseFactory::performRequest(
    RequestVerb verb, const Request& request,
    std::unique_ptr<MultipartData> form)
{
    if (mTerminated || !mNetworkManager) {
        return std::make_shared<StubResponse>(request);
    }

    auto response = std::make_shared<Response>(verb, request, mNetworkManager);

    response->moveToThread(mNetworkThread);

    QMetaObject::invokeMethod(response.get(),
                              [response, rawForm = form.release()]() mutable {
        if (rawForm != nullptr && !rawForm->IsEmpty()) {
            response->setForm(std::unique_ptr<MultipartData>(rawForm));
        }
        response->perform();
    }, Qt::QueuedConnection);

    return response;
}

void ResponseFactory::terminate()
{
    if (mTerminated) {
        return;
    }

    mTerminated = true;

    if (mNetworkThread) {
        mNetworkThread->quit();
        if (!mNetworkThread->wait(5000)) {
            mNetworkThread->terminate();
            mNetworkThread->wait();
        }

        delete mNetworkThread;
        mNetworkThread = nullptr;
    }
    delete mNetworkManager;
    mNetworkManager = nullptr;
}
}
}
