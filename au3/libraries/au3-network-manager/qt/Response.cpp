/*!********************************************************************
 Audacity: A Digital Audio Editor
 **********************************************************************/

#include "Response.h"

#include <algorithm>

#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QNetworkRequest>
#include <QHttpMultiPart>
#include <QBuffer>
#include <QUrl>

#include "../MultipartData.h"
#include "../RequestPayload.h"

namespace audacity {
namespace network_manager {
Response::Response(
    RequestVerb verb, const Request& request,
    QNetworkAccessManager* networkManager) noexcept
    : mVerb(verb)
    , mRequest(request)
    , mNetworkManager(networkManager)
{
}

Response::~Response()
{
    if (mReply) {
        mReply->disconnect();
        if (!mReply->isFinished()) {
            mReply->abort();
        }
        mReply->deleteLater();
    }

    delete mMultiPart;
}

bool Response::isFinished() const noexcept
{
    std::lock_guard<std::recursive_mutex> lock(mStatusMutex);
    return mRequestFinished;
}

unsigned Response::getHTTPCode() const noexcept
{
    std::lock_guard<std::recursive_mutex> lock(mStatusMutex);
    return mHttpCode;
}

NetworkError Response::getError() const noexcept
{
    std::lock_guard<std::recursive_mutex> lock(mStatusMutex);
    return mNetworkError;
}

std::string Response::getErrorString() const
{
    std::lock_guard<std::recursive_mutex> lock(mStatusMutex);
    return mErrorString;
}

bool Response::headersReceived() const noexcept
{
    std::lock_guard<std::recursive_mutex> lock(mStatusMutex);
    return mHeadersReceived;
}

bool Response::hasHeader(const std::string& headerName) const noexcept
{
    std::lock_guard<std::mutex> lock(mHeadersMutex);
    return mResponseHeaders.hasHeader(headerName);
}

std::string Response::getHeader(const std::string& headerName) const
{
    std::lock_guard<std::mutex> lock(mHeadersMutex);
    return mResponseHeaders.getHeaderValue(headerName);
}

const HeadersList& Response::getHeaders() const noexcept
{
    std::lock_guard<std::mutex> lock(mHeadersMutex);
    return mResponseHeaders;
}

const CookiesList& Response::getCookies() const noexcept
{
    std::lock_guard<std::mutex> lock(mHeadersMutex);
    return mResponseCookies;
}

const Request& Response::getRequest() const noexcept
{
    return mRequest;
}

std::string Response::getURL() const
{
    return mRequest.getURL();
}

void Response::abort() noexcept
{
    {
        std::lock_guard<std::recursive_mutex> lock(mStatusMutex);
        mAbortRequested = true;
    }

    if (mReply && !mReply->isFinished()) {
        mReply->abort();
    }
}

void Response::setOnDataReceivedCallback(RequestCallback callback)
{
    std::lock_guard<std::mutex> lock(mCallbackMutex);

    mOnDataReceivedCallback = std::move(callback);

    if (mOnDataReceivedCallback && getBytesAvailable() > 0) {
        mOnDataReceivedCallback(this);
    }
}

void Response::setRequestFinishedCallback(RequestCallback callback)
{
    std::lock_guard<std::mutex> callbackLock(mCallbackMutex);

    mRequestFinishedCallback = std::move(callback);

    std::lock_guard<std::recursive_mutex> statusLock(mStatusMutex);

    if (mRequestFinishedCallback && mRequestFinished) {
        mRequestFinishedCallback(this);
    }
}

void Response::setDownloadProgressCallback(ProgressCallback callback)
{
    std::lock_guard<std::mutex> callbackLock(mCallbackMutex);
    mDownloadProgressCallback = std::move(callback);
}

void Response::setUploadProgressCallback(ProgressCallback callback)
{
    std::lock_guard<std::mutex> callbackLock(mCallbackMutex);
    mUploadProgressCallback = std::move(callback);
}

uint64_t Response::getBytesAvailable() const noexcept
{
    std::lock_guard<std::mutex> lock(mDataBufferMutex);
    return mDataBuffer.size();
}

uint64_t Response::readData(void* buffer, uint64_t maxBytesCount)
{
    if (buffer == nullptr || maxBytesCount == 0) {
        return 0;
    }

    std::lock_guard<std::mutex> lock(mDataBufferMutex);

    if (mDataBuffer.empty()) {
        return 0;
    }

    maxBytesCount = std::min<uint64_t>(maxBytesCount, mDataBuffer.size());

    const auto begin = mDataBuffer.begin();
    const auto end = begin + maxBytesCount;

    std::copy(begin, end, static_cast<uint8_t*>(buffer));

    mDataBuffer.erase(begin, end);

    return maxBytesCount;
}

void Response::setPayload(RequestPayloadStreamPtr payload)
{
    mPayload = std::move(payload);
}

void Response::setForm(std::unique_ptr<MultipartData> form)
{
    mForm = std::move(form);
}

void Response::perform()
{
    QNetworkRequest request;

    request.setUrl(QUrl(QString::fromStdString(mRequest.getURL())));

    if (mRequest.getMaxRedirects() > 0) {
        request.setAttribute(
            QNetworkRequest::RedirectPolicyAttribute,
            QNetworkRequest::NoLessSafeRedirectPolicy);
        request.setMaximumRedirectsAllowed(
            static_cast<int>(std::min(mRequest.getMaxRedirects(), size_t(100))));
    } else {
        request.setAttribute(
            QNetworkRequest::RedirectPolicyAttribute,
            QNetworkRequest::ManualRedirectPolicy);
    }

    request.setTransferTimeout(
        static_cast<int>(mRequest.getTimeout().count()));

    for (const auto& header : mRequest.getHeaders()) {
        request.setRawHeader(
            QByteArray::fromStdString(header.Name),
            QByteArray::fromStdString(header.Value));
    }

    const auto& cookies = mRequest.getCookies();
    if (cookies.getCookiesCount() > 0) {
        request.setRawHeader("Cookie",
                             QByteArray::fromStdString(cookies.getCookiesString()));
    }

    QIODevice* payloadDevice = nullptr;
    QBuffer* payloadBuffer = nullptr;

    if (mForm != nullptr && !mForm->IsEmpty()) {
        mMultiPart = createMultiPart();
    } else if (mPayload != nullptr && mPayload->HasData()) {
        const auto payloadSize = mPayload->GetDataSize();
        QByteArray payloadData;

        if (payloadSize > 0) {
            payloadData.resize(static_cast<qsizetype>(payloadSize));
            mPayload->Read(payloadData.data(), payloadSize);
        } else {
            // Unknown size, read until empty
            constexpr size_t chunkSize = 8192;
            char chunk[chunkSize];
            while (size_t bytesRead = mPayload->Read(chunk, chunkSize)) {
                payloadData.append(chunk, static_cast<qsizetype>(bytesRead));
                if (bytesRead < chunkSize) {
                    break;
                }
            }
        }

        payloadBuffer = new QBuffer();
        payloadBuffer->setData(payloadData);
        payloadBuffer->open(QIODevice::ReadOnly);
        payloadDevice = payloadBuffer;
    }

    switch (mVerb) {
    case RequestVerb::Get:
        mReply = mNetworkManager->get(request);
        break;

    case RequestVerb::Head:
        mReply = mNetworkManager->head(request);
        break;

    case RequestVerb::Delete:
        mReply = mNetworkManager->deleteResource(request);
        break;

    case RequestVerb::Post:
        if (mMultiPart) {
            mReply = mNetworkManager->post(request, mMultiPart);
        } else if (payloadDevice) {
            mReply = mNetworkManager->post(request, payloadDevice);
        } else {
            mReply = mNetworkManager->post(request, QByteArray());
        }
        break;

    case RequestVerb::Put:
        if (mMultiPart) {
            mReply = mNetworkManager->put(request, mMultiPart);
        } else if (payloadDevice) {
            mReply = mNetworkManager->put(request, payloadDevice);
        } else {
            mReply = mNetworkManager->put(request, QByteArray());
        }
        break;

    case RequestVerb::Patch:
        if (mMultiPart) {
            mReply = mNetworkManager->sendCustomRequest(request, "PATCH", mMultiPart);
        } else if (payloadDevice) {
            mReply = mNetworkManager->sendCustomRequest(request, "PATCH", payloadDevice);
        } else {
            mReply = mNetworkManager->sendCustomRequest(request, "PATCH", QByteArray());
        }
        break;
    }

    // Set parent so it gets deleted when reply is deleted
    if (payloadBuffer) {
        payloadBuffer->setParent(mReply);
    }

    if (!mReply) {
        std::lock_guard<std::recursive_mutex> lock(mStatusMutex);
        mNetworkError = NetworkError::UnknownError;
        mErrorString = "Failed to create network request";
        mRequestFinished = true;

        std::lock_guard<std::mutex> callbackLock(mCallbackMutex);
        if (mRequestFinishedCallback) {
            mRequestFinishedCallback(this);
        }
        return;
    }

    connect(mReply, &QNetworkReply::readyRead,
            this, &Response::onReadyRead);
    connect(mReply, &QNetworkReply::metaDataChanged,
            this, &Response::onMetaDataChanged);
    connect(mReply, &QNetworkReply::finished,
            this, &Response::onFinished);
    connect(mReply, &QNetworkReply::downloadProgress,
            this, &Response::onDownloadProgress);
    connect(mReply, &QNetworkReply::uploadProgress,
            this, &Response::onUploadProgress);
    connect(mReply, &QNetworkReply::errorOccurred,
            this, [this](QNetworkReply::NetworkError error) {
        onErrorOccurred(static_cast<int>(error));
    });
}

void Response::onReadyRead()
{
    {
        std::lock_guard<std::recursive_mutex> lock(mStatusMutex);

        if (mAbortRequested) {
            return;
        }

        if (!mHeadersReceived) {
            processResponseHeaders();
        }
    }

    if (!mReply) {
        return;
    }

    const QByteArray data = mReply->readAll();

    if (!data.isEmpty()) {
        {
            std::lock_guard<std::mutex> lock(mDataBufferMutex);
            mDataBuffer.insert(mDataBuffer.end(),
                               reinterpret_cast<const uint8_t*>(data.constData()),
                               reinterpret_cast<const uint8_t*>(data.constData() + data.size()));
        }

        std::lock_guard<std::mutex> lock(mCallbackMutex);
        if (mOnDataReceivedCallback) {
            mOnDataReceivedCallback(this);
        }
    }
}

void Response::onMetaDataChanged()
{
    std::lock_guard<std::recursive_mutex> lock(mStatusMutex);

    if (mAbortRequested) {
        return;
    }

    processResponseHeaders();
}

void Response::onFinished()
{
    RequestCallback finishedCallback;
    {
        std::lock_guard<std::recursive_mutex> lock(mStatusMutex);

        if (mReply && mReply->bytesAvailable() > 0) {
            const QByteArray data = mReply->readAll();
            if (!data.isEmpty()) {
                std::lock_guard<std::mutex> dataLock(mDataBufferMutex);
                mDataBuffer.insert(mDataBuffer.end(),
                                   reinterpret_cast<const uint8_t*>(data.constData()),
                                   reinterpret_cast<const uint8_t*>(data.constData() + data.size()));
            }
        }

        if (mReply && mNetworkError == NetworkError::NoError) {
            const auto error = mReply->error();
            if (error != QNetworkReply::NoError) {
                mNetworkError = mapNetworkError(static_cast<int>(error));
                mErrorString = mReply->errorString().toStdString();
            }
        }

        if (mReply && mHttpCode == 0) {
            const QVariant statusCode
                =mReply->attribute(QNetworkRequest::HttpStatusCodeAttribute);
            if (statusCode.isValid()) {
                mHttpCode = statusCode.toUInt();
            }
        }

        if (mHttpCode >= 400 && mNetworkError == NetworkError::NoError) {
            mNetworkError = NetworkError::HTTPError;
        }

        mRequestFinished = true;
    }

    {
        std::lock_guard<std::mutex> lock(mCallbackMutex);

        finishedCallback = std::move(mRequestFinishedCallback);

        mRequestFinishedCallback = {};
        mOnDataReceivedCallback = {};
        mDownloadProgressCallback = {};
        mUploadProgressCallback = {};
    }

    if (finishedCallback) {
        finishedCallback(this);
    }
}

void Response::onDownloadProgress(qint64 bytesReceived, qint64 bytesTotal)
{
    {
        std::lock_guard<std::recursive_mutex> lock(mStatusMutex);
        if (mAbortRequested) {
            return;
        }
    }

    std::lock_guard<std::mutex> callbackLock(mCallbackMutex);

    if (bytesTotal > 0 && mDownloadProgressCallback) {
        mDownloadProgressCallback(bytesReceived, bytesTotal);
    }
}

void Response::onUploadProgress(qint64 bytesSent, qint64 bytesTotal)
{
    {
        std::lock_guard<std::recursive_mutex> lock(mStatusMutex);
        if (mAbortRequested) {
            return;
        }
    }

    std::lock_guard<std::mutex> callbackLock(mCallbackMutex);

    if (bytesTotal > 0 && mUploadProgressCallback) {
        mUploadProgressCallback(bytesSent, bytesTotal);
    }
}

void Response::onErrorOccurred(int errorCode)
{
    std::lock_guard<std::recursive_mutex> lock(mStatusMutex);

    if (mAbortRequested) {
        mNetworkError = NetworkError::OperationCancelled;
        return;
    }

    mNetworkError = mapNetworkError(errorCode);
    if (mReply) {
        mErrorString = mReply->errorString().toStdString();
    }
}

void Response::processResponseHeaders()
{
    if (!mReply || mHeadersReceived) {
        return;
    }

    const QVariant statusCode = mReply->attribute(QNetworkRequest::HttpStatusCodeAttribute);
    if (statusCode.isValid()) {
        mHttpCode = statusCode.toUInt();
    }

    std::lock_guard<std::mutex> headerLock(mHeadersMutex);

    const auto rawHeaders = mReply->rawHeaderPairs();
    for (const auto& header : rawHeaders) {
        const std::string name = header.first.toStdString();
        const std::string value = header.second.toStdString();

        Header h;
        h.Name = "Set-Cookie";
        if (h.hasSameName(name)) {
            mResponseCookies.addCookie(Cookie::Parse(value));
        } else {
            mResponseHeaders.addHeader(name, value);
        }
    }

    mHeadersReceived = true;
}

NetworkError Response::mapNetworkError(int qtError) const
{
    const auto error = static_cast<QNetworkReply::NetworkError>(qtError);

    switch (error) {
    case QNetworkReply::NoError:
        return NetworkError::NoError;

    case QNetworkReply::ConnectionRefusedError:
        return NetworkError::ConnectionRefused;

    case QNetworkReply::RemoteHostClosedError:
        return NetworkError::RemoteHostClosed;

    case QNetworkReply::HostNotFoundError:
        return NetworkError::HostNotFound;

    case QNetworkReply::TimeoutError:
        return NetworkError::Timeout;

    case QNetworkReply::OperationCanceledError:
        return NetworkError::OperationCancelled;

    case QNetworkReply::SslHandshakeFailedError:
        return NetworkError::SSLHandshakeFailed;

    case QNetworkReply::TooManyRedirectsError:
        return NetworkError::TooManyRedirects;

    case QNetworkReply::ProxyConnectionRefusedError:
    case QNetworkReply::ProxyConnectionClosedError:
    case QNetworkReply::ProxyTimeoutError:
    case QNetworkReply::ProxyAuthenticationRequiredError:
        return NetworkError::ProxyConnectionFailed;

    case QNetworkReply::ProxyNotFoundError:
        return NetworkError::ProxyNotFound;

    case QNetworkReply::ContentAccessDenied:
    case QNetworkReply::ContentOperationNotPermittedError:
    case QNetworkReply::ContentNotFoundError:
    case QNetworkReply::AuthenticationRequiredError:
    case QNetworkReply::ContentReSendError:
    case QNetworkReply::ContentConflictError:
    case QNetworkReply::ContentGoneError:
        return NetworkError::HTTPError;

    case QNetworkReply::ProtocolUnknownError:
    case QNetworkReply::ProtocolInvalidOperationError:
    case QNetworkReply::ProtocolFailure:
        return NetworkError::BadURL;

    case QNetworkReply::TemporaryNetworkFailureError:
    case QNetworkReply::NetworkSessionFailedError:
    case QNetworkReply::BackgroundRequestNotAllowedError:
        return NetworkError::ConnectionFailed;

    default:
        return NetworkError::UnknownError;
    }
}

QHttpMultiPart* Response::createMultiPart() const
{
    if (!mForm || mForm->IsEmpty()) {
        return nullptr;
    }

    auto* multiPart = new QHttpMultiPart(QHttpMultiPart::FormDataType);

    for (size_t i = 0; i < mForm->GetPartsCount(); ++i) {
        auto* part = mForm->GetPart(i);
        if (!part) {
            continue;
        }

        QHttpPart httpPart;

        const auto& headers = part->GetHeaders();
        for (const auto& header : headers) {
            httpPart.setRawHeader(
                QByteArray::fromStdString(header.Name),
                QByteArray::fromStdString(header.Value));
        }

        const auto partSize = part->GetSize();
        QByteArray partData;

        if (partSize > 0) {
            partData.resize(static_cast<qsizetype>(partSize));
            part->Seek(0, SEEK_SET);
            part->Read(partData.data(), partSize);
        }

        httpPart.setBody(partData);
        multiPart->append(httpPart);
    }

    return multiPart;
}
}
}
