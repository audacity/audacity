/*!********************************************************************
 Audacity: A Digital Audio Editor
 **********************************************************************/

#pragma once

#include <deque>
#include <mutex>
#include <cstdint>

#include <QObject>
#include <QPointer>

#include "../IResponse.h"
#include "../IResponseFactory.h"

#include "../HeadersList.h"
#include "../CookiesList.h"
#include "../Request.h"

class QNetworkReply;
class QNetworkAccessManager;
class QHttpMultiPart;

namespace audacity {
namespace network_manager {
class MultipartData;

class RequestPayloadStream;
using RequestPayloadStreamPtr = std::shared_ptr<RequestPayloadStream>;

class Response final : public QObject, public IResponse
{
    Q_OBJECT
public:
    Response(
        RequestVerb verb, const Request& request, QNetworkAccessManager* networkManager) noexcept;
    ~Response() override;

    bool isFinished() const noexcept override;
    unsigned getHTTPCode() const noexcept override;

    NetworkError getError() const noexcept override;
    std::string getErrorString() const override;

    bool headersReceived() const noexcept override;

    bool hasHeader(const std::string& headerName) const noexcept override;
    std::string getHeader(const std::string& headerName) const override;

    const HeadersList& getHeaders() const noexcept override;
    const CookiesList& getCookies() const noexcept override;

    const Request& getRequest() const noexcept override;

    std::string getURL() const override;

    void abort() noexcept override;

    void setOnDataReceivedCallback(RequestCallback callback) override;

    void setRequestFinishedCallback(RequestCallback callback) override;

    void setDownloadProgressCallback(ProgressCallback callback) override;
    void setUploadProgressCallback(ProgressCallback callback) override;

    uint64_t getBytesAvailable() const noexcept override;
    uint64_t readData(void* buffer, uint64_t maxBytesCount) override;

    void setPayload(RequestPayloadStreamPtr payload);
    void setForm(std::unique_ptr<MultipartData> form);

    //! Start the network request (must be called from the Qt event loop thread)
    void perform();

private slots:
    void onReadyRead();
    void onMetaDataChanged();
    void onFinished();
    void onDownloadProgress(qint64 bytesReceived, qint64 bytesTotal);
    void onUploadProgress(qint64 bytesSent, qint64 bytesTotal);
    void onErrorOccurred(int errorCode);

private:
    void processResponseHeaders();
    NetworkError mapNetworkError(int qtError) const;
    QHttpMultiPart* createMultiPart() const;

    RequestVerb mVerb;
    Request mRequest;

    QNetworkAccessManager* mNetworkManager;
    QPointer<QNetworkReply> mReply;

    mutable std::mutex mCallbackMutex;
    RequestCallback mOnDataReceivedCallback;
    RequestCallback mRequestFinishedCallback;
    ProgressCallback mDownloadProgressCallback;
    ProgressCallback mUploadProgressCallback;

    mutable std::mutex mHeadersMutex;
    HeadersList mResponseHeaders;
    CookiesList mResponseCookies;

    mutable std::mutex mDataBufferMutex;
    std::deque<uint8_t> mDataBuffer;

    mutable std::recursive_mutex mStatusMutex;

    NetworkError mNetworkError { NetworkError::NoError };
    std::string mErrorString;

    unsigned mHttpCode { 0 };

    RequestPayloadStreamPtr mPayload;
    std::unique_ptr<MultipartData> mForm;

    QHttpMultiPart* mMultiPart { nullptr };

    bool mHeadersReceived { false };
    bool mRequestFinished { false };
    bool mAbortRequested { false };
};
}
}
