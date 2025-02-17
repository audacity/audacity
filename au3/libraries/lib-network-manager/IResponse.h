/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file IResponse.h
 @brief Declare an interface for HTTP response.

 Dmitry Vedenko
 **********************************************************************/

#pragma once

#include <string>
#include <cstdint>
#include <vector>
#include <functional>

#include "NetworkManagerApi.h"

#include "concurrency/ICancellable.h"

namespace audacity {
namespace network_manager {
class Request;
class HeadersList;
class CookiesList;

enum class NetworkError
{
    NoError,
    BadURL,
    ConnectionFailed,
    ConnectionRefused,
    RemoteHostClosed,
    HostNotFound,
    Timeout,
    OperationCancelled,
    SSLHandshakeFailed,
    TooManyRedirects,
    ProxyConnectionFailed,
    ProxyNotFound,
    UnknownError,
    HTTPError
};

namespace HttpCode {
enum
{
    Continue = 100,
    SwitchingProtocols = 101,
    Processing = 102,
    EarlyHints = 103,

    OK = 200,
    Created = 201,
    Accepted = 202,
    NonAuthoritativeInformation = 203,
    NoContent = 204,
    ResetContent = 205,
    PartialContent = 206,

    MultipleChoices = 300,
    MovedPermanently = 301,
    Found = 302,
    SeeOther = 303,
    NotModified = 304,
    TemporaryRedirect = 307,
    PermanentRedirect = 308,

    BadRequest = 400,
    Unauthorized = 401,
    PaymentRequired = 402,
    Forbidden = 403,
    NotFound = 404,
    MethodNotAllowed = 405,
    NotAcceptable = 406,
    RequestTimeout = 408,
    Conflict = 409,
    Gone = 410,
    LengthRequired = 411,
    PayloadTooLarge = 413,
    URITooLong = 414,
    UnsupportedMediaType = 415,
    RangeNotSatisfiable = 416,
    ExpectationFailed = 417,
    MisdirectedRequest = 421,
    UnprocessableEntity = 422,
    Locked = 423,
    FailedDependency = 424,
    TooEarly = 425,
    UpgradeRequired = 426,
    PreconditionRequired = 428,
    TooManyRequests = 429,
    RequestHeaderFieldsTooLarge = 431,

    InternalServerError = 500,
    NotImplemented = 501,
    BadGateway = 502,
    ServiceUnavailable = 503,
    GatewayTimeout = 504,
    HTTPVersionNotSupported = 505,
    NetworkAuthenticationRequired = 511
};
} // namespace HttpCode

//! Interface, that provides access to the data from the HTTP response
class NETWORK_MANAGER_API IResponse : public concurrency::ICancellable
{
public:
    using RequestCallback = std::function<void (IResponse*)>;
    //! Called when download or upload progress changes. Expected can be zero when
    //! transfer encoding does not allow to calculate the size
    using ProgressCallback = std::function<void (int64_t current, int64_t expected)>;

    virtual ~IResponse () = default;

    virtual bool isFinished() const noexcept = 0;

    virtual unsigned getHTTPCode() const noexcept = 0;

    virtual NetworkError getError() const noexcept = 0;
    virtual std::string getErrorString() const = 0;

    virtual bool headersReceived() const noexcept = 0;

    virtual bool hasHeader(const std::string& headerName) const noexcept = 0;
    virtual std::string getHeader(const std::string& headerName) const = 0;

    virtual const HeadersList& getHeaders() const noexcept = 0;
    virtual const CookiesList& getCookies() const noexcept = 0;

    virtual const Request& getRequest() const noexcept = 0;
    virtual std::string getURL() const = 0;

    virtual void abort() noexcept = 0;

    virtual void setOnDataReceivedCallback(RequestCallback callback) = 0;
    virtual void setRequestFinishedCallback(RequestCallback callback) = 0;

    //! Set the download progress callback
    virtual void setDownloadProgressCallback(ProgressCallback callback) = 0;
    //! Set the upload progress callback
    virtual void setUploadProgressCallback(ProgressCallback callback) = 0;

    // The total bytes available to read by readData
    virtual uint64_t getBytesAvailable() const noexcept = 0;

    // Reads at max maxBytesCount into the buffer, returns the actual count of bytes read
    virtual uint64_t readData(void* buffer, uint64_t maxBytesCount) = 0;

    template<typename RetVal = std::vector<uint8_t> >
    RetVal readAll()
    {
        RetVal result;

        constexpr uint64_t bufferSize = 4 * 1024;
        uint8_t buffer[bufferSize];

        while (uint64_t bytesRead = readData(buffer, bufferSize))
        {
            using PtrType = typename RetVal::pointer;

            PtrType begin = reinterpret_cast<PtrType>(buffer);
            PtrType end = reinterpret_cast<PtrType>(buffer + bytesRead);

            result.insert(result.end(), begin, end);

            if (bytesRead < bufferSize) {
                break;
            }
        }

        return result;
    }

    void Cancel() override
    {
        abort();
    }
};
}
}
