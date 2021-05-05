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

namespace audacity
{
namespace network_manager
{

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

class NETWORK_MANAGER_API IResponse
{
public:
    using RequestCallback = std::function<void (IResponse*)>;

    virtual ~IResponse () = default;

    virtual bool isFinished () const noexcept = 0;

    virtual unsigned getHTTPCode () const noexcept = 0;

    virtual NetworkError getError () const noexcept = 0;
    virtual std::string getErrorString () const = 0;

    virtual bool headersReceived () const noexcept = 0;

    virtual bool hasHeader (const std::string& headerName) const noexcept = 0;
    virtual std::string getHeader (const std::string& headerName) const = 0;

    virtual const HeadersList& getHeaders () const noexcept = 0;
    virtual const CookiesList& getCookies () const noexcept = 0;

    virtual const Request& getRequest () const noexcept = 0;
    virtual std::string getURL () const = 0;

    virtual void abort () noexcept = 0;

    virtual void setOnDataReceivedCallback (RequestCallback callback) = 0;
    virtual void setRequestFinishedCallback (RequestCallback callback) = 0;

    // The total bytes available to read by readData
    virtual uint64_t getBytesAvailable () const noexcept = 0;

    // Reads at max maxBytesCount into the buffer, returns the actual count of bytes read
    virtual uint64_t readData (void* buffer, uint64_t maxBytesCount) = 0;

    template<typename RetVal = std::vector<uint8_t>>
    RetVal readAll ()
    {
        RetVal result;

        constexpr uint64_t bufferSize = 4 * 1024;
        uint8_t buffer[bufferSize];

        while (uint64_t bytesRead = readData (buffer, bufferSize))
        {
            using PtrType = typename RetVal::pointer;

            PtrType begin = reinterpret_cast<PtrType>(buffer);
            PtrType end = reinterpret_cast<PtrType>(buffer + bytesRead);

            result.insert (result.end (), begin, end);

            if (bytesRead < bufferSize)
                break;
        }

        return result;
    }
};

}
}
