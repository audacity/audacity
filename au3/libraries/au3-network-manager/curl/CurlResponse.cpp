/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file CurlResponse.cpp
 @brief Define an implementation of IResponse using libcurl.

 Dmitry Vedenko
 **********************************************************************/

#include "CurlResponse.h"

#include <cassert>
#include <map>
#include <algorithm>

#include "MultipartData.h"
#include "RequestPayload.h"

#include "MemoryX.h"

namespace audacity {
namespace network_manager {
namespace {
static const std::map<CURLcode, NetworkError> errorsMap = {
    { CURLE_OK, NetworkError::NoError },
    { CURLE_URL_MALFORMAT, NetworkError::BadURL },
    { CURLE_COULDNT_RESOLVE_PROXY, NetworkError::ProxyNotFound },
    { CURLE_COULDNT_RESOLVE_HOST,  NetworkError::HostNotFound },
    { CURLE_COULDNT_CONNECT, NetworkError::ConnectionRefused },
    { CURLE_HTTP_RETURNED_ERROR, NetworkError::HTTPError },
    { CURLE_WRITE_ERROR, NetworkError::OperationCancelled },
    { CURLE_READ_ERROR, NetworkError::OperationCancelled },
    { CURLE_OPERATION_TIMEDOUT, NetworkError::Timeout },
    { CURLE_RANGE_ERROR, NetworkError::HTTPError },
    { CURLE_HTTP_POST_ERROR, NetworkError::HTTPError },
    { CURLE_SSL_CONNECT_ERROR, NetworkError::SSLHandshakeFailed },
    { CURLE_ABORTED_BY_CALLBACK, NetworkError::OperationCancelled },
    { CURLE_TOO_MANY_REDIRECTS, NetworkError::OperationCancelled },
    { CURLE_PEER_FAILED_VERIFICATION, NetworkError::SSLHandshakeFailed },
    { CURLE_GOT_NOTHING, NetworkError::RemoteHostClosed },
    { CURLE_SSL_ENGINE_NOTFOUND, NetworkError::SSLHandshakeFailed },
    { CURLE_SSL_ENGINE_SETFAILED, NetworkError::SSLHandshakeFailed },
    { CURLE_SEND_ERROR, NetworkError::RemoteHostClosed },
    { CURLE_RECV_ERROR, NetworkError::RemoteHostClosed },
    { CURLE_SSL_CERTPROBLEM, NetworkError::SSLHandshakeFailed },
    { CURLE_SSL_CIPHER, NetworkError::SSLHandshakeFailed },
    { CURLE_SSL_CACERT, NetworkError::SSLHandshakeFailed },
    { CURLE_USE_SSL_FAILED, NetworkError::SSLHandshakeFailed },
    { CURLE_SSL_ENGINE_INITFAILED, NetworkError::SSLHandshakeFailed },
    { CURLE_SSL_CACERT_BADFILE, NetworkError::SSLHandshakeFailed },
    { CURLE_SSL_SHUTDOWN_FAILED, NetworkError::SSLHandshakeFailed },
    { CURLE_SSL_CRL_BADFILE, NetworkError::SSLHandshakeFailed },
    { CURLE_SSL_ISSUER_ERROR, NetworkError::SSLHandshakeFailed },
    { CURLE_CHUNK_FAILED, NetworkError::HTTPError },
    { CURLE_NO_CONNECTION_AVAILABLE, NetworkError::ConnectionFailed },
    { CURLE_SSL_PINNEDPUBKEYNOTMATCH, NetworkError::SSLHandshakeFailed },
    { CURLE_SSL_INVALIDCERTSTATUS, NetworkError::SSLHandshakeFailed },
    { CURLE_PARTIAL_FILE, NetworkError::RemoteHostClosed }
};

size_t DataStreamRead(char* ptr, size_t size, size_t nmemb, RequestPayloadStream* stream)
{
    return stream->Read(ptr, size * nmemb);
}

int DataStreamSeek(RequestPayloadStream* stream, curl_off_t offs, int origin)
{
    const auto direction
        =origin == SEEK_SET ? RequestPayloadStream::SeekDirection::Start
          : origin == SEEK_CUR ? RequestPayloadStream::SeekDirection::Current
          : RequestPayloadStream::SeekDirection::End;

    return stream->Seek(offs, direction) ? CURL_SEEKFUNC_OK
           : CURL_SEEKFUNC_FAIL;
}

size_t MimePartRead(char* ptr, size_t size, size_t nmemb, MultipartData::Part* stream)
{
    return stream->Read(ptr, size * nmemb);
}

int MimePartSeek(MultipartData::Part* stream, curl_off_t offs, int origin) noexcept
{
    return stream->Seek(offs, origin) ? CURL_SEEKFUNC_OK : CURL_SEEKFUNC_FAIL;
}
}

CurlResponse::CurlResponse (RequestVerb verb, const Request& request, CurlHandleManager* handleManager) noexcept
    : mVerb(verb),
    mRequest(request),
    mHandleManager(handleManager)
{
}

bool CurlResponse::isFinished() const noexcept
{
    std::lock_guard<std::recursive_mutex> lock(mStatusMutex);
    return mRequestFinished;
}

unsigned CurlResponse::getHTTPCode() const noexcept
{
    std::lock_guard<std::recursive_mutex> lock(mStatusMutex);
    return mHttpCode;
}

NetworkError CurlResponse::getError() const noexcept
{
    std::lock_guard<std::recursive_mutex> lock(mStatusMutex);
    return mNetworkError;
}

std::string CurlResponse::getErrorString() const
{
    std::lock_guard<std::recursive_mutex> lock(mStatusMutex);
    return mErrorString;
}

bool CurlResponse::headersReceived() const noexcept
{
    std::lock_guard<std::recursive_mutex> lock(mStatusMutex);
    return mHeadersReceived;
}

bool CurlResponse::hasHeader(const std::string& headerName) const noexcept
{
    std::lock_guard<std::mutex> lock(mHeadersMutex);
    return mResponseHeaders.hasHeader(headerName);
}

std::string CurlResponse::getHeader(const std::string& headerName) const
{
    std::lock_guard<std::mutex> lock(mHeadersMutex);
    return mResponseHeaders.getHeaderValue(headerName);
}

const HeadersList& CurlResponse::getHeaders() const noexcept
{
    std::lock_guard<std::mutex> lock(mHeadersMutex);
    return mResponseHeaders;
}

const CookiesList& CurlResponse::getCookies() const noexcept
{
    std::lock_guard<std::mutex> lock(mHeadersMutex);
    return mResponseCookies;
}

const Request& CurlResponse::getRequest() const noexcept
{
    return mRequest;
}

std::string CurlResponse::getURL() const
{
    return mRequest.getURL();
}

void CurlResponse::abort() noexcept
{
    std::lock_guard<std::recursive_mutex> lock(mStatusMutex);
    mAbortRequested = true;
}

void CurlResponse::setOnDataReceivedCallback(RequestCallback callback)
{
    std::lock_guard<std::mutex> lock(mCallbackMutex);

    mOnDataReceivedCallback = std::move(callback);

    if (mOnDataReceivedCallback && getBytesAvailable() > 0) {
        mOnDataReceivedCallback(this);
    }
}

void CurlResponse::setRequestFinishedCallback(RequestCallback callback)
{
    std::lock_guard<std::mutex> callbackLock(mCallbackMutex);

    mRequestFinishedCallback = std::move(callback);

    std::lock_guard<std::recursive_mutex> statusLock(mStatusMutex);

    if (mRequestFinishedCallback && mRequestFinished) {
        mRequestFinishedCallback(this);
    }
}

void CurlResponse::setDownloadProgressCallback(ProgressCallback callback)
{
    std::lock_guard<std::mutex> callbackLock(mCallbackMutex);
    mDownloadProgressCallback = std::move(callback);
}

void CurlResponse::setUploadProgressCallback(ProgressCallback callback)
{
    std::lock_guard<std::mutex> callbackLock(mCallbackMutex);
    mUploadProgressCallback = std::move(callback);
}

uint64_t CurlResponse::getBytesAvailable() const noexcept
{
    std::lock_guard<std::mutex> lock(mDataBufferMutex);
    return mDataBuffer.size();
}

uint64_t CurlResponse::readData(void* buffer, uint64_t maxBytesCount)
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

void CurlResponse::setPayload(RequestPayloadStreamPtr payload)
{
    mPayload = std::move(payload);
}

void CurlResponse::setForm(std::unique_ptr<MultipartData> form)
{
    mForm = std::move(form);
}

void CurlResponse::perform()
{
    CurlHandleManager::Handle handle = mHandleManager->getHandle(mVerb, mRequest.getURL());

    handle.setOption(CURLOPT_WRITEFUNCTION, WriteCallback);
    handle.setOption(CURLOPT_WRITEDATA, this);

    handle.setOption(CURLOPT_HEADERFUNCTION, HeaderCallback);
    handle.setOption(CURLOPT_HEADERDATA, this);

    handle.setOption(CURLOPT_XFERINFOFUNCTION, CurlProgressCallback);
    handle.setOption(CURLOPT_XFERINFODATA, this);

    handle.setOption(CURLOPT_FOLLOWLOCATION, mRequest.getMaxRedirects() == 0 ? 0 : 1);
    handle.setOption(CURLOPT_MAXREDIRS, mRequest.getMaxRedirects());

    handle.setOption(CURLOPT_NOPROGRESS, 0L);

    handle.setOption(CURLOPT_CONNECTTIMEOUT_MS,
                     std::chrono::duration_cast<std::chrono::milliseconds>(mRequest.getTimeout()).count()
                     );

    handle.appendCookies(mRequest.getCookies());

    curl_mime* mimeList = nullptr;

    if (mForm != nullptr) {
        mimeList = curl_mime_init(handle.getCurlHandle());

        for (size_t i = 0; i < mForm->GetPartsCount(); ++i) {
            auto part = mForm->GetPart(i);

            curl_mimepart* curlPart = curl_mime_addpart(mimeList);

            const auto& headers = part->GetHeaders();

            if (headers.getHeadersCount() > 0) {
                curl_slist* partHeaders = nullptr;

                for (auto header : headers) {
                    partHeaders = curl_slist_append(
                        partHeaders, (header.Name + ": " + header.Value).c_str());
                }

                curl_mime_headers(curlPart, partHeaders, 1);
            }

            curl_mime_data_cb(
                curlPart, part->GetSize(), curl_read_callback(MimePartRead),
                curl_seek_callback(MimePartSeek), nullptr, part);
        }

        curl_easy_setopt(handle.getCurlHandle(), CURLOPT_MIMEPOST, mimeList);
    } else if (mPayload != nullptr && mPayload->HasData()) {
        if (const auto payloadSize = mPayload->GetDataSize(); payloadSize > 0) {
            handle.appendHeader({ "Transfer-Encoding", std::string() });
            handle.appendHeader(
                { "Content-Length", std::to_string(payloadSize) });

            if (mVerb == RequestVerb::Post) {
                handle.setOption(CURLOPT_POSTFIELDSIZE_LARGE, payloadSize);
            } else {
                handle.setOption(CURLOPT_INFILESIZE_LARGE, payloadSize);
            }
        }

        handle.setOption(CURLOPT_READFUNCTION, DataStreamRead);
        handle.setOption(CURLOPT_READDATA, mPayload.get());

        handle.setOption(CURLOPT_SEEKFUNCTION, DataStreamSeek);
        handle.setOption(CURLOPT_SEEKDATA, mPayload.get());
    } else if (mVerb == RequestVerb::Post || mVerb == RequestVerb::Put || mVerb == RequestVerb::Patch) {
        handle.setOption(CURLOPT_POSTFIELDS, "");
        handle.setOption(CURLOPT_POSTFIELDSIZE, 0);
    }

    auto cleanupMime = finally(
        [mimeList]() {
        if (mimeList != nullptr) {
            curl_mime_free(mimeList);
        }
    });

    handle.appendHeaders(mRequest.getHeaders());

    mCurrentHandle = &handle;
    const auto result = handle.perform();
    mCurrentHandle = nullptr;

    {
        std::lock_guard<std::recursive_mutex> lock(mStatusMutex);

        if (result.Code != CURLE_OK) {
            const auto it = errorsMap.find(result.Code);

            mNetworkError = it != errorsMap.end() ? it->second : NetworkError::UnknownError;
            mErrorString = result.Message;
        } else {
            if (mHttpCode == 0) {
                mHttpCode = handle.getHTTPCode();
            }
            if (mHttpCode >= 400) {
                mNetworkError = NetworkError::HTTPError;
            }
        }

        mRequestFinished = true;
    }

    std::lock_guard<std::mutex> lock(mCallbackMutex);

    if (mRequestFinishedCallback) {
        mRequestFinishedCallback(this);
    }

    mRequestFinishedCallback = {};
    mOnDataReceivedCallback = {};
    mDownloadProgressCallback = {};
    mUploadProgressCallback = {};
}

size_t CurlResponse::WriteCallback(const uint8_t* ptr, size_t size, size_t nmemb, CurlResponse* request) noexcept
{
    {
        std::lock_guard<std::recursive_mutex> lock(request->mStatusMutex);

        if (request->mAbortRequested) {
            return 0;
        }

        if (!request->mHeadersReceived) {
            request->mHeadersReceived = true;

            // WriteCallback is called by the handle
            assert(request->mCurrentHandle != nullptr);

            if (request->mCurrentHandle != nullptr) {
                request->mHttpCode = request->mCurrentHandle->getHTTPCode();
            }
        }
    }

    size *= nmemb;

    {
        std::lock_guard<std::mutex> lock(request->mDataBufferMutex);
        request->mDataBuffer.insert(request->mDataBuffer.end(), ptr, ptr + size);
    }

    std::lock_guard<std::mutex> lock(request->mCallbackMutex);

    if (request->mOnDataReceivedCallback) {
        request->mOnDataReceivedCallback(request);
    }

    return size;
}

size_t CurlResponse::HeaderCallback(const char* buffer, size_t size, size_t nitems, CurlResponse* request) noexcept
{
    {
        std::lock_guard<std::recursive_mutex> lock(request->mStatusMutex);

        if (request->mAbortRequested) {
            return 0;
        }

        // HeaderCallback is called by the handle
        assert(request->mCurrentHandle != nullptr);

        if (request->mCurrentHandle != nullptr) {
            request->mHttpCode = request->mCurrentHandle->getHTTPCode();
        }
    }

    size = size * nitems;

    if (size < 2) {
        return 0;
    }

    const Header header = Header::Parse(std::string(buffer, size - 2));

    std::lock_guard<std::mutex> lock(request->mHeadersMutex);

    if (header.hasSameName("Set-Cookie")) {
        request->mResponseCookies.addCookie(Cookie::Parse(header.Value));
    } else {
        if (header.hasSameName("Keep-Alive")) {
            request->mCurrentHandle->markKeepAlive();
        }

        request->mResponseHeaders.addHeader(header);
    }

    return size;
}

int CurlResponse::CurlProgressCallback(
    CurlResponse* clientp, curl_off_t dltotal, curl_off_t dlnow,
    curl_off_t ultotal, curl_off_t ulnow) noexcept
{
    {
        std::lock_guard<std::recursive_mutex> lock(clientp->mStatusMutex);

        if (clientp->mAbortRequested) {
            return -1;
        }
    }

    std::lock_guard<std::mutex> callbackLock(clientp->mCallbackMutex);

    if (dltotal > 0 && clientp->mDownloadProgressCallback) {
        clientp->mDownloadProgressCallback(dlnow, dltotal);
    }

    if (ultotal > 0 && clientp->mUploadProgressCallback) {
        clientp->mUploadProgressCallback(ulnow, ultotal);
    }

    return CURLE_OK;
}
}
}
