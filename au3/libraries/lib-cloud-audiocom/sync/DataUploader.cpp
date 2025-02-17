/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DataUploader.cpp

  Dmitry Vedenko

**********************************************************************/

#include "DataUploader.h"

#include <variant>

#include <wx/file.h>

#include "CodeConversions.h"

#include "IResponse.h"
#include "NetworkManager.h"
#include "Request.h"

#include "RequestPayload.h"

#include "BasicUI.h"

using namespace audacity::network_manager;

namespace audacity::cloud::audiocom::sync {
constexpr int RetriesCount { 3 };

using UploadData = std::variant<std::vector<uint8_t>, std::string>;

struct DataUploader::UploadOperation final : std::enable_shared_from_this<DataUploader::UploadOperation>
{
    DataUploader& Uploader;
    UploadUrls Target;
    std::function<void(ResponseResult)> Callback;
    std::function<void(double)> ProgressCallback;

    std::string MimeType;
    UploadData Data;

    ResponseResult CurrentResult;
    CancellationContextPtr CancelContext;

    std::atomic<bool> UploadFailed { false };

    UploadOperation(
        DataUploader& uploader, CancellationContextPtr cancellationContex,
        const UploadUrls& target, UploadData data, std::string mimeType,
        std::function<void(ResponseResult)> callback,
        std::function<void(double)> progressCallback)
        : Uploader{uploader}
        , Target{target}
        , Callback{std::move(callback)}
        , ProgressCallback{std::move(progressCallback)}
        , MimeType{std::move(mimeType)}
        , Data{std::move(data)}
        , CancelContext{std::move(cancellationContex)}
    {
    }

    void PerformUpload(int retriesLeft)
    {
        Request request { Target.UploadUrl };
        request.setHeader(common_headers::ContentType, MimeType);

        ResponsePtr networkResponse;

        if (std::holds_alternative<std::vector<uint8_t> >(Data)) {
            auto data = *std::get_if<std::vector<uint8_t> >(&Data);

            networkResponse = NetworkManager::GetInstance().doPut(
                request, data.data(), data.size());
        } else {
            auto filePath = *std::get_if<std::string>(&Data);

            networkResponse = NetworkManager::GetInstance().doPut(
                request, CreateRequestPayloadStream(filePath));
        }

        CancelContext->OnCancelled(networkResponse);

        networkResponse->setRequestFinishedCallback(
            [this, retriesLeft, networkResponse, operation = weak_from_this()](auto)
        {
            auto strongThis = operation.lock();
            if (!strongThis) {
                return;
            }

            CurrentResult = GetResponseResult(*networkResponse, false);

            if (CurrentResult.Code == SyncResultCode::Success) {
                ConfirmUpload(RetriesCount);
            } else if (
                CurrentResult.Code == SyncResultCode::ConnectionFailed
                && retriesLeft > 0) {
                PerformUpload(retriesLeft - 1);
            } else {
                FailUpload(RetriesCount);
            }
        });

        networkResponse->setUploadProgressCallback(
            [this, operation = weak_from_this()](
                int64_t current, int64_t total)
        {
            auto strongThis = operation.lock();
            if (!strongThis) {
                return;
            }

            if (total <= 0) {
                total   = 1;
                current = 0;
            }

            ProgressCallback(static_cast<double>(current) / total);
        });
    }

    void ConfirmUpload(int retriesLeft)
    {
        Data = {};
        Request request { Target.SuccessUrl };

        auto networkResponse
            =NetworkManager::GetInstance().doPost(request, nullptr, 0);
        CancelContext->OnCancelled(networkResponse);

        networkResponse->setRequestFinishedCallback(
            [this, retriesLeft, networkResponse, operation = weak_from_this()](auto)
        {
            auto strongThis = operation.lock();
            if (!strongThis) {
                return;
            }

            CurrentResult = GetResponseResult(*networkResponse, false);

            if (CurrentResult.Code == SyncResultCode::Success) {
                Callback(ResponseResult { SyncResultCode::Success, {} });
                CleanUp();
            } else if (
                CurrentResult.Code == SyncResultCode::ConnectionFailed
                && retriesLeft > 0) {
                ConfirmUpload(retriesLeft - 1);
            } else {
                FailUpload(RetriesCount);
            }
        });
    }

    void FailUpload(int retriesLeft)
    {
        if (!UploadFailed.exchange(true)) {
            Data = {};
            Callback(CurrentResult);
        }

        Request request { Target.FailUrl };

        auto networkResponse
            =NetworkManager::GetInstance().doPost(request, nullptr, 0);
        CancelContext->OnCancelled(networkResponse);

        networkResponse->setRequestFinishedCallback(
            [this, retriesLeft, networkResponse, operation = weak_from_this()](auto)
        {
            auto strongThis = operation.lock();
            if (!strongThis) {
                return;
            }

            const auto result = GetResponseResult(*networkResponse, false);

            if (
                result.Code == SyncResultCode::ConnectionFailed
                && retriesLeft > 0) {
                FailUpload(retriesLeft - 1);
            } else {
                CleanUp();
            }

            // Ignore other errors, server will collect garbage
            // and delete the file eventually
        });
    }

    void CleanUp()
    {
        BasicUI::CallAfter([this]() { Uploader.RemoveResponse(*this); });
    }
};

DataUploader::~DataUploader()
{
}

DataUploader& DataUploader::Get()
{
    static DataUploader instance;
    return instance;
}

void DataUploader::Upload(
    CancellationContextPtr cancellationContex, const ServiceConfig&,
    const UploadUrls& target, std::vector<uint8_t> data,
    std::function<void(ResponseResult)> callback,
    std::function<void(double)> progressCallback)
{
    if (!callback) {
        callback = [](auto...) {} }

    if (!progressCallback) {
        progressCallback = [](auto...) { return true; } }

    if (!cancellationContex) {
        cancellationContex = concurrency::CancellationContext::Create();
    }

    auto lock = std::lock_guard { mResponseMutex };

    mResponses.emplace_back(std::make_unique<UploadOperation>(
                                *this, cancellationContex, target, std::move(data),
                                audacity::network_manager::common_content_types::ApplicationXOctetStream,
                                std::move(callback), std::move(progressCallback)));

    mResponses.back()->PerformUpload(RetriesCount);
}

void DataUploader::Upload(
    CancellationContextPtr cancellationContex, const ServiceConfig& config,
    const UploadUrls& target, std::string filePath,
    std::function<void(ResponseResult)> callback,
    std::function<void(double)> progressCallback)
{
    if (!callback) {
        callback = [](auto...) {} }

    if (!progressCallback) {
        progressCallback = [](auto...) { return true; } }

    if (!cancellationContex) {
        cancellationContex = concurrency::CancellationContext::Create();
    }

    if (!wxFileExists(audacity::ToWXString(filePath))) {
        if (callback) {
            callback(ResponseResult {
                    SyncResultCode::UnknownError,
                    audacity::ToUTF8(XO("File not found").Translation()) });
        }

        return;
    }

    auto lock = std::lock_guard { mResponseMutex };

    mResponses.emplace_back(std::make_shared<UploadOperation>(
                                *this, cancellationContex, target, std::move(filePath),
                                audacity::network_manager::common_content_types::ApplicationXOctetStream,
                                std::move(callback), std::move(progressCallback)));

    mResponses.back()->PerformUpload(RetriesCount);
}

void DataUploader::RemoveResponse(UploadOperation& response)
{
    auto lock = std::lock_guard { mResponseMutex };

    mResponses.erase(
        std::remove_if(
            mResponses.begin(), mResponses.end(),
            [&response](const auto& item) { return item.get() == &response; }),
        mResponses.end());
}
} // namespace audacity::cloud::audiocom::sync
