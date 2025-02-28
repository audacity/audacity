/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  UploadService.cpp

  Dmitry Vedenko

**********************************************************************/

#include "UploadService.h"

#include <cassert>
#include <mutex>

#include <wx/filefn.h>
#include <wx/filename.h>

#include <rapidjson/document.h>
#include <rapidjson/writer.h>

#include "AudacityException.h"

#include "OAuthService.h"
#include "ServiceConfig.h"

#include "IResponse.h"
#include "MultipartData.h"
#include "NetworkManager.h"
#include "NetworkUtils.h"
#include "Request.h"

#include "CodeConversions.h"

#include "TempDirectory.h"
#include "FileNames.h"

namespace audacity::cloud::audiocom {
namespace {
std::string_view DeduceMimeType(const wxString& ext)
{
    if (ext == "wv") {
        return "audio/x-wavpack";
    } else if (ext == "flac") {
        return "audio/x-flac";
    } else if (ext == "mp3") {
        return "audio/mpeg";
    } else {
        return "audio/x-wav";
    }
}

std::string GetUploadRequestPayload(
    const wxString& filePath, const wxString& projectName, bool isPublic)
{
    rapidjson::Document document;
    document.SetObject();

    const wxFileName fileName(filePath);
    const auto mimeType = DeduceMimeType(fileName.GetExt());

    document.AddMember(
        "mime",
        rapidjson::Value(
            mimeType.data(), mimeType.length(), document.GetAllocator()),
        document.GetAllocator());

    const auto downloadMime = GetServiceConfig().GetDownloadMime();

    if (!downloadMime.empty()) {
        document.AddMember(
            "download_mime",
            rapidjson::Value(
                downloadMime.data(), downloadMime.length(),
                document.GetAllocator()),
            document.GetAllocator());
    }

    const auto name = audacity::ToUTF8(projectName.empty() ? fileName.GetFullName() : projectName);

    document.AddMember(
        "name",
        rapidjson::Value(name.data(), name.length(), document.GetAllocator()),
        document.GetAllocator());

    document.AddMember(
        "size",
        rapidjson::Value(static_cast<int64_t>(fileName.GetSize().GetValue())),
        document.GetAllocator());

    document.AddMember(
        "public", rapidjson::Value(isPublic), document.GetAllocator());

    rapidjson::StringBuffer buffer;
    rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
    document.Accept(writer);

    return std::string(buffer.GetString());
}

std::string GetProgressPayload(uint64_t current, uint64_t total)
{
    rapidjson::Document document;
    document.SetObject();

    document.AddMember(
        "progress", rapidjson::Value(current / static_cast<double>(total) * 100.0),
        document.GetAllocator());

    rapidjson::StringBuffer buffer;
    rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
    document.Accept(writer);

    return std::string(buffer.GetString());
}

UploadFailedPayload ParseUploadFailedMessage(const std::string& payloadText)
{
    rapidjson::StringStream stream(payloadText.c_str());
    rapidjson::Document document;

    document.ParseStream(stream);

    if (!document.IsObject()) {
        // This is unexpected, just return an empty object
        assert(document.IsObject());
        return {};
    }

    UploadFailedPayload payload;

    auto readInt = [&document](const char* name) {
        return document.HasMember(name) && document[name].IsInt()
               ? document[name].GetInt()
               : 0;
    };

    auto readString = [&document](const char* name) -> const char*
    {
        return document.HasMember(name) && document[name].IsString()
               ? document[name].GetString()
               : "";
    };

    payload.code = readInt("code");
    payload.status = readInt("status");

    payload.name = readString("name");
    payload.message = readString("message");

    if (document.HasMember("errors") && document["errors"].IsObject()) {
        for (auto& err : document["errors"].GetObject()) {
            if (!err.value.IsString()) {
                continue;
            }

            payload.additionalErrors.emplace_back(
                err.name.GetString(), err.value.GetString());
        }
    }

    return payload;
}

// This class will capture itself inside the request handlers
// by a strong reference. This way we ensure that it outlives all
// the outstanding requests.
struct AudiocomUploadOperation final : UploadOperation, std::enable_shared_from_this<UploadOperation>
{
    AudiocomUploadOperation(
        const ServiceConfig& serviceConfig, wxString fileName,
        wxString projectName, bool isPublic,
        UploadService::CompletedCallback completedCallback,
        UploadService::ProgressCallback progressCallback, AudiocomTrace trace)
        : mServiceConfig(serviceConfig)
        , mFileName(std::move(fileName))
        , mProjectName(std::move(projectName))
        , mIsPublic(isPublic)
        , mAudiocomTrace(trace)
        , mCompletedCallback(std::move(completedCallback))
        , mProgressCallback(std::move(progressCallback))
    {
    }

    const ServiceConfig& mServiceConfig;

    const wxString mFileName;
    const wxString mProjectName;

    const bool mIsPublic;

    const AudiocomTrace mAudiocomTrace;
    UploadService::CompletedCallback mCompletedCallback;
    UploadService::ProgressCallback mProgressCallback;

    std::string mAuthToken;

    std::string mSuccessUrl;
    std::string mFailureUrl;
    std::string mProgressUrl;

    std::string mAudioID;
    std::string mUploadToken;
    std::string mUserName;

    std::string mAudioSlug;

    using Clock = std::chrono::steady_clock;

    Clock::time_point mLastProgressReportTime;

    mutable std::mutex mStatusMutex;
    mutable std::mutex mCallbacksMutex;

    std::weak_ptr<audacity::network_manager::IResponse> mActiveResponse;
    bool mCompleted {};
    bool mAborted {};

    void SetRequiredHeaders(audacity::network_manager::Request& request) const
    {
        if (!mAuthToken.empty()) {
            request.setHeader(
                audacity::network_manager::common_headers::Authorization, std::string(mAuthToken));
        }

        const auto language = mServiceConfig.GetAcceptLanguageValue();

        if (!language.empty()) {
            request.setHeader(
                audacity::network_manager::common_headers::AcceptLanguage,
                language);
        }
    }

    void FailPromise(UploadOperationCompleted::Result result, std::string errorMessage)
    {
        {
            std::lock_guard<std::mutex> lock(mStatusMutex);
            mCompleted = true;
        }

        std::lock_guard<std::mutex> callbacksLock(mCallbacksMutex);

        if (mCompletedCallback) {
            mCompletedCallback(
                UploadOperationCompleted { result, ParseUploadFailedMessage(errorMessage) });
        }

        mProgressCallback = {};
        mCompletedCallback = {};
    }

    void CompletePromise()
    {
        {
            std::lock_guard<std::mutex> lock(mStatusMutex);
            mCompleted = true;
        }

        std::lock_guard<std::mutex> callbacksLock(mCallbacksMutex);

        if (mCompletedCallback) {
            const auto uploadURL = mAuthToken.empty()
                                   ? mServiceConfig.GetFinishUploadPage(
                mAudioID, mUploadToken, mAudiocomTrace)
                                   : mServiceConfig.GetAudioURL(
                mUserName, mAudioSlug, mAudiocomTrace);

            mCompletedCallback(
                { UploadOperationCompleted::Result::Success,
                  UploadSuccessfulPayload { mAudioID, mAudioSlug, mUploadToken, uploadURL } });
        }

        mProgressCallback = {};
        mCompletedCallback = {};
    }

    void InitiateUpload(std::string_view authToken)
    {
        using namespace audacity::network_manager;

        Request request(mServiceConfig.GetAPIUrl("/audio"));

        request.setHeader(
            common_headers::ContentType, common_content_types::ApplicationJson);

        request.setHeader(
            common_headers::Accept, common_content_types::ApplicationJson);

        mAuthToken = std::string(authToken);
        SetRequiredHeaders(request);

        const auto payload = GetUploadRequestPayload(mFileName, mProjectName, mIsPublic);

        std::lock_guard<std::mutex> lock(mStatusMutex);

        // User has already aborted? Do not send the request.
        if (mAborted) {
            return;
        }

        auto response = NetworkManager::GetInstance().doPost(
            request, payload.data(), payload.size());

        mActiveResponse = response;

        response->setRequestFinishedCallback(
            [response, sharedThis = shared_from_this(), this](auto) {
            auto responseCode = response->getHTTPCode();

            if (responseCode == 201) {
                HandleUploadPolicy(response->readAll<std::string>());
            } else if (responseCode == 401) {
                FailPromise(
                    UploadOperationCompleted::Result::Unauthorized,
                    response->readAll<std::string>());
            } else if (responseCode == 422) {
                FailPromise(
                    UploadOperationCompleted::Result::InvalidData,
                    response->readAll<std::string>());
            } else {
                FailPromise(
                    UploadOperationCompleted::Result::UnexpectedResponse,
                    response->readAll<std::string>());
            }
        });
    }

    void HandleUploadPolicy(std::string uploadPolicyJSON)
    {
        using namespace audacity::network_manager;

        rapidjson::Document document;
        document.Parse(uploadPolicyJSON.data(), uploadPolicyJSON.length());

        if (
            !document.HasMember("url") || !document.HasMember("success")
            || !document.HasMember("fail") || !document.HasMember("progress")) {
            FailPromise(
                UploadOperationCompleted::Result::UnexpectedResponse,
                uploadPolicyJSON);

            return;
        }

        auto form = std::make_unique<MultipartData>();

        if (document.HasMember("fields")) {
            const auto& fields = document["fields"];

            for (auto it = fields.MemberBegin(); it != fields.MemberEnd(); ++it) {
                form->Add(it->name.GetString(), it->value.GetString());
            }
        }

        const auto fileField
            =document.HasMember("field") ? document["field"].GetString() : "file";

        const wxFileName name { mFileName };

        try
        {
            // We have checked for the file existence on the main thread
            // already. For safety sake check for any exception thrown by AddFile
            // anyway
            form->AddFile(fileField, DeduceMimeType(name.GetExt()), name);
        }
        catch (...)
        {
            // Just fail the promise in case if any exception was thrown
            // UploadService user is responsible to display an appropriate dialog
            FailPromise(UploadOperationCompleted::Result::FileNotFound, {});
            return;
        }

        const auto url = document["url"].GetString();

        mSuccessUrl = document["success"].GetString();
        mFailureUrl = document["fail"].GetString();
        mProgressUrl = document["progress"].GetString();

        if (document.HasMember("extra")) {
            const auto& extra = document["extra"];

            mAudioID = extra["audio"]["id"].GetString();
            mAudioSlug = extra["audio"]["slug"].GetString();

            if (extra.HasMember("token")) {
                mUploadToken = extra["token"].GetString();
            }

            mUserName = extra["audio"]["username"].GetString();
        }

        const auto encType = document.HasMember("enctype")
                             ? document["enctype"].GetString()
                             : "multipart/form-data";

        Request request(url);

        request.setHeader(common_headers::ContentType, encType);
        request.setHeader(
            common_headers::Accept, common_content_types::ApplicationJson);

        // We only lock late and for very short time
        std::lock_guard<std::mutex> lock(mStatusMutex);

        if (mAborted) {
            return;
        }

        auto response
            =NetworkManager::GetInstance().doPost(request, std::move(form));

        mActiveResponse = response;

        response->setRequestFinishedCallback(
            [response, sharedThis = shared_from_this(), this](auto)
        {
            HandleS3UploadCompleted(response);
        });

        response->setUploadProgressCallback(
            [response, sharedThis = shared_from_this(),
             this](auto current, auto total)
        { HandleUploadProgress(current, total); });
    }

    void HandleUploadProgress(uint64_t current, uint64_t total)
    {
        {
            std::lock_guard<std::mutex> callbacksLock(mCallbacksMutex);

            if (mProgressCallback) {
                mProgressCallback(current, total);
            }
        }

        const auto now = Clock::now();

        if ((now - mLastProgressReportTime) > mServiceConfig.GetProgressCallbackTimeout()) {
            mLastProgressReportTime = now;

            using namespace audacity::network_manager;
            Request request(mProgressUrl);

            request.setHeader(
                common_headers::ContentType, common_content_types::ApplicationJson);
            request.setHeader(
                common_headers::Accept, common_content_types::ApplicationJson);

            auto payload = GetProgressPayload(current, total);

            std::lock_guard<std::mutex> lock(mStatusMutex);

            if (mAborted) {
                return;
            }

            auto response = NetworkManager::GetInstance().doPatch(
                request, payload.data(), payload.size());

            response->setRequestFinishedCallback([response](auto) {});
        }
    }

    void HandleS3UploadCompleted(std::shared_ptr<audacity::network_manager::IResponse> response)
    {
        using namespace audacity::network_manager;

        const auto responseCode = response->getHTTPCode();

        const bool success
            =responseCode == 200 || responseCode == 201 || responseCode == 204;

        Request request(success ? mSuccessUrl : mFailureUrl);
        SetRequiredHeaders(request);

        std::lock_guard<std::mutex> lock(mStatusMutex);

        if (mAborted) {
            return;
        }

        auto finalResponse = success ? NetworkManager::GetInstance().doPost(request, nullptr, 0)
                             : NetworkManager::GetInstance().doDelete(request);

        mActiveResponse = finalResponse;

        finalResponse->setRequestFinishedCallback(
            [finalResponse, sharedThis = shared_from_this(), this, success](auto)
        {
            const auto httpCode = finalResponse->getHTTPCode();
            if (success && httpCode >= 200 && httpCode < 300) {
                CompletePromise();
                return;
            }

            FailPromise(
                UploadOperationCompleted::Result::UploadFailed,
                finalResponse->readAll<std::string>());
        });
    }

    bool IsCompleted() override
    {
        std::lock_guard<std::mutex> lock(mStatusMutex);
        return mCompleted;
    }

    void Abort() override
    {
        {
            std::lock_guard<std::mutex> lock(mStatusMutex);

            if (mCompleted) {
                return;
            }

            mCompleted = true;
            mAborted = true;

            if (auto activeResponse = mActiveResponse.lock()) {
                activeResponse->abort();
            }
        }

        std::lock_guard<std::mutex> callbacksLock(mCallbacksMutex);

        if (mCompletedCallback) {
            mCompletedCallback({ UploadOperationCompleted::Result::Aborted });
        }

        mCompletedCallback = {};
        mProgressCallback = {};
    }

    void DiscardResult() override
    {
        using namespace audacity::network_manager;

        Abort();

        auto url = mServiceConfig.GetAPIUrl("/audio");
        url += "/" + mAudioID + "?token=" + mUploadToken;

        Request request(url);
        auto response = NetworkManager::GetInstance().doDelete(request);

        response->setRequestFinishedCallback(
            [response](auto)
        {
            // Do nothing
        });
    }
}; // struct UploadOperation
} // namespace

UploadService::UploadService(const ServiceConfig& config, OAuthService& service)
    : mServiceConfig(config), mOAuthService(service)
{
}

UploadOperationHandle UploadService::Upload(
    const wxString& fileName, const wxString& projectName, bool isPublic,
    CompletedCallback completedCallback, ProgressCallback progressCallback,
    AudiocomTrace trace)
{
    if (!wxFileExists(fileName)) {
        if (completedCallback) {
            completedCallback(UploadOperationCompleted {
                    UploadOperationCompleted::Result::FileNotFound });
        }

        return {};
    }

    auto operation = std::make_shared<AudiocomUploadOperation>(
        mServiceConfig, fileName, projectName, isPublic,
        std::move(completedCallback), std::move(progressCallback), trace);

    mOAuthService.ValidateAuth(
        [operation, this](std::string_view authToken) {
        operation->InitiateUpload(authToken);
    },
        trace, false);

    return UploadOperationHandle { operation };
}

UploadOperation::~UploadOperation() = default;

UploadOperationHandle::UploadOperationHandle(
    std::shared_ptr<UploadOperation> operation)
    : mOperation(std::move(operation))
{
}

UploadOperationHandle::~UploadOperationHandle()
{
    if (mOperation) {
        // It is safe to call Abort on completed operations
        mOperation->Abort();
    }
}

UploadOperationHandle::operator bool() const noexcept
{
    return mOperation != nullptr;
}

UploadOperation* UploadOperationHandle::operator->() const noexcept
{
    return mOperation.operator->();
}

wxString GetUploadTempPath()
{
    const auto tempPath = TempDirectory::DefaultTempDir();

    if (!wxDirExists(tempPath)) {
        // Temp directory was not created yet.
        // Is it a first run of Audacity?
        // In any case, let's wait for some better time
        return {};
    }

    if (!FileNames::WritableLocationCheck(
            tempPath, XO("Cannot proceed to upload."))) {
        return {}
    }

    return tempPath + "/cloud/";
}

namespace {
const auto tempChangedSubscription = TempDirectory::GetTempPathObserver().Subscribe([](const auto&) {
    const auto tempPath = GetUploadTempPath();

    if (!wxDirExists(tempPath)) {
        return;
    }

    wxArrayString files;

    wxDir::GetAllFiles(tempPath, &files, {}, wxDIR_FILES);

    for (const auto& file : files) {
        wxRemoveFile(file);
    }

    return;
});
}
} // namespace audacity::cloud::audiocom
