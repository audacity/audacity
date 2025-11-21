/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  UploadService.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstdint>
#include <functional>
#include <memory>
#include <utility>
#include <variant>
#include <vector>

#include <wx/string.h>

enum class AudiocomTrace;

namespace audacity::cloud::audiocom {
class ServiceConfig;
class OAuthService;

//! This structure represents an upload error as returned by the server.
struct CLOUD_AUDIOCOM_API UploadFailedPayload final
{
    int32_t code {};
    int32_t status {};

    std::string name;
    std::string message;

    using AdditionalError = std::pair<std::string, std::string>;
    std::vector<AdditionalError> additionalErrors;
};

//! This structure represents the payload associated with successful upload
struct CLOUD_AUDIOCOM_API UploadSuccessfulPayload final
{
    //! ID of the uploaded audio
    std::string audioId;
    //! "Slug" to be used for shareable URL construction
    std::string audioSlug;
    //! Upload token, if any
    std::string uploadToken;
    //! URL to the uploaded audio
    std::string audioUrl;
};

//! Message that is sent when upload is finished.
//! This message is sent from the network thread.
struct CLOUD_AUDIOCOM_API UploadOperationCompleted final
{
    //! Result of the upload
    enum class Result
    {
        //! Upload was successful
        Success,
        //! Upload was aborted by the user
        Aborted,
        //! Specified file is not found
        FileNotFound,
        //! Authorization is required
        Unauthorized,
        //! audio.com has failed to understand what Audacity wants
        InvalidData,
        //! Audacity has failed to understand audio.com response
        UnexpectedResponse,
        //! Upload failed for some other reason
        UploadFailed
    };

    //! Upload result
    Result result;

    using Payload = std::variant<
        std::monostate,
        UploadSuccessfulPayload,
        UploadFailedPayload
        >;

    //! Operation payload
    Payload payload;
};

//! Class used to track the upload operation
class CLOUD_AUDIOCOM_API UploadOperation
{
public:
    virtual ~UploadOperation();

    //! Returns true if the upload is finished
    virtual bool IsCompleted() = 0;
    //! Abort the upload, if running
    virtual void Abort() = 0;
    //! Abort the upload, if running,
    //! notify audio.com that the uploaded file is no longer needed otherwise
    virtual void DiscardResult() = 0;
};

//! A unique_ptr like class that holds a pointer to UploadOperation
class CLOUD_AUDIOCOM_API UploadOperationHandle final
{
public:
    UploadOperationHandle() = default;
    explicit UploadOperationHandle(std::shared_ptr<UploadOperation> operation);

    UploadOperationHandle(const UploadOperationHandle&) = delete;
    UploadOperationHandle(UploadOperationHandle&&) = default;
    UploadOperationHandle& operator=(const UploadOperationHandle&) = delete;
    UploadOperationHandle& operator=(UploadOperationHandle&&) = default;

    ~UploadOperationHandle();

    explicit operator bool() const noexcept;

    UploadOperation* operator->() const noexcept;

private:
    std::shared_ptr<UploadOperation> mOperation {};
};

//! Service, responsible for uploading audio files to audio.com
class CLOUD_AUDIOCOM_API UploadService final
{
public:
    UploadService(const ServiceConfig& config, OAuthService& service);

    using CompletedCallback
        =std::function<void (const UploadOperationCompleted&)>;

    using ProgressCallback = std::function<void (uint64_t current, uint64_t total)>;

    //! Uploads the file to audio.com
    /*
       Both callbacks are invoked from the network thread.

       If projectName is empty, the name for the uploaded file is deduced from the fileName.
    */
    UploadOperationHandle Upload(
        const wxString& fileName, const wxString& projectName, bool isPublic, CompletedCallback completedCallback,
        ProgressCallback progressCallback, AudiocomTrace);

private:
    const ServiceConfig& mServiceConfig;
    OAuthService& mOAuthService;
};

CLOUD_AUDIOCOM_API wxString GetUploadTempPath();
} // namespace audacity::cloud::audiocom
