/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file SentryReport.cpp
 @brief Define a class to report errors to Sentry.

 Dmitry Vedenko
 **********************************************************************/

#include "SentryReport.h"

#include <chrono>
#include <cstring>
#include <mutex>

#include <algorithm>
#include <cctype>
#include <regex>

#include <rapidjson/document.h>
#include <rapidjson/writer.h>
#include <rapidjson/prettywriter.h>

#include <wx/platinfo.h>
#include <wx/log.h>

#include "CodeConversions.h"
#include "Uuid.h"

#include "IResponse.h"
#include "NetworkManager.h"

#include "SentryRequestBuilder.h"

namespace audacity {
namespace sentry {
namespace {
//! Helper class to store additional details about the exception
/*! This small class is a thread safe store for the information
    we want to add to the exception before the exception occurs.
    For example, we may log SQLite3 return codes here, as otherwise
    they wont be available when everything fails
*/
class ExceptionContext final
{
public:
    //! Adds a new item to the exception context
    void Add(std::string parameterName, AnonymizedMessage parameterValue)
    {
        std::lock_guard<std::mutex> lock(mDataMutex);
        mData.emplace_back(std::move(parameterName), std::move(parameterValue));
    }

    //! Return the current context and reset it
    std::vector<ExceptionData> MoveParameters()
    {
        std::lock_guard<std::mutex> lock(mDataMutex);

        std::vector<ExceptionData> emptyVector;

        std::swap(mData, emptyVector);

        return emptyVector;
    }

    //! Get an instance of the ExceptionContext
    static ExceptionContext& Get()
    {
        static ExceptionContext instance;
        return instance;
    }

private:
    ExceptionContext() = default;

    std::mutex mDataMutex;
    std::vector<ExceptionData> mData;
};

//! Append the data about the operating system to the JSON document
void AddOSContext(
    rapidjson::Value& root, rapidjson::Document::AllocatorType& allocator)
{
    rapidjson::Value osContext(rapidjson::kObjectType);

    const wxPlatformInfo platformInfo = wxPlatformInfo::Get();

    const std::string osName
        =ToUTF8(platformInfo.GetOperatingSystemFamilyName());

    osContext.AddMember("type", rapidjson::Value("os", allocator), allocator);

    osContext.AddMember(
        "name", rapidjson::Value(osName.c_str(), osName.length(), allocator),
        allocator);

    const std::string osVersion
        =std::to_string(platformInfo.GetOSMajorVersion()) + "."
          + std::to_string(platformInfo.GetOSMinorVersion()) + "."
          + std::to_string(platformInfo.GetOSMicroVersion());

    osContext.AddMember(
        "version",
        rapidjson::Value(osVersion.c_str(), osVersion.length(), allocator),
        allocator);

    root.AddMember("os", std::move(osContext), allocator);
}

//! Create the minimal required Sentry JSON document
rapidjson::Document CreateSentryDocument()
{
    using namespace std::chrono;
    rapidjson::Document document;

    document.SetObject();

    document.AddMember(
        "timestamp",
        rapidjson::Value(
            duration_cast<seconds>(system_clock::now().time_since_epoch())
            .count()),
        document.GetAllocator());

    std::string eventId = Uuid::Generate().ToHexString();

    document.AddMember(
        "event_id",
        rapidjson::Value(
            eventId.c_str(), eventId.length(), document.GetAllocator()),
        document.GetAllocator());

    constexpr char platform[] = "native";

    document.AddMember(
        "platform",
        rapidjson::Value(platform, sizeof(platform) - 1, document.GetAllocator()),
        document.GetAllocator());

    document["platform"].SetString(
        platform, sizeof(platform) - 1, document.GetAllocator());

    const std::string release = std::string("audacity@")
                                + std::to_string(AUDACITY_VERSION) + "."
                                + std::to_string(AUDACITY_RELEASE) + "."
                                + std::to_string(AUDACITY_REVISION);

    document.AddMember(
        "release",
        rapidjson::Value(
            release.c_str(), release.length(), document.GetAllocator()),
        document.GetAllocator());

    rapidjson::Value contexts = rapidjson::Value(rapidjson::kObjectType);

    AddOSContext(contexts, document.GetAllocator());

    document.AddMember("contexts", contexts, document.GetAllocator());

    return document;
}

//! Append the ExceptionData to the Exception JSON object
void AddExceptionDataToJson(
    rapidjson::Value& value, rapidjson::Document::AllocatorType& allocator,
    const ExceptionData& data)
{
    value.AddMember(
        rapidjson::Value(data.first.c_str(), data.first.length(), allocator),
        rapidjson::Value(data.second.c_str(), data.second.length(), allocator),
        allocator);
}

//! Serialize the Exception to JSON
void SerializeException(
    const Exception& exception, rapidjson::Value& root,
    rapidjson::Document::AllocatorType& allocator)
{
    root.AddMember(
        "type",
        rapidjson::Value(
            exception.Type.c_str(), exception.Type.length(), allocator),
        allocator);

    root.AddMember(
        "value",
        rapidjson::Value(
            exception.Value.c_str(), exception.Value.length(), allocator),
        allocator);

    rapidjson::Value mechanismObject(rapidjson::kObjectType);

    mechanismObject.AddMember(
        "type", rapidjson::Value("runtime_error", allocator), allocator);

    mechanismObject.AddMember(
        "handled", false, allocator);

    auto contextData = ExceptionContext::Get().MoveParameters();

    if (!exception.Data.empty() || !contextData.empty()) {
        rapidjson::Value dataObject(rapidjson::kObjectType);

        for (const auto& data : contextData) {
            AddExceptionDataToJson(dataObject, allocator, data);
        }

        for (const auto& data : exception.Data) {
            AddExceptionDataToJson(dataObject, allocator, data);
        }

        mechanismObject.AddMember("data", std::move(dataObject), allocator);
    }

    root.AddMember("mechanism", std::move(mechanismObject), allocator);
}
} // namespace

Exception Exception::Create(std::string type, AnonymizedMessage value)
{
    std::replace_if(type.begin(), type.end(), [](char c) {
        return std::isspace(c) != 0;
    }, '_');

    return { std::move(type), std::move(value) };
}

Exception Exception::Create(AnonymizedMessage value)
{
    return { "runtime_error", std::move(value) };
}

Exception& Exception::AddData(std::string key, AnonymizedMessage value)
{
    Data.emplace_back(std::move(key), std::move(value));
    return *this;
}

Message Message::Create(AnonymizedMessage message)
{
    return { std::move(message) };
}

Message& Message::AddParam(AnonymizedMessage value)
{
    Params.emplace_back(std::move(value));
    return *this;
}

void AddExceptionContext(
    std::string parameterName, AnonymizedMessage parameterValue)
{
    ExceptionContext::Get().Add(std::move(parameterName), std::move(parameterValue));
}

class Report::ReportImpl
{
public:
    explicit ReportImpl(const Exception& exception);
    explicit ReportImpl(const Message& message);

    void AddUserComment(const std::string& message);

    std::string ToString(bool pretty) const;

    void Send(CompletionHandler completionHandler) const;

private:
    rapidjson::Document mDocument;
};

Report::ReportImpl::ReportImpl(const Exception& exception)
    : mDocument(CreateSentryDocument())
{
    rapidjson::Value exceptionObject(rapidjson::kObjectType);
    rapidjson::Value valuesArray(rapidjson::kArrayType);
    rapidjson::Value valueObject(rapidjson::kObjectType);

    SerializeException(exception, valueObject, mDocument.GetAllocator());

    valuesArray.PushBack(std::move(valueObject), mDocument.GetAllocator());

    exceptionObject.AddMember(
        "values", std::move(valuesArray), mDocument.GetAllocator());

    mDocument.AddMember(
        "exception", std::move(exceptionObject), mDocument.GetAllocator());
}

Report::ReportImpl::ReportImpl(const Message& message)
    : mDocument(CreateSentryDocument())
{
    rapidjson::Value messageObject(rapidjson::kObjectType);

    messageObject.AddMember(
        "message",
        rapidjson::Value(
            message.Value.c_str(), message.Value.length(),
            mDocument.GetAllocator()),
        mDocument.GetAllocator());

    if (!message.Params.empty()) {
        rapidjson::Value paramsArray(rapidjson::kArrayType);

        for (const AnonymizedMessage& param : message.Params) {
            paramsArray.PushBack(
                rapidjson::Value(
                    param.c_str(), param.length(), mDocument.GetAllocator()),
                mDocument.GetAllocator());
        }

        messageObject.AddMember(
            "params", std::move(paramsArray), mDocument.GetAllocator());
    }

    mDocument.AddMember(
        "message", std::move(messageObject), mDocument.GetAllocator());
}

void Report::ReportImpl::AddUserComment(const std::string& message)
{
    // We only allow adding comment to exceptions now
    if (!mDocument.HasMember("exception") || message.empty()) {
        return;
    }

    rapidjson::Value& topException = mDocument["exception"]["values"][0];

    if (!topException.IsObject()) {
        return;
    }

    rapidjson::Value& mechanism = topException["mechanism"];

    // Create a data object if it still does not exist
    if (!mechanism.HasMember("data")) {
        mechanism.AddMember(
            "data", rapidjson::Value(rapidjson::kObjectType),
            mDocument.GetAllocator());
    }

    // Add a comment itself
    mechanism["data"].AddMember(
        "user_comment",
        rapidjson::Value(
            message.data(), message.length(), mDocument.GetAllocator()),
        mDocument.GetAllocator());
}

void Report::ReportImpl::Send(CompletionHandler completionHandler) const
{
    const std::string serializedDocument = ToString(false);

    network_manager::Request request
        =SentryRequestBuilder::Get().CreateRequest();

    auto response = network_manager::NetworkManager::GetInstance().doPost(
        request, serializedDocument.data(), serializedDocument.size());

    response->setRequestFinishedCallback(
        [response, handler = std::move(completionHandler)](network_manager::IResponse*) {
        const std::string responseData = response->readAll<std::string>();

        wxLogDebug(responseData.c_str());

        if (handler) {
            handler(response->getHTTPCode(), responseData);
        }
    });
}

std::string Report::ReportImpl::ToString(bool pretty) const
{
    rapidjson::StringBuffer buffer;

    if (pretty) {
        rapidjson::PrettyWriter<rapidjson::StringBuffer> writer(buffer);
        mDocument.Accept(writer);
    } else {
        rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
        mDocument.Accept(writer);
    }

    return std::string(buffer.GetString());
}

Report::~Report()
{
}

Report::Report(const Exception& exception)
    : mImpl(std::make_unique<ReportImpl>(exception))
{
}

Report::Report(const Message& message)
    : mImpl(std::make_unique<ReportImpl>(message))
{
}

void Report::AddUserComment(const std::string& comment)
{
    mImpl->AddUserComment(comment);
}

std::string Report::GetReportPreview() const
{
    return mImpl->ToString(true);
}

void Report::Send(CompletionHandler completionHandler) const
{
    mImpl->Send(std::move(completionHandler));
}
} // namespace sentry
} // namespace audacity
